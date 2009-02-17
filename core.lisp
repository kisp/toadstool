(in-package #:toadstool-system)

(defvar *trace* '())
(defvar *used-components*)
(defvar *inner-forms* nil)
(defvar *outer-form* nil)
(defvar *debug-nesting-level* 0)
(defvar *end-nestings* nil)
(defvar *toplevel-syms*)
(defvar *toplevel-patterns*)

(defclass component ()
  ())
(defclass form (component)
  ((form :reader form-of
         :initarg form)
   (inner-forms :reader inner-forms-of
                :initarg inner-forms)
   (outer-form :reader outer-form-of
               :initarg outer-form))
  (:documentation "Superclass of all components being a part of a pattern."))
(defclass operator (form) ())
(defclass component-mixin () ())
(defclass nesting (component)
  ((if-expr  :initarg :if-expr
             :reader if-expr-of
             :initform (error "No if-expr"))
   (else-expr :initarg :else-expr
              :reader else-expr-of
              :initform (error "No else-expr"))))
(defclass macro-mixin ()
  (expansion))
(defclass sequence-mixin () ())
(defclass ignored-expr-mixin () ())
(defclass destructuring-mixin (sequence-mixin) ())

(defun extract-prefix (sym suffix)
  (let* ((str (string sym))
         (suffix (string suffix))
         (diff (- (length str) (length suffix))))
    (when (and (> (length str) (length suffix))
               (string= suffix str :start2 diff))
      (intern (subseq str 0 diff)
              (symbol-package sym)))))

(defgeneric name-of (component)
  (:method ((c operator))
    (or (extract-prefix #1=(class-name (class-of c)) '-form)
        #1#)))

(defgeneric ignored-expr? (form)
  (:documentation "If T is returned, expression isn't rebound to a gensym.
Useful for forms like T and destructuring operators to avoid macroexpansion clutter.")
  (:method (form)
    nil)
  (:method ((c macro-mixin))
    t)
  (:method ((c destructuring-mixin))
    t))

(defmacro k (&body body)
  `(lambda () . ,body))

(defmacro with-end-nesting (((if-expr else-expr k)
                             &body nesting-body)
                            &body body)
  `(let ((*end-nestings* (cons (lambda (,if-expr ,else-expr ,k)
                                 . ,nesting-body)
                               *end-nestings*)))
     ,@body))

(defmacro k-once (k &body body)
  (assert (= 1 (length body)))
  (let ((k (if (symbolp k)
               k
               (cadr k)))
        (var (if (symbolp k)
                 k
                 (car k)))
        (sym (gensym)))
    ``(let ((,',sym (lambda () ,(funcall ,k))))
        ,(let ((,var (lambda () `(funcall ,',sym))))
           ,(car body)))))

(defgeneric coerce-to-obj (datum)
  (:method ((c component)) c)
  (:method ((c symbol)) (coerce-to-obj (find-class c)))
  (:method ((c class))
    (when (not (closer-mop:class-finalized-p c))
      (closer-mop:finalize-inheritance c))
    (closer-mop:class-prototype c)))

(defgeneric matches? (component datum)
  (:documentation "Decide whether component can operate on given value")
  (:method ((c component) datum)
    nil)
  (:method ((o operator) datum)
    (let ((name (name-of o)))
      (and (consp datum)
           (if (consp (car datum))
               (and (eq (caar datum) name)
                    (or (cdar datum) t))
               (eq (car datum) name))))))

(defgeneric expand-form (form expression k)
  (:documentation "Generate an expansion of a pattern element")
  (:method :around ((f form) expression k)
    (if (ignored-expr? f)
        (call-next-method)
        (let ((expr-name (if (gensym? expression)
                             expression
                             (gensym))))
          (push (cons f expr-name) *trace*)
          (if (gensym? expression)
              (call-next-method)
              `(let ((,expr-name ,expression))
                 (declare (ignorable ,expr-name))
                 ,(call-next-method f expr-name k)))))))

(defmacro define-predicate (type)
  (let ((pred (intern (format nil "~A?" type))))
               `(defun ,pred (x)
                  (typep x ',type))))

(define-predicate component)
(define-predicate form)
(define-predicate operator)
(define-predicate nesting)
(define-predicate sequence-mixin)

(defgeneric expand-nesting (obj k))

(defmethod initialize-instance :around ((form form) &key)
  (when (boundp '*inner-forms*) 
    (push form *inner-forms*)) 
  (let ((*outer-form* form)
        (*inner-forms* nil))
    (prog1 (call-next-method)
      (setf (slot-value form 'inner-forms)
            (nreverse *inner-forms*)))))

(defun %mkform (class datum &optional args) 
  (apply #'make-instance class
         'outer-form *outer-form*
         'form datum
         args))

(defun mkform (datum)
  "Instantiate a form basing on DATUM with component subtype TYPE."
  (loop for i in (components 'form)
        for j = (coerce-to-obj i)
        for ret = (matches? j datum)
        do (check-type ret (or boolean cons))
        when ret
          do (return (%mkform i datum (and (consp ret) ret)))
        finally (error "Datum ~S doesn't match any form" datum)))

(defgeneric sequence-initial-state (sequence-mixin expr)
  (:documentation "Initial state"))

(defgeneric sequence-cdr-state (sequence-mixin state)
  (:documentation "Next state"))

(defgeneric sequence-endp (sequence-mixin state)
  (:documentation "Check whether state is over the end of the sequence"))

(defgeneric sequence-item (sequence-mixin state)
  (:documentation "Extract the value from the current sequence state"))

(defgeneric sequence-set-state (sequence-mixin state))

(defgeneric sequence-get-state (sequence-mixin))

(defun find-form-expr (form)
  (cdr (or (find form *trace* :key #'car)
           (error "No such form: ~S" form))))

(defun find-sequence-form (f)
  (loop for form = (outer-form-of f) then (outer-form-of form)
        while form
        when (sequence-mixin? form)
          do (return (values form (find-form-expr form)))
        finally (return (error "There's no SEQUENCE-MIXIN in trace ~S" f))))

(defvar +nesting-keyword-list+
  '(:if-expr :else-expr))

(defun call/nestings (nestings if-expr else-expr k)
  (if (endp nestings)
      (funcall k if-expr else-expr)
      (expand-nesting (apply #'make-instance
                             (car nestings)
                             (mappend #'list +nesting-keyword-list+
                                      (list if-expr else-expr)))
                      (lambda (&rest args)
                        (apply #'call/nestings
                               (cdr nestings)
                               (append args (list k)))))))

(defun call/end-nestings (nestings if-expr else-expr k)
  (if (endp nestings)
      (funcall k if-expr else-expr)
      (funcall (car nestings) if-expr else-expr
               (lambda (if-expr else-expr)
                 (call/end-nestings (cdr nestings) if-expr else-expr k)))))

(defmacro with-root-mixins (&body body &aux (c '(components 'component-mixin)))
  `(progn (closer-mop:ensure-class 'component :direct-superclasses ,c)
          (unwind-protect (progn . ,body)
            (closer-mop:ensure-class 'component :direct-superclasses nil))))

(defun toplevel-expansion (block-name patterns exprs
                           guard if-expr else-expr)
  (assert (= (length patterns)
             (length exprs)) nil
             "There must be as many patterns as expressions. ~
Got patterns: ~S, expressions: ~S"
             patterns exprs)
  (assert (/= 0 (length patterns)) nil "There must be at least one pattern")
  (assert (every #'gensym? exprs))
  (let* ((forms (mapcar #'mkform patterns))
         (*toplevel-patterns* forms) 
         (*trace* nil)
         (*toplevel-syms* exprs))
    (with-root-mixins
      (labels ((aux (if-expr else-expr)
                 (rec aux ((exprs exprs)
                           (forms forms))
                   (expand-form (car forms)
                                (car exprs)
                                (if (cdr forms)
                                    (k
                                      (aux (cdr exprs)
                                           (cdr forms)))
                                    (k
                                      (call/end-nestings *end-nestings*
                                                         `(when ,guard
                                                            ,if-expr)
                                                         else-expr
                                                         #'aux2))))))
               (aux2 (if-expr else-expr2)
                 (setq else-expr else-expr2)
                 if-expr))
        `(progn ,(call/nestings (components 'nesting) 
                                `(return-from ,block-name
                                   ,if-expr)
                                else-expr
                                #'aux)
                ,else-expr)))))

(defgeneric form-arguments (component)
  (:method ((c form))
    (list (form-of c)))
  (:method ((c operator))
    (cdr (form-of c))))

(defmacro definit (class dlist &body body)
  `(defmethod initialize-instance :after ((,class ,class) &key)
     (destructuring-bind ,dlist (form-arguments ,class)
       (apply #'reinitialize-instance ,class (progn ,@body)))))

(defun remove-from-plist (plist to-remove)
  (loop for (k v) on plist by #'cddr
        unless (member k to-remove)
          collect k and collect v))

(defmacro thunk (form)
  `(lambda () ,form))

(defun fmt (control &rest stuff)
  (apply #'format nil control stuff))

(defun mklist (x)
  (if (listp x)
      x
      (list x)))

(defun partition (fn list &key (key #'identity))
  (loop for i in list
        if (funcall fn (funcall key i))
          collect i into a
        else
          collect i into b
        finally (return (values a b))))

(defmacro defcomponent (name supers &body options)
  (flet ((foo (key list default)
           (let ((foo (getf list key '#1=#:foo)))
             (cond ((null foo) '())
                   ((eq foo '#1#) (list key (funcall default)))
                   (t (list key foo))))))
    (multiple-value-bind (options slots)
        (partition (lambda (x)
                     (and (consp x)
                          (keywordp (car x))))
                   options)
      `(defclass ,name ,supers
         ,(loop
            for s in slots
            collect (let* ((s (mklist s))
                           (name (pop s))
                           (str (string name))
                           (of (string '#:-of))
                           (req (if (getf s :required)
                                    `(:initform (error "Missing initarg :~S"
                                                       str))))
                           (keyword (find-package '#:keyword)))
                      (append (list name)
                              (foo :initarg s (thunk (intern str keyword)))
                              (foo :reader s (thunk (intern
                                                     (fmt "~A~A" str of))))
                              req
                              (remove-from-plist
                               s '(:reader :initarg :required)))))
         ,@options))))

(defmacro equality (x y)
  `(eql ,x ,y))

#+nil
(defmethod print-object ((c form) s)
  (if (slot-boundp c 'form)
      (print-unreadable-object (c s :type t :identity nil)
        (format s "~{~S~^ ~}" (form-arguments c)))
      (call-next-method)))

(defmacro if-matches (test k)
  ``(if ,,test ,(funcall ,k)))

(defun effective-inner-forms-of (c)
  (rec aux ((xs (inner-forms-of c))
            (tail nil)) 
    (if (endp xs)
        tail
        (aux (cdr xs)
             (append (aux (inner-forms-of (car xs))
                          (list (car xs)))
                     tail)))))
(defun components (type)
  (loop for i in *used-components*
        when (subtypep i type)
          collect i))

(defmethod expand-form ((c macro-mixin) expr k)
  (expand-form (slot-value c 'expansion) expr k))

(defmacro defexpand (name dlist &body body)
  `(defmethod initialize-instance :after ((,name ,name) &key)
     (destructuring-bind ,dlist (form-arguments ,name)
       (setf (slot-value ,name 'expansion)
             (mkform (progn . ,body))))))

(defun debug-print (datum expr)
  (let ((*print-level* 4))
    (format *debug-io* "~A~S = ~S~%" (make-string *debug-nesting-level*
                                                 :initial-element #\Space)
            datum expr)))

(defvar *debug-nesting-level* 0)
(defclass debug-mixin (component-mixin) ())
(defmethod expand-form :around ((c debug-mixin) expr k)
  (with-gensyms (block-name)
    `(block ,block-name
       (let ((*debug-nesting-level* (1+ *debug-nesting-level*)))
         (debug-print ',(form-of c) ,expr)
         ,(call-next-method c expr k)))))

(defun mapc/forms (fn)
  (rec aux ((xs *toplevel-patterns*)) 
    (if (null xs)
        nil
        (progn (funcall fn (car xs))
               (aux (inner-forms-of (car xs)))
               (aux (cdr xs))))))

(defun gensym? (x)
  (and (symbolp x)
       (null (symbol-package x))))
