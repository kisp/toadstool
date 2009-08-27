(in-package #:toadstool-impl)

(defvar *default-components*
  '(satisfies-form typep-form variable-form not-form quote-form list*-form
    literal-form +-form t-form and-form list-form *-form cons-form or-form
    variable-nesting vector-form vector-rest-form push-form push-nesting
    with-accessors-form >-form >=-form <-form <=-form class-form eql-form
    equal-form assoc-form))

(deftype constant-value ()
  '(or null keyword (not (or symbol cons))))


;;; literal-form

(defcomponent literal-form (form)
  datum)

(defmethod matches? ((c literal-form) datum)
  (typep datum 'constant-value))

(defmethod expand-form ((c literal-form) expr k)
  (if-matches `(equality ',(datum-of c) ,expr)
              k))

(definit literal-form (datum)
  `(:datum ,datum))


;;; typep-form

(defclass typep-form (operator)
  ((type :initarg :type :reader type-of*)))

(defcomponent typep-form (operator)
  (type :reader type-of*))

(definit typep-form (type)
  `(:type ,type))

(defmethod expand-form ((c typep-form) expr k)
  (if-matches `(typep ,expr ,(type-of* c))
              k))


;;; satisfies-form

(defcomponent satisfies-form (operator)
  function-expr)

(definit satisfies-form (function-expr)
  `(:function-expr ,function-expr))

(defmethod expand-form ((c satisfies-form) expr k)
  (if-matches `(funcall ,(function-expr-of c) ,expr)
              k))


;;; not-form

(defcomponent not-form (operator)
  datum)

(definit not-form (datum)
  `(:datum ,(mkform datum)))

(defmethod expand-form ((c not-form) expr k)
  (with-gensyms (block-name)
    `(block ,block-name
       ,(expand-form (datum-of c) expr (k `(return-from ,block-name)))
       ,(funcall k))))


;;; and-form

(defcomponent and-form (operator)
  forms)

(definit and-form (&rest forms)
  `(:forms ,(mapcar #'mkform forms)))

;;; XXX (toad-case '(1 2 3 4)
;;;             (list (and (* a)
;;;                        (* (satisfies #'print)))) -> a)
;;; BUG returns true

(defmethod expand-form ((c and-form) expr k)
  (rec aux ((xs (forms-of c)))
    (if (null xs)
        (funcall k)
        (expand-form (car xs)
                     expr
                     (k (aux (cdr xs)))))))


;;; or-form

(defcomponent or-form (operator)
  forms)

(definit or-form (&rest forms)
  `(:forms ,(mapcar #'mkform forms)))

(defmethod expand-form ((c or-form) expr k)
  (k-once k
    `(progn ,@(loop for i in (forms-of c)
                    collect (expand-form i expr k)))))


;;; quote-form

(defcomponent quote-form (operator)
  datum)

(definit quote-form (datum)
  `(:datum ,datum))

(defmethod expand-form ((c quote-form) expr k)
  (if-matches `(equality ',(datum-of c) ,expr)
              k))


;;; cons-form

(defcomponent cons-form (operator sequence-mixin)
  car
  cdr
  (cdr-state :initform (gensym)))

(definit cons-form (car cdr)
  `(:car ,(mkform car) :cdr ,(mkform cdr)))

(defmethod expand-form ((c cons-form) expr k)
  `(when (,(if (typep (car-of c) 'destructuring-mixin)
               'listp
               'consp)
           ,expr)
     (let ((,#1=(cdr-state-of c) (cdr ,expr)))
       (declare (ignorable ,#1#))
       ,(expand-form (car-of c)
                     `(car ,expr)
                     (k (expand-form (cdr-of c) (cdr-state-of c) k))))))

(defmethod sequence-initial-state ((f cons-form) expr)
  expr)

(defmethod sequence-cdr-state ((f cons-form) state)
  `(cdr ,state))

(defmethod sequence-endp ((f cons-form) state)
  `(null ,state))

(defmethod sequence-item ((f cons-form) state)
  `(car ,state))

(defmethod sequence-set-state ((f cons-form) state)
  `(setq ,(cdr-state-of f) ,state))

(defmethod sequence-get-state ((f cons-form))
  (cdr-state-of f))


;;; list*-form

(defcomponent list*-form (operator macro-mixin))

(defexpand list*-form (&rest elts)
  (let ((forms (butlast elts))
        (last-form (car (last elts))))
    (rec aux ((xs forms))
      (if (null xs)
          last-form
          `(cons ,(car xs)
                 ,(aux (cdr xs)))))))


;;; list-form

(defcomponent list-form (operator macro-mixin))

(defexpand list-form (&rest elts)
  `(list* . ,(append elts '(nil))))


;;; destructuring

(defun destructure-form (seq forms k)
  (assert (not (endp forms)))
  (with-gensyms (state-name)
    `(let ((,state-name ,(sequence-get-state seq)))
       ,(rec aux ((forms forms))
          (if (endp forms)
              (funcall k)
              `(unless ,(sequence-endp seq state-name) 
                 ,(expand-form (car forms)
                               (sequence-item seq (sequence-get-state seq))
                               (k `(progn
                                     ,@(unless (typep (car forms)
                                                      'destructuring-mixin)
                                              
                                         `(,(sequence-set-state
                                             seq
                                             (sequence-cdr-state
                                              seq (sequence-get-state seq)))))
                                          ,(aux (cdr forms))))))))
       
       ,(sequence-set-state seq state-name))))

(defun destructuring-loop (c forms expr greedy? k) 
  (with-gensyms (aux state-name)
    (k-once k
      `(progn ,@(unless greedy? `(,(funcall k)))
              ,(sequence-set-state c (sequence-initial-state c expr))
              (rec ,aux () 
                (let ((,state-name ,(sequence-get-state c)))
                  ,(destructure-form c forms
                                     (k (if greedy?
                                            `(progn (,aux) 
                                                    ,(funcall k))
                                            `(progn ,(funcall k)
                                                    (,aux)))))
                  ,(sequence-set-state c state-name)))
              ,(sequence-set-state c (sequence-initial-state c expr))
              ,@(when greedy? `(,(funcall k)))))))


;;; *-form

(defcomponent *-form (operator destructuring-mixin)
  forms
  (greedy? :initarg :greedy? :initform t :reader greedy-of))

(definit *-form (&rest forms) 
  `(:forms ,(mapcar #'mkform forms)))

(defmethod expand-form ((c *-form) expr2 k)
  (multiple-value-bind (form expr) (find-sequence-form c)
    (destructuring-loop form (forms-of c) expr (greedy-of c) k)))


;;; t-form

(defcomponent t-form (form))

(defmethod matches? ((c t-form) datum)
  (eq datum 't))

(defmethod expand-form ((c t-form) expr k)
  (funcall k))

(defmethod ignored-expr? ((c t-form))
  t)


;;; +-form destructuring

(defcomponent +-form (operator destructuring-mixin)
  forms
  (greedy? :initarg :greedy? :initform t :reader greedy-of))

(definit +-form (&rest list)
  `(:forms ,(mapcar #'mkform list)))

(defmethod expand-form ((c +-form) expr2 k)
  (multiple-value-bind (form expr) (find-sequence-form c)
    (let ((state (sequence-initial-state form expr)))
      `(progn ,(sequence-set-state form state)
              ,(destructure-form form (forms-of c)
                                 (k
                                   (destructuring-loop
                                    form (forms-of c)
                                    (sequence-get-state form)
                                    (greedy-of c) k)))
              ,(sequence-set-state form state)))))


;;; vector-rest-form

(defcomponent vector-rest-form (operator sequence-mixin)
  elt index-sym len-name vec-name rest index-sym2 destructuring)

(definit vector-rest-form (elt index-sym len-name vec-name destructuring rest)
  `(:elt ,(mkform elt)
    :index-sym ,(gensym)
    :len-name ,len-name
    :vec-name ,vec-name
    :rest ,rest
    :index-sym2 ,index-sym
    :destructuring ,destructuring))

(defmethod expand-form ((c vector-rest-form) expr k)
  (let ((index-name (index-sym-of c))
        (len-name (len-name-of c))
        (index2-name (index-sym2-of c)))
    `(let ((,index2-name ,index2-name)
           (,index-name (1+ ,index2-name)))
       (declare (ignorable ,index-name ,index2-name))
       ,(let ((ret (expand-form (elt-of c)
                                `(aref ,expr ,index2-name)
                                (k (if (null (rest-of c))
                                       (if (destructuring-of c)
                                           `(unless (< ,index-name ,len-name)
                                              ,(funcall k))
                                           (funcall k))
                                       `(let ((,index2-name ,index-name))
                                          ,(expand-form (rest-of c)
                                                        expr
                                                        k)))))))
          (if (destructuring-of c)
              `(when (> ,len-name ,index2-name)
                 ,ret)
              ret)))))


;;; vector-form

(defcomponent vector-form (operator)
  elt index-sym len-name vec-name destructuring elt-count)

(definit vector-form (&rest elts)
  (with-gensyms (index-sym len-name vec-name)
    (let* ((destructuring?
            ;; hack
            (let ((toadstool-system::*inner-forms* nil))
              (find-if (lambda (x)
                         (typep x 'destructuring-mixin))
                       (mappend (lambda (x)
                                  (cons x (effective-inner-forms-of x)))
                                (mapcar #'mkform elts))))))
      `(:elt ,(and elts
                  (rec aux ((elts elts))
                    (if (null elts)
                        nil
                        (mkform `(vector-rest ,(car elts)
                                              ,index-sym
                                              ,len-name
                                              ,vec-name
                                              ,destructuring?
                                              ,(aux (cdr elts)))))))
             :index-sym ,index-sym
             :len-name ,len-name
             :vec-name ,vec-name
             :destructuring ,destructuring?
             :elt-count ,(length elts)))))

(defmethod expand-form ((c vector-form) expr k)
  (if (null (elt-of c))
      `(when (and (vectorp ,expr)
                  (zerop (length ,expr)))
         ,(funcall k))
      `(when (and (vectorp ,expr)
                  ,@(when (not (destructuring-of c))
                      `((= (length ,expr) ,(elt-count-of c)))))
         (let ((,(index-sym-of c) 0)
               ,@(when (destructuring-of c)
                   `((,(len-name-of c) (length ,expr))))
               (,(vec-name-of c) ,expr))
           (declare (ignorable ,(vec-name-of c)))
           ,(expand-form (elt-of c) expr k)))))

(defmethod sequence-initial-state ((f vector-rest-form) expr)
  (index-sym2-of f))

(defmethod sequence-cdr-state ((f vector-rest-form) state)
  `(1+ ,state))

(defmethod sequence-endp ((f vector-rest-form) state)
  `(<= ,(len-name-of f) ,state))

(defmethod sequence-item ((f vector-rest-form) state)
  `(aref ,(vec-name-of f) ,state))

(defmethod sequence-set-state ((f vector-rest-form) state) 
  `(setq ,(index-sym-of f) ,state))

(defmethod sequence-get-state ((f vector-rest-form))
  (index-sym-of f))


;;; destructuring-mixin



;;; with-accessors-form

(defcomponent with-accessors-form (operator)
  functions forms)

(definit with-accessors-form (&rest options)
  (multiple-value-bind (functions forms)
      (loop for (fn form) on options by #'cddr
            collect fn into functions
            collect (mkform form) into forms
            finally (return (values functions forms)))
    (assert (= (length functions)
               (length forms)))
    `(:functions ,functions :forms ,forms)))

(defmethod expand-form ((c with-accessors-form) expr k)
  (rec aux ((forms (forms-of c))
            (fns (functions-of c))) 
    (if (null forms)
        (funcall k)
        (expand-form (car forms)
                     `(,(car fns) ,expr)
                     (k (aux (cdr forms)
                             (cdr fns)))))))


;;; numeric forms

(defcomponent comparison-operator (operator))

(defmethod expand-form ((c comparison-operator) expr k)
  (ecase (length (form-of c))
    (2 (destructuring-bind (name first) (form-of c)
           `(when (,name ,expr ,first)
              ,(funcall k))))
    (3 (destructuring-bind (name first second) (form-of c)
           `(when (,name ,first ,expr ,second)
              ,(funcall k))))))

(defcomponent >-form (comparison-operator))
(defcomponent >=-form (comparison-operator))
(defcomponent <-form (comparison-operator))
(defcomponent <=-form (comparison-operator))


;;; class-form

(defcomponent class-form (operator macro-mixin))

(defexpand class-form (class-name &rest accessors-and-values)
  `(and (typep ',class-name)
        (with-accessors . ,accessors-and-values)))


;;; eql-form

(defcomponent eql-form (operator)
  lisp-expr)

(definit eql-form (lisp-expr)
  `(:lisp-expr ,lisp-expr))

(defmethod expand-form ((c eql-form) expr k)
  `(when (,(name-of c) ,expr ,(lisp-expr-of c))
     ,(funcall k)))


;;; equal-form

(defcomponent equal-form (eql-form))


;;; assoc-form

(defcomponent assoc-form (operator)
  cases forms)

(definit assoc-form (&rest cases)
  `(:cases ,(loop for (key value) on cases by #'cddr
                  collect (list key value))
    :forms ,(loop for (key value) on cases by #'cddr
                  collect (mkform value))))

(defmethod assoc-value ((c assoc-form))
  'cdr)

(defmethod assoc-key ((c assoc-form) key)
  key)

(defmethod assoc-test ((c assoc-form))
  'eql)

(defmethod expand-form ((c assoc-form) expr k)
  (expand-form (mkform '(typep 'list))
               expr
               (k
                 (rec aux ((cases (cases-of c))
                           (forms (forms-of c)))
                   (if (null cases)
                       (funcall k)
                       (let* ((case (car cases))
                              (var (car forms))
                              (key (first case)))
                         (expand-form var
                                      `(,(assoc-value c)
                                         (assoc ,(assoc-key c key)
                                                ,expr
                                                :test ',(assoc-test c)))
                                      (k (aux (cdr cases)
                                              (cdr forms))))))))))


;;; debug-mixin

(defvar *debug-nesting-level* 0)

(defun debug-print (datum expr)
  (let ((*print-level* 4))
    (format *debug-io* "~A~A = ~S~%" (make-string *debug-nesting-level*
                                                 :initial-element #\Space)
            datum expr)))

(defclass debug-mixin (component-mixin) ())
(defmethod expand-form :around ((c debug-mixin) expr k)
  (with-gensyms (block-name)
    `(block ,block-name
       (let ((*debug-nesting-level* (1+ *debug-nesting-level*)))
         (debug-print ',(format nil "~S" (form-of c)) ,expr)
         ,(call-next-method c expr k)))))
