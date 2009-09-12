(in-package #:toadstool)

(unless (boundp '*used-components*)
  (setq *used-components* *default-components*))

(define-condition partial-pattern-error (error)
  ((patterns :initarg :patterns :reader patterns-of)
   (exprs :initarg :exprs :reader exprs-of))
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Partial pattern"))))

(defun partial-error (exprs patterns)
  (error 'partial-pattern-error :patterns patterns :exprs exprs))

(defun partial-cerror (exprs patterns)
  (cerror "Return NIL instead."
          'partial-pattern-error
          :patterns patterns
          :exprs exprs))

(defun %toad-case (exprs cases phail)
  (with-gensyms (block-name)
    (let ((syms (loop for i in exprs collect (gensym))))
      `(block ,block-name
         (let ,(mapcar #'list syms exprs)
           ,(rec aux ((xs cases))
                 (if (null xs)
                     (and phail `(funcall ,phail (list ,@syms) ',cases))
                     (let* ((patterns (caar xs))
                            (exprs (cdar xs))
                            (guard? (typep (car exprs)
                                           '(cons (eql when)
                                                  (cons t null))))
                            (guard (if guard? (cadar exprs) t))
                            (progn (if guard? (cdr exprs) exprs)))
                       (toplevel-expansion block-name
                                           patterns
                                           syms
                                           guard
                                           `(progn . ,progn)
                                           (aux (cdr xs)))))))))))

(defmacro toad-case (exprs &body cases) 
  (%toad-case exprs cases nil))

(defmacro toad-ecase (exprs &body cases)
  (%toad-case exprs cases '#'partial-error))

(defmacro toad-ccase (exprs &body cases)
  (%toad-case exprs cases '#'partial-cerror))

(defclass macrolet-form (operator)
  ((name :initarg name :reader name-of :allocation :class)
   (function :initarg function :reader function-of :allocation :class)
   (expansion :initarg form :reader expansion-of)))

(defun make-macrolet-class (name function)
  (let* ((m-f (find-class 'macrolet-form))
         (class (make-instance 'standard-class :direct-superclasses
         `(,m-f))))
    (closer-mop:finalize-inheritance class)
    (reinitialize-instance (closer-mop:class-prototype class)
                           'function function
                           'name name)
    class))

(defmethod initialize-instance :after ((c macrolet-form) &key)
  (setf (slot-value c 'expansion)
        (mkform (apply (function-of c)
                       (cdr (form-of c))))))

(defmethod expand-form ((c macrolet-form) expr k)
  (expand-form (expansion-of c) expr k))

(defmacro toad-macrolet (&environment env bindings &body body)
  (let* ((macros (loop for (name lambda-list . body) in bindings
                      collect (make-macrolet-class name
                                   (compile nil `(lambda ,lambda-list .
                                   ,body)))))
         (*used-components* (append macros *used-components*)))
    (#+sbcl sb-cltl2:macroexpand-all
     #-sbcl cl-walker:macroexpand-all
     `(progn . ,body) env)))

(defmacro toad-case1 (expr &body cases)
  `(toad-case (,expr)
      ,@(loop for (pattern . body) in cases
              collect `((,pattern) . ,body))))

(defmacro toad-ecase1 (expr &body cases)
  `(toad-ecase (,expr)
      ,@(loop for (pattern . body) in cases
              collect `((,pattern) . ,body))))

(defmacro toad-ccase1 (expr &body cases)
  `(toad-ccase (,expr)
      ,@(loop for (pattern . body) in cases
              collect `((,pattern) . ,body))))
