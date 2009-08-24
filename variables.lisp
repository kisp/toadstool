(in-package #:toadstool-impl)

(defvar *variable-alist* '())

(defcomponent variable-form (form)
  name)

(defcomponent variable-nesting (nesting))

(deftype matcher-variable ()
  '(and symbol (not keyword) (not (satisfies constantp))))

(defmethod matches? ((c variable-form) datum)
  (typep datum 'matcher-variable))

(definit variable-form (var)
  `(:name ,var))

(defmethod expand-form ((c variable-form) expr k)
  (let* ((name (name-of c))
         (foo (find name *variable-alist* :key #'car))
         (prev (cdr foo))
         (*variable-alist* (list* (cons name expr) *variable-alist*)))
    (if foo
        `(when (equality ,prev ,expr)
           ,(funcall k))
        `(progn (setq ,name ,expr)
                ,(funcall k)
                (setq ,name nil)))))

(defmethod expand-nesting ((c variable-nesting) k)
  (let ((vars '()))
    (mapc/forms (lambda (x)
                  (when (typep x 'variable-form)
                    (push (name-of x) vars))))
    (setq vars (remove-duplicates vars))
    `(let ,vars
       ,(funcall k (if-expr-of c) (else-expr-of c)))))


(defcomponent push-form (operator)
  var)

(definit push-form (var)
  `(:var ,var))

(defcomponent push-nesting (nesting))

(defmethod expand-nesting ((c push-nesting) k)
  (let ((push '()))
    (mapc/forms (lambda (var)
                  (when (typep var 'push-form)
                    (pushnew (var-of var) push))))
    (with-end-nesting ((if else k) 
                       `(let ,(loop for i in push
                                    collect `(,i (nreverse ,i)))
                          ,(funcall k if else)))
      (let ((ret (funcall k (if-expr-of c) (else-expr-of c)))) 
        `(let ,push
           ,ret)))))

(defmethod expand-form ((c push-form) expr k) 
  `(progn (push ,expr ,(var-of c))
          ,(funcall k)
          (pop ,(var-of c))))

