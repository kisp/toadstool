(in-package #:toadstool-impl)

(defvar *variable-alist* '())
(defvar +nil+ (make-symbol (string 'nil)))

(defcomponent variable-form (form)
  name)

(defcomponent variable-nesting (nesting))

(deftype matcher-variable ()
  '(and symbol (not keyword) (not (satisfies constantp))))

(defmethod matches? ((c variable-form) datum)
  (typep datum 'matcher-variable))

(definit variable-form (var)
  `(:name ,var))

(defun has-outer-destructuring? (f)
  (loop for form = (outer-form-of f) then (outer-form-of form)
        while form
        thereis (typep form 'destructuring-mixin)))

(defmethod expand-form ((c variable-form) expr k)
  (let* ((name (name-of c))
         (foo (find name *variable-alist* :key #'car))
         (prev (cdr foo))
         (*variable-alist* (list* (cons name expr) *variable-alist*))
         (outer? (has-outer-destructuring? c))
         (old (gensym)))
    (cond (outer?
           `(when (or (eq +nil+ ,name)
                      (equality ,name ,expr))
              (let ((,old ,name))
                (setq ,name ,expr)
                ,(funcall k)
                (setq ,name ,old))))
          (foo
           `(when (equality ,prev ,expr)
              ,(funcall k)))
          (t
           `(progn (setq ,name ,expr)
                   ,(funcall k)
                   (setq ,name nil))))))

(defmethod expand-nesting ((c variable-nesting) k)
  (let ((vars '()))
    (mapc/forms (lambda (x)
                  (when (typep x 'variable-form)
                    (push x vars))))
    (let ((bindings (loop for var in (remove-duplicates vars :key #'name-of)
                          for name = (name-of var)
                          if (some (lambda (x)
                                     (and (eq name (name-of x))
                                          (has-outer-destructuring? x)))
                                   vars)
                            collect (list name `',+nil+)
                          else
                            collect (list name nil))))
      (with-end-nesting ((if else k)
                         `(progn ,@(loop for (var val) in bindings
                                         when (not (null val))
                                           collect `(when (eq ,val ,var)
                                                      (setq ,var nil)))
                                 ,(funcall k if else)))
        `(let ,bindings
           ,(funcall k (if-expr-of c) (else-expr-of c)))))))


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

