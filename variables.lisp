(in-package #:toadstool-impl)

(defvar +nil+ (make-symbol (string 'nil)))
(defvar *variable-alist* '())

(defcomponent variable-form (form)
  name
  (using-k-once :initform nil :accessor using-k-once-of)
  (previous :initform nil :accessor previous-of))

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

(defun inside-k-once? (name)
  #+ (or) (let ((ret nil))
    (mapc/forms (lambda (x)
                  (when (and (typep x 'variable-form)
                             (eq name (name-of x))
                             (using-k-once-of x))
                    (setq ret t))))
            ret)
  *using-k-once?*)

(defmethod expand-form ((c variable-form) expr k)
  (when *using-k-once?*
    (setf (using-k-once-of c) t))
  (let* ((name (name-of c))
         (check-every-time? (var-checked-every-time? c))
         (old (gensym))
         (multiple? (find name *variable-alist* :key #'car))
         (*variable-alist* (list* (cons name expr) *variable-alist*))
         (prev (cdr multiple?)))
    (when (and prev (null (previous-of c)))
      (setf (previous-of c) prev))
    (cond (check-every-time?
           `(when (or (equality +nil+ ,name)
                      (equality ,name ,expr))
              (let ((,old ,name))
                (setq ,name ,expr)
                ,(funcall k)
                (setq ,name ,old))))
          (multiple?
           `(progn (when (equality ,prev ,expr) 
                      ,(funcall k))))
          (t
           `(progn (setq ,name ,expr)
                   ,(funcall k)
                   (setq ,name nil))))))

(defun var-checked-every-time? (var)
  (or (has-outer-destructuring? var)
      (inside-k-once? var)))

(defmethod expand-nesting ((c variable-nesting) k)
  (let ((vars '())
        (*variable-alist* '()))
    (mapc/forms (lambda (x)
                  (when (typep x 'variable-form)
                    (push x vars))))
    (let ((bindings (remove-duplicates vars :key #'name-of)))
      (with-end-nesting ((if else k)
                         `(progn ,@(loop for var in bindings
                                         for name = (name-of var)
                                         when (var-checked-every-time? var)
                                           collect `(when (eq ,name +nil+)
                                                      (setq ,name nil)))
                                 ,(funcall k if else)))
        (let ((body (funcall k (if-expr-of c) (else-expr-of c)))
              (vars (mapcar (lambda (c)
                              (list (name-of c)
                                    (if (var-checked-every-time? c)
                                        `+nil+
                                        nil)))
                            bindings)))
          `(let ,vars
             ,body))))))


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

