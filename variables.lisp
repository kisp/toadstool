(in-package #:toadstool-impl)

(defcomponent variable-form (form)
  name)

(defcomponent variable-nesting (nesting))

(deftype matcher-variable ()
  '(and symbol (not keyword) (not (satisfies constantp))))

(defmethod matches? ((c variable-form) datum)
  (typep datum 'matcher-variable))

(definit variable-form (var)
  `(:name ,var))

(defun single-var-occurence? (var)
  (let ((count 0))
    (mapc/forms (lambda (x)
                  (when (and (typep x 'variable-form)
                             (eq (name-of var)
                                 (name-of x)))
                    (incf count))))
    (= count 1)))

(defun toplevel-var-of (var) 
  (find (find-form-expr var) *toplevel-syms*))

(defmethod expand-nesting ((c variable-nesting) k)
  (let ((vars '())
        (ret (funcall k (if-expr-of c) (else-expr-of c))))
    (mapc/forms (lambda (var) 
                  (when (typep var 'variable-form) 
                    (let ((outer? (has-outer-destructuring-mixin? var))
                          (name (name-of var)))
                      (pushnew (list name (if (and (not outer?)
                                                   (single-var-occurence? var))
                                              (toplevel-var-of var)
                                              ''unbound))
                               vars :key #'car))))) 
    `(let ,vars
       (declare (ignorable . ,(mapcar #'car vars)))
       ,ret)))

(defun has-outer-destructuring-mixin? (f)
  (loop for form = (outer-form-of f) then (outer-form-of form)
        while form
        thereis (typep form 'destructuring-mixin)))

(defmethod expand-form ((c variable-form) expr k)
  (let ((outer? (has-outer-destructuring-mixin? c))
        (name (name-of c)))
    (cond ((and (not outer?)
                (single-var-occurence? c)
                (toplevel-var-of c))
           (funcall k))
          ((and (not outer?) (single-var-occurence? c)) 
           `(progn (setq ,name ,expr)
                   ,(funcall k)))
          (t (with-gensyms (prev-name) 
               `(if (or (eq ,name 'unbound)
                        (equality ,name ,expr))
                    (let ((,prev-name ,name))
                      (setf ,name ,expr)
                      ,(funcall k)
                      (setf ,name ,prev-name))))))))

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

