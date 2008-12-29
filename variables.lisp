(in-package #:toadstool)

(defvar *variable-map*)
(defvar *ignored*)

(defun ignored-variables (pattern)
  (let ((*ignored* '()))
    (collect-variable-names pattern)
    *ignored*))

(defun used-variables (pattern)
  (list-difference (collect-variable-names pattern)
                   (ignored-variables pattern)))

(define-keyword-fns (op pattern expr k)
  (declare (ignore op pattern expr k))
  '((:vars (pattern)
     (collect-variable-names pattern))))

;; variable
(define-matcher-form (pattern expr k)
  (:test (typep pattern '(cons matcher-variable null)))
  (:vars (list (car pattern)))
  (:expand (let ((pattern (car pattern)))
             `(when (or (eq ,(var pattern) +magic-unbound-value+)
                        (equality ,(var pattern) ,expr))
                (let ((,(var pattern) ,expr))
                  (declare (ignorable ,(var pattern)))
                  ,(funcall k))))))

(defun frob-vars (pattern)
  (remove-duplicates
   (rec aux ((x pattern))
     (cond ((null x) '())
           ((lookup-matcher-form pattern)
            (collect-form-variables pattern))
           ((and (consp x)
                 (lookup-matcher-keyword (car x)))
            (collect-matcher-variables (car x) x))
           ((consp x)
            (append (frob-vars (car x))
                    (frob-vars (cdr x))))
           (t (error "Can't get vars from ~S" pattern))))))

;; bind variables
(define-nesting (patterns expressions guard if-expr else-expr k)
  (declare (ignore guard if-expr else-expr expressions))
  (let* ((vars (reduce #'append (mapcar #'collect-variable-names patterns)))
         (bound-variables (loop for i in vars collect (gensym (string i))))
         (*variable-map* (mapcar #'cons vars bound-variables)))
    (with-end-nesting (k) `(let ,(mapcar #'list vars bound-variables)
                             (declare (ignorable ,@vars))
                             ,(funcall k))
      `(let (,@(loop for i in bound-variables
                     collect (list i `',+magic-unbound-value+)))
         (declare (ignorable ,@bound-variables))
         ,(funcall k)))))

(defun collect-matcher-variables (keyword pattern)
  (funcall (lookup-matcher-keyword keyword)
           :vars pattern nil nil))

(defun collect-form-variables (pattern)
  (funcall (lookup-matcher-form pattern)
           :vars (list 'form pattern) nil nil))

(deftype matcher-variable ()
  `(and symbol
        (not (or keyword null (satisfies constantp)))))

(defun var (x)
  (cdr (or (assoc x *variable-map*)
           (error "BUG: No such variable ~S" x))))

(defun collect-variable-names (pattern)
  (remove-duplicates (frob-vars pattern)))
