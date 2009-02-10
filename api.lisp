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

(defun guard? (x)
  (and (consp x)
       (eq 'when (car x))))

(defun %toad-case (body phail)
  (let* ((pos (position '-> body)) 
         (exprs (subseq body 0 (/ pos 2)))
         (cases (subseq body (- pos (length exprs))))) 
    (assert (zerop (mod (length (remove-if #'guard? cases))
                        (+ 2 (length exprs)))))
    (with-gensyms (block-name)
      (let ((syms (loop for i in exprs collect (gensym))))
        `(block ,block-name
           (let ,(mapcar #'list syms exprs)
             ,(rec aux ((xs cases)) 
                (if (null xs)
                    (and phail `(funcall ,phail (list ,@syms) ',cases))
                    (let* ((pos (or (position '-> xs)
                                    (error "Malformed pattern")))
                           (patterns (subseq xs 0 pos))
                           (guard? (guard? (nth (+ 1 pos) xs)))
                           (guard (if guard?
                                      `(and . ,(cdr (nth (+ 1 pos) xs)))
                                      't))
                           (if-expr (nth (+ pos (if guard?
                                                    2
                                                    1))
                                         xs))) 
                      (toplevel-expansion block-name 
                                          patterns
                                          syms
                                          guard
                                          if-expr
                                          (aux (nthcdr (if guard?
                                                           (+ 3 pos)
                                                           (+ 2 pos))
                                                       xs))))))))))))

(defmacro toad-case (expr &body cases)
  (%toad-case (cons expr cases) nil))

(defmacro toad-ecase (expr &body cases)
  (%toad-case (cons expr cases) '#'partial-error))

(defmacro toad-ccase (expr &body cases)
  (%toad-case (cons expr cases) '#'partial-cerror))
