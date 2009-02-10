(in-package #:toadstool-utils)

(defmacro with-gensyms ((&rest syms) &body body)
  `(let ,(loop for i in syms collect (list i `(gensym ,(symbol-name i))))
     ,@body))

(defmacro rec (name (&rest vars) &body body)
  `(labels ((,name ,(mapcar #'car vars)
              ,@body))
     (let* ,vars
       (,name ,@(mapcar #'car vars)))))

(defun mappend (fn list &rest more-lists)
  (declare (dynamic-extent more-lists))
  (loop for i in (apply #'mapcar fn list more-lists)
        append i))
