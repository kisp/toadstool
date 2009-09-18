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

(defun remove-from-plist (plist to-remove)
  (loop for (k v) on plist by #'cddr
        unless (member k to-remove)
          collect k and collect v))

(defmacro thunk (form)
  `(lambda () ,form))

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
                              (when (or (null (getf s :accessor))
                                        (not (eq (getf s :accessor)
                                                 (getf s :reader))))
                                (foo :reader s (thunk (intern
                                                       (fmt "~A~A" str of)))))
                              req
                              (remove-from-plist
                               s '(:reader :initarg :required)))))
         ,@options))))

(defun gensym? (x)
  (and (symbolp x)
       (null (symbol-package x))))

(defun extract-prefix (sym suffix)
  (let* ((str (string sym))
         (suffix (string suffix))
         (diff (- (length str) (length suffix))))
    (when (and (> (length str) (length suffix))
               (string= suffix str :start2 diff))
      (intern (subseq str 0 diff)
              (symbol-package sym)))))

(defmacro if-matches (test k)
  ``(if ,,test ,(funcall ,k)))
