(defpackage #:toadstool-html
    (:use #:cl #:toadstool #:toadstool-system #:toadstool-impl)
  (:export #:html))

(in-package #:toadstool-html)

(defun intern-keyword (thing &aux
                       (k (load-time-value (find-package '#:keyword))))
  (typecase thing
    (symbol (intern (symbol-name thing) k))
    (t      (intern (string-upcase thing) k))))

(defcomponent html-assoc-form (assoc-form))

(defmethod assoc-value ((c html-assoc-form))
  'second)

(defmethod assoc-key ((c html-assoc-form) key)
  (intern-keyword key))

(defmethod assoc-test ((c html-assoc-form))
  'eql)

(pushnew 'html-assoc-form *used-components*)

(defcomponent html-form (operator macro-mixin))

;;; XXX BUG: (html :a alist (* t)) expands (* t) to (cons (* t) nil) and
;;;          so it fails with empty child nodes
;;;          It should be fixed in CONS-FORM

(defexpand html-form (tag alist &optional (other-forms t))
  `(list* ,tag
          (html-assoc . ,alist)
          ,other-forms))

(pushnew 'html-form *used-components*)
