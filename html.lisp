(defpackage #:toadstool-html
    (:use #:cl #:toadstool #:toadstool-system #:toadstool-impl)
  (:export #:html))

(in-package #:toadstool-html)

(defcomponent html-assoc-form (assoc-form))

(defmethod assoc-value ((c html-assoc-form))
  'second)

(defmethod assoc-key ((c html-assoc-form) key)
  (string-downcase key))

(defmethod assoc-test ((c html-assoc-form))
  'equal)

(push 'html-assoc-form *used-components*)

(defcomponent html-form (operator macro-mixin))

(defexpand html-form (tag alist &rest other-forms)
  `(list (equal ,(string-downcase tag))
         (html-assoc . ,alist)
         ,@other-forms))

(push 'html-form *used-components*)
