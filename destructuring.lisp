(in-package #:toadstool-system)

(defclass destructuring-mixin (sequence-mixin) ())

(defgeneric ignored-expr? (form)
  (:documentation "If T is returned, expression isn't rebound to a gensym.
Useful for forms like T and destructuring operators to avoid macroexpansion clutter.")
  (:method (form)
    nil)
  (:method ((c macro-mixin))
    t)
  (:method ((c destructuring-mixin))
    t))

(defmethod sequence-initial-state ((f destructuring-mixin) expr)
  (sequence-get-state (find-sequence-form f)))

(defmethod sequence-cdr-state ((f destructuring-mixin) state)
  (sequence-cdr-state (find-sequence-form f) state))

(defmethod sequence-endp ((f destructuring-mixin) state)
  (sequence-endp (find-sequence-form f) state))

(defmethod sequence-item ((f destructuring-mixin) state)
  (sequence-item (find-sequence-form f) state))

(defmethod sequence-item ((f destructuring-mixin) state)
  (sequence-item (find-sequence-form f) state))

(defmethod sequence-set-state ((f destructuring-mixin) state)
  (sequence-set-state (find-sequence-form f) state))

(defmethod sequence-get-state ((f destructuring-mixin))
  (sequence-get-state (find-sequence-form f)))
