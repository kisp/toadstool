(in-package #:toadstool-system)

(defclass destructuring-mixin (sequence-mixin) ())

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
