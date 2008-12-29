;; XXX discuss whether macro continuation should support non-keyword
;; plist keys and whether it should allow-other-keys

;; XXX discuss whether forms should have a symbol as an identifier to
;; ease redefinition

(in-package #:toadstool)

;; t
(define-matcher-form (pattern expr k)
  (declare (ignore expr))
  (:test (typep pattern '(cons (eql t) null)))
  (:vars '())
  (:expand (funcall k)))

;; constant
(define-matcher-form (pattern expr k)
  (:test (typep pattern '(cons (satisfies constantp) null)))
  (:vars '())
  (:expand `(when (equality ,(car pattern) ,expr)
              ,(funcall k))))
