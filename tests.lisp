(defpackage #:toadstool-tests (:use #:cl #:stefil #:toadstool)
  (:export #:run-tests)
  (:import-from #:stefil #:expected-p #:failure-description-of #:assertion-failed))
(in-package #:toadstool-tests)

(defsuite* tests)

(defun run-tests (&key whine-on-expected-failures-p)
  (flet ((frob-error (e)
           (when (expected-p (failure-description-of e))
             (continue))))
    (if whine-on-expected-failures-p
        (tests)
        (handler-bind ((assertion-failed #'frob-error))
          (tests)))))

(defmacro toad-test (name ret &body body)
  `(deftest ,name ()
     (is (equalp ,ret (toad-case . ,body)))))

(defun coerce* (value type)
  (handler-case (coerce value type)
    (error (e)
      (declare (ignore e))
      value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mklist (x)
    (if (listp x)
        x
        (list x))))

(defmacro deftest* (name expr expected-value &body body)
  (destructuring-bind (name &key (types '(list vector))) (mklist name)
    `(progn ,@(mapcar (lambda (name type)
                        (destructuring-bind (type &key failure-expected-p) (mklist type)
                          `(deftest (,name :compile-before-run t) ()
                             (,(if failure-expected-p
                                   'with-expected-failures
                                   'progn)
                               (is (equal ,expected-value
                                          (toad-case ((coerce* ,expr ',type))
                                            ,@(subst type 'type body))))))))
                      (mapcar (lambda (x)
                                (intern (format nil "~A/~A"
                                                (symbol-name name)
                                                (symbol-name (if (consp x)
                                                                 (first x)
                                                                 x)))))
                              types)
                      types))))

(deftest* simple-unification '(42 69 42) '(42 69)
  (((type a b a))
   (list a b)))

(deftest* unification-failure '(1 2 1 2) '(ok 1 2)
  (((type a b b a))
   'bad)
  (((type a b a b))
   (list 'ok a b)))

(deftest* list-end '(1 2 3) 'ok
  (((type 1 2))
   'fail)
  ((a) 'ok))

(deftest* find-in-list '(1 2 foo -42 a) -42
  (((type (* t)
           (and a
                (satisfies (lambda (x)
                             (and (numberp x)
                                  (minusp x)))))
           (* t)))
   a))

(deftest* failure-unwinds '(foo 1 2 3 4 foo) '(1 2 3 4)
  (((type a
          (or (* (satisfies (lambda (x) (> 3 x))))
               (* (and (typep 'number)
                       (push ret))))
           a))
   ret))

(deftest* logical-ops '(1 1 2 3) '(1 2 3)
  (((type a (and b (not a)) c d))
   'bad)
  (((type a a b c))
   (list a b c)))

(deftest* destructuring-end '(1 2 3 a) 'ok
  (((type (* (typep 'number))))
   'bad)
  ((t) 'ok))

(deftest* push-form '(1 a 2 b c 3) '((1 2 3) (a b c))
  (((type (* (or (and (typep 'number)
                       (push numbers))
                  (and (typep 'symbol)
                       (push symbols))))))
   (list numbers symbols)))

(deftest many-expressions ()
  (is (toad-case (1 1 2)
                 ((a a b) (list a b)) )
      '(1 2))
  (is (toad-case (3 4 5)
                 ((a a b) 'bad)
                 ((a b c) 'ok))
      'ok))

(deftest macro-let ()
  (is '(1 2)
      (toad-macrolet ((foo (&rest args)
                           `(list . ,args)))
                     (toad-case ('(1 2 3))
                                (((foo a b))
                                 (values a b))))))

(deftest* destructuring-unification/1 '(a b b b c b) 'ok
  (((type a (* b))) 'bad)
  ((t) 'ok))

(deftest* destructuring-unification/2 '(a b b b b b) 'ok
  (((type a (* b))) 'ok))

(deftest* (destructuring-empty-sequence/1
           :types (list (vector :failure-expected-p t)))
    '(a) 'ok
  (((type a (* b))) 'ok))

(deftest* (destructuring-empty-sequence/2 :types (list (vector :failure-expected-p t))) '() 'ok
  (((type (* a))) 'ok))

(deftest* variables-checked-every-time-with-k-once?/1 '(1 2 2 2) '(1 2)
  (((type (or b a) (or a b) a b)) 'bad)
  (((type (or b a) (or a b) (or a b) b)) (list a b)))

(deftest* variables-checked-every-time-with-k-once?/2 '(1 1) 'ok
  (((type a (or a b))) 'ok))
