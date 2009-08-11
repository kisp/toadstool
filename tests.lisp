(defpackage #:toadstool-tests (:use #:cl #:stefil #:toadstool)
  (:export #:tests))
(in-package #:toadstool-tests)

(defsuite tests)
(in-suite tests)

(defmacro toad-test (name ret &body body)
  `(deftest ,name ()
     (is ,ret (toad-case . ,body))))

(defmacro deftest* (name expr expected-value &body body)
  `(deftest ,name ()
     ,@(loop for i in '(list vector)
             collect `(is ,expected-value
                           (toad-case ((coerce ,expr ',i))
                                ,@(eval `(let ((type ',i))
                                           ,@body)))))))

(defmacro deftest* (name expr expected-value &body body)
  `(deftest ,name ()
     ,(labels ((frob (type)
                 `(is (coerce ,expected-value '(or ,type t))
                      (toad-case ((coerce ,expr '(or ,type t)))
                                 ,@(subst type 'type body)))))
        `(progn ,(frob 'list)
                ,(frob 'vector)))))

(deftest* simple-unification '(42 69 foo 69 42) '(42 69)
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

(deftest* push-form '(1 a 1 2 b c) '((1 2 3) (a b c))
  (((type (* (or (and (typep 'number)
                       (push numbers))
                  (and (typep 'symbol)
                       (push symbols))))))
   (list numbers symbols)))

(deftest many-expressions ()
  )

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


