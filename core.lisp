(defpackage #:toadstool (:use #:cl)
  (:nicknames #:toad))

(in-package #:toadstool)

(defvar +magic-unbound-value+ '#:bug)

(defvar *keywords* '())
(defvar *vars*)
(defvar *trace* '())
(defvar *forms* '())
(defvar *nestings* '())
(defvar *end-nestings* '())
(defvar *keyword-fns* '())

;; todo really do names for nesting, forms etc

(defmacro continuation (keys &body body)
  `(lambda (&key ,@keys &allow-other-keys)
     ,@body))

(defmacro rec (name (&rest vars) &body body)
  `(labels ((,name ,(mapcar #'car vars)
              ,@body))
     (let* ,vars
       (,name ,@(mapcar #'car vars)))))

(defun register-matcher-keyword (keyword fn)
  (setf *keywords* (list* (cons keyword fn) *keywords*)))

(defun register-matcher-form (fn)
  (setf *forms* (cons fn *forms*)))

(defmacro with-gensyms ((&rest syms) &body body)
  `(let ,(loop for i in syms collect (list i `(gensym ,(symbol-name i))))
     ,@body))

(defmacro define-keyword-fns ((op pattern expr k) &body body)
  `(setq *keyword-fns* (cons (lambda (,op ,pattern ,expr ,k)
                               ,@body)
                             *keyword-fns*)))

(define-keyword-fns (op pattern expr k)
  (declare (ignore op pattern expr k))
  '((:walk (pattern expr k)
     (frob-expand pattern expr k))))

;; todo generalize make-keyword-lambda keyword-fns

(defmacro matcher-keyword-lambda ((pattern expr-name k-name)
                                  &body cases)
  (with-gensyms (op-name pattern-name)
    (let* ((decls (loop for i in cases
                        while (eq (car i) 'declare)
                        collect i))
           (cases (loop for i on cases
                        unless (and (consp i)
                                    (consp (car i))
                                    (eq (caar i) 'declare))
                          do (return i)))
           (docstring (if (and (stringp (car cases))
                               (< 1 (length cases)))
                          (car cases)))
           (cases (if docstring
                      (cdr cases)
                      cases)))
      `(lambda (,op-name ,pattern-name ,expr-name ,k-name)
         ,@(and docstring (list docstring))
         ,@decls
         (destructuring-bind ,pattern (cdr ,pattern-name)
           ,(let ((flets (loop for i in *keyword-fns*
                               append (funcall i op-name pattern-name
                                               expr-name k-name))))
              (assert (= (length (remove-duplicates flets :key #'car))
                         (length flets))
                      nil
                      "Duplicate flets: ~S" flets)
              `(flet ,flets
                 (case ,op-name
                   ,@cases))))))))

(defmacro define-matcher-keyword ((keyword pattern expr-name k-name)
                                  &body cases)
  `(register-matcher-keyword
    ',keyword
    (matcher-keyword-lambda (,pattern ,expr-name ,k-name) ,@cases)))

(defmacro define-matcher-form ((pattern expr-name k-name)
                                  &body cases)
  `(register-matcher-form 
    (matcher-keyword-lambda (,pattern ,expr-name ,k-name) ,@cases)))

(defun lookup-matcher-keyword (keyword)
  (cdr (assoc keyword *keywords*)))

(defun expand-foo (fn symbol-name pattern expr k)
  (let ((keyword-expr-name (gensym symbol-name))
        (body (lambda (expr-name)
                (let ((*trace* (list* (cons pattern expr-name)
                                      *trace*))) 
                  (funcall fn :expand pattern expr-name k)))))
    (if (symbolp expr)
        (funcall body expr)
        `(let ((,keyword-expr-name ,expr))
           ,(funcall body keyword-expr-name)))))

(defun expand-matcher (keyword pattern expr k)
  (expand-foo (lookup-matcher-keyword keyword)
              (format nil "OP-~A-" keyword)
              pattern expr k))

(defun funcall-matcher (keyword msg pattern expr k)
  (funcall (lookup-matcher-keyword keyword)
           msg pattern expr k))

(defun lookup-matcher-form (pattern)
  (loop for i in *forms*
        thereis (and (funcall i :test (list 'form pattern) nil nil) i)))

(defun expand-form (pattern expr k)
  (expand-foo (lookup-matcher-form pattern)
              (format nil "FORM-~S-" pattern)
              (list 'form pattern) expr k))

(defun frob-expand (pattern expr k)
  (cond ((lookup-matcher-form pattern)
         (expand-form pattern expr k))
        ((and (consp pattern)
              (lookup-matcher-keyword (car pattern))) 
         (expand-matcher (car pattern) pattern expr k))
        (t (error "Unable to parse pattern ~S" pattern))))

(defun register-nesting (fn)
  (setq *nestings* (cons fn *nestings*)))

(defun register-end-nesting (fn)
  (setq *end-nestings* (cons fn *nestings*)))

(defmacro nesting-lambda ((patterns expressions guard if-expr else-expr k)
                          &body body)
  `(lambda (,patterns ,expressions ,guard ,if-expr ,else-expr ,k)
     ,@body))

(defmacro define-nesting ((patterns expressions guard
                                    if-expr else-expr k)
                          &body body)
  `(register-nesting
    (nesting-lambda (,patterns ,expressions ,guard
                               ,if-expr ,else-expr ,k)
      ,@body)))

(defmacro with-end-nesting ((k) nesting-body &body body)
  `(let ((*end-nestings* (cons (lambda (,k)
                                 ,nesting-body)
                               *end-nestings*)))
     ,@body))

(defun call/nestings (nestings args k)
  (rec aux ((xs nestings))
    (if (endp xs)
        (funcall k)
        (apply (car xs) (append args
                                (list (continuation ()
                                        (aux (cdr xs)))))))))

(defmacro %patmatch (patterns expressions guard if-expr else-expr)
  (assert (= (length patterns)
             (length expressions)))
  (assert (not (endp patterns)))
  (let ((kont nil)
        (expr-syms (loop for i in expressions collect (gensym "EXPR")))
        (nesting-list (list patterns expressions guard if-expr else-expr)))
    (with-gensyms (block-name)
      (setq kont
       (continuation ()
        `(block ,block-name
           (let ,(mapcar #'list expr-syms expressions)
             ,(rec aux ((patterns patterns)
                        (expressions expr-syms)) 
                (frob-expand (car patterns)
                             (car expressions)
                             (if (null (cdr patterns))
                                 (continuation ()
                                   (call/nestings *end-nestings*
                                                  '() 
                                                  (continuation ()
                                                    `(when ,guard
                                                       (return-from ,block-name
                                                         ,if-expr)))))
                                 (lambda ()
                                   (aux (cdr patterns)
                                        (cdr expressions)))))))
           ,else-expr))))
    (call/nestings *nestings* nesting-list kont)))

(defun list-difference (big small &key
                        (test #'eql) (key #'identity) test-not
                        &aux (test (or test-not test)))
  (loop for i in big
        when (not (member (funcall key i) small
                          :key key :test test))
          collect i))

#+nil(define-matcher-keyword (typep pattern expr)
  )

#+nil (%patmatch (foo (bar 42 bar) foo)
                 '(1 (2 42 2) 1)
                 t
                 (list foo bar) 'bad)
#+nil (%patmatch (foo 42 . 'bar)
                 '(abc 42 . bar)
                 t
                 'ok 'bad)
