(in-package #:toadstool)

(defvar *equality* 'eql)

(defmacro equality (x y)
  `(,*equality* ,x ,y))

;; todo kleene-star
;; separate into toadstool-system for stuff used for defining keywords

;; remove the matcher or add checks against variable names being
;; keywords or forms

;; XXX discuss changing matchers, forms et al to classes and methods
;; XXX really do that

;; add stuff like keyword in form (keyword :foo 42 :bar 69) expands to
;; &key in matcher

#+nil
(define-matcher-keyword (let (var value) expr k)
  (:vars (list var))
  (:expand
   (frob-variable var expr (continuation ()
                             (:walk value expr k)))))

(define-matcher-keyword (typep (type) expr k)
  (:vars '())
  (:expand
   `(when (typep ,expr ,type)
      ,(funcall k))))

(define-matcher-keyword (funcall (fn) expr k)
  (:vars '())
  (:expand `(when (funcall ,fn ,expr) ,(funcall k))))

(define-matcher-keyword (and (&rest clauses) expr k)
  (:vars (:vars clauses))
  (:expand
   (rec aux ((xs clauses))
     (if (null xs) (funcall k)
         (:walk (car xs) expr
                (continuation () 
                  (if (null (cdr xs))
                      (funcall k)
                      (aux (cdr xs)))))))))

(define-matcher-keyword (or (&rest clauses) expr k)
  (:vars (:vars clauses))
  (:expand
   (with-gensyms (fn-name)
     (let ((vars (mapcar #'var (:vars clauses))))
       `(labels ((,fn-name (,@vars) ,(funcall k)))
          ,@(loop for i in clauses
                  collect (:walk i expr (constantly `(,fn-name ,@vars)))))))))

(define-matcher-keyword (quote (val) expr k)
  (:vars nil)
  (:expand `(when (equality ',val ,expr)
              ,(funcall k))))

(define-matcher-keyword (list (&rest elts) expr k)
  (:vars (:vars elts))
  (:expand
   (rec aux ((xs elts)
             (expr expr))
     (typecase xs
       (null `(when (null ,expr)
                ,(funcall k)))
       (atom (frob-expand xs expr k))
       (t `(when (consp ,expr)
             ,(frob-expand
               (car xs) `(car ,expr)
               (continuation ((expr `(cdr ,expr)))
                 (with-gensyms (list-tail)
                   `(let ((,list-tail ,expr))
                      (declare (ignorable ,list-tail))
                      ,(let ((*trace* (list* (list* (cons 'list (cdr xs))
                                                    list-tail)
                                             *trace*)))
                            (aux (cdr xs) list-tail)))))))))))
  (:sequence-p t)
  (:sequence-initial-state expr)
  (:sequence-next-state '#'cdr)
  (:sequence-endp `(not (consp ,expr)))
  (:sequence-item `(car ,expr)))

(define-matcher-keyword (cons (car cdr) expr k)
  (:vars (:vars (list car cdr)))
  (:expand (:walk `(list ,car . ,cdr) expr k)))

(define-matcher-keyword (vector (&rest elts) expr k)
  (:vars (:vars elts))
  (:expand `(when (and (vectorp ,expr)
                       ;; TODO change when adding kleene star
                       (= (length ,expr) ,(length elts))
                       ,(rec aux ((i 0))
                          (if (= i (length elts))
                              (funcall k)
                              (frob-expand (elt elts i)
                                           `(aref ,expr ,i)
                                           (continuation ()
                                             (aux (1+ i))))))))))

(defmacro with-collect2 ((&rest fns) result &body body)
  (rec aux ((xs fns)
            (vars nil))
      (if (null xs)
          `(progn ,@body
                  (funcall ,result ,@(mapcar (lambda (x) `(cdr ,x))
                                             (reverse vars))))
          (let ((last-name (gensym "LAST-NAME"))
                (list-name (gensym "LIST-NAME")))
            `(let* ((,list-name (list nil))
                    (,last-name ,list-name))
               (flet ((,(car xs) (obj)
                        (declare (optimize (speed 3) (safety 0) (debug 0)
                                           (space 3) (compilation-speed 0)))
                        (setq ,last-name
                              (setf (cdr ,last-name) (cons obj nil)))
                        (cdr ,list-name)))
                 (declare (inline ,(car xs)))
                 ,(aux (cdr xs) (cons list-name vars))))))))

(define-matcher-keyword (ignore (val) expr k)
  (:vars (let ((vars (:vars val)))
           (when (boundp '*ignored*)
             (setq *ignored* (append vars *ignored*)))
           vars))
  (:expand (:walk val expr k)))

(define-matcher-keyword (dolist (&rest pattern) expr k)
  (:vars (:vars (cons 'list pattern)))
  (:expand (let* ((vars (used-variables pattern))
                  (bound-vars (mapcar #'var vars))
                  (temp-fns (loop for i in vars collect (gensym))))
             (with-gensyms (out-name cdr-name out2-name aux-name)
               `(block ,out-name
                  (with-collect2 ,temp-fns (lambda ,bound-vars
                                             ,(funcall k))
                    (rec ,aux-name ((,cdr-name ,expr)) 
                      (let ,(mapcar #'list bound-vars bound-vars)
                        (typecase ,cdr-name
                          (null)
                          (atom (return-from ,out-name))
                          (t (block ,out2-name
                               ,(:walk
                                 (cons 'list pattern) `(car ,cdr-name)
                                 (continuation () 
                                   `(return-from ,out2-name
                                      (progn
                                        ,@(mapcar #'list temp-fns bound-vars) 
                                        (,aux-name
                                         (cdr ,cdr-name))))))
                               (return-from ,out-name))))))))))))

(define-matcher-keyword (not (pattern) expr k)
  (:vars (:vars pattern))
  (:expand (with-gensyms (block-name)
             `(block ,block-name
                ,(:walk pattern expr (continuation ()
                                       `(return-from ,block-name)))
                ,(funcall k)))))

;; ret (keyword pattern expr)
(defun extract-sequence-from-trace (trace op-name)
  (loop with initial = (let ((foo (cdar trace)))
                         (assert (and trace
                                      (symbolp foo)
                                      (null (symbol-package foo))))
                         foo)
        for (name . expr) in *trace*
        for (keyword . pattern) = name
        when (and (atom keyword)
                  (funcall-matcher keyword :sequence-p
                          pattern expr nil))
          do (return (values keyword pattern expr))
        when (not (eq initial expr))
          do #1=(error "Can't find a suitable sequence for ~S" op-name)
        finally #1#))

;; WARNING: ((* A B) (AND A 42)) #:LIST-TAIL1081

(define-matcher-keyword (* (&rest pattern) expr k)
  (:vars (:vars pattern))
  (:expand
   (multiple-value-bind (keyword pattern2 expr2)
       (extract-sequence-from-trace *trace* '*)
     (let ((next (funcall-matcher keyword :sequence-next-state
                                  pattern2 expr nil))
           (vars (mapcar #'var (:vars pattern))))
       (flet ((seq-endp (next-item-state)
                (funcall-matcher keyword :sequence-endp
                                 nil next-item-state nil))
              (item (state)
                (funcall-matcher keyword :sequence-item pattern2 state nil)))
         (with-gensyms (aux-name good-state-name)
           `(rec ,aux-name ((,good-state-name ,(funcall-matcher
                                                keyword
                                                :sequence-initial-state
                                                pattern2 expr2 nil))
                            ,@(mapcar #'list vars vars))
              ,(rec aux2 ((next-item-state good-state-name)
                          (xs pattern)
                          (x (car pattern)))
                    (if (null xs)
                        `(,aux-name ,next-item-state ,@vars)
                        `(when (not ,(seq-endp next-item-state))
                           ,(:walk x (item next-item-state)
                                   (continuation ()
                                     (aux2 `(funcall ,next
                                                     ,next-item-state)
                                           (cdr xs)
                                           (cadr xs)))))))
              ,(funcall k :expr good-state-name))))))))

   #+nil
   `(progn ,@(loop for (name . expr) in *trace*
                           collect `(warn "~S => ~S" ',name ,expr)))
