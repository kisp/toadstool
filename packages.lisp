(defpackage #:toadstool-utils (:use #:cl)
  (:export #:with-gensyms #:rec #:mappend #:fmt #:mklist #:partition
           #:remove-from-plist #:thunk #:defcomponent #:gensym?
           #:extract-prefix #:if-matches))

(defpackage #:toadstool-system (:use #:cl #:toadstool-utils)
  (:export #:*default-components* #:*used-components*
           #:component #:form #:operator #:form-of #:inner-forms-of
           #:outer-form-of #:effective-inner-forms-of #:name-of
           #:component-mixin #:nesting #:if-expr-of #:else-expr-of
           #:sequence-mixin #:k #:with-end-nesting #:k-once #:matches?
           #:expand-form #:mkform #:init #:sequence-initial-state
           #:sequence-cdr-state #:sequence-endp #:sequence-item
           #:sequence-get-state #:sequence-set-state #:find-sequence-form
           #:definit #:defcomponent #:equality #:if-matches #:macro-mixin
           #:debug-mixin #:mapc/forms #:destructuring-mixin #:defexpand
           #:*toplevel-syms* #:toplevel-expansion #:expand-nesting
           #:find-form-expr #:*using-k-once?*))

(defpackage #:toadstool-impl (:use #:cl #:toadstool-system #:toadstool-utils)
  (:export #:*default-components* #:satisfies-form #:typep-form #:variable-form
           #:not-form #:quote-form #:list*-form #:literal-form #:+-form
           #:t-form #:and-form #:list-form #:*-form #:cons-form #:or-form
           #:variable-nesting #:vector-form #:vector-rest-form #:push-form
           #:push-nesting #:>-form #:>=-form #:<-form #:<=-form #:eql-form
           #:equal-form #:with-accessors-form #:class-form
           #:assoc-form #:assoc-value #:assoc-key #:assoc-test #:char-form
           #:string-form))

(defpackage #:toadstool (:use #:cl #:toadstool-impl #:toadstool-system
                              #:toadstool-utils)
  (:export #:toad-case #:toad-ecase #:toad-ccase #:partial-pattern-error
           #:toad-macrolet #:toad-case1 #:toad-ecase1 #:toad-ccase1))
