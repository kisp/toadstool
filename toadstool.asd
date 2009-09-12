(asdf:defsystem #:toadstool
    :name "toadstool"
    :author "Stanislaw Halik"
    :licence "MIT/X11"
    :description "Pattern matcher"
    :depends-on (#:closer-mop #+sbcl #:sb-cltl2 #-sbcl #:cl-walker) 
    :serial t
    :components ((:file "packages")
                 (:file "utils")
                 (:file "core")
                 (:file "destructuring")
                 (:file "forms")
                 (:file "variables")
                 (:file "api")))
