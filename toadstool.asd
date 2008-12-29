(asdf:defsystem #:toadstool
    :name "toadstool"
    :author "Stanislaw Halik"
    :licence "MIT/X11"
    :description "Pattern matcher"
    :depends-on () 
    :serial t
    :components ((:file "core")
                 (:file "variables")
                 (:file "forms")
                 (:file "matchers")))
