(asdf:defsystem #:toadstool-tests
  :depends-on (#:toadstool #:stefil)
  :serial t
  :components ((:file "tests")))
