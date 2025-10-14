(asdf:defsystem #:arr.test
  :author "Bruno Dias"
  :serial t
  :depends-on (#:fiveam
               #:arr
               #:bordeaux-threads)
  :components ((:file "test")))
