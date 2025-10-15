(asdf:defsystem #:arr
  :author "Bruno Dias"
  :license "Unlicense"
  :serial t
  :depends-on (#+sbcl #:sb-concurrency
               #:bordeaux-threads
               #:local-time
               #:log4cl)
  :components ((:file "package")
               (:file "generics")
               (:file "background-workers")))
