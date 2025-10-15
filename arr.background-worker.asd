(asdf:defsystem #:arr.background-worker
  :author "Bruno Dias"
  :license "Unlicense"
  :serial t
  :depends-on (#+sbcl #:sb-concurrency
               #:bordeaux-threads
               #:local-time
               #:log4cl
               #:arr)
  :components ((:module "background-worker"
                :components ((:file "package")))))
