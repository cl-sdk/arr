(asdf:defsystem #:arr.in-memory-queue
  :author "Bruno Dias"
  :license "Unlicense"
  :serial t
  :depends-on (#+sbcl #:sb-concurrency
               #:log4cl
               #:arr)
  :components ((:module "in-memory-queue"
                :components ((:file "package")))))
