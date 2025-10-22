(asdf:defsystem #:arr.sqlite-queue
  :author "Bruno Dias"
  :license "Unlicense"
  :serial t
  :depends-on (#+sbcl #:sb-concurrency
               #:log4cl
               #:arr)
  :components ((:module "sqlite-queue"
                :components ((:file "package")))))
