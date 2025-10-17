(asdf:defsystem #:arr.background-worker.test
  :author "Bruno Dias"
  :serial t
  :depends-on (#+sbcl #:sb-concurrency
               #:fiveam
               #:arr
               #:bordeaux-threads
               #:arr.background-worker
               #:arr.in-memory-queue)
  :components ((:module "background-worker"
                :components ((:file "test")))))
