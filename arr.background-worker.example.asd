(asdf:defsystem #:arr.background-worker.example
  :author "Bruno Dias"
  :serial t
  :depends-on (#:fiveam
               #:log4cl
               #:frugal-uuid
               #:arr
               #:bordeaux-threads
               #:arr.global-background-worker
               #:arr.in-memory-queue
               #:sqlite
               #:woo)
  :components ((:module "background-worker"
                :components ((:file "example")))))
