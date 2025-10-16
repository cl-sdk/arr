(asdf:defsystem #:arr.global-background-worker
  :author "Bruno Dias"
  :serial t
  :depends-on (#:fiveam
               #:log4cl
               #:frugal-uuid
               #:arr
               #:bordeaux-threads
               #:arr.background-worker
               #:sqlite
               #:woo)
  :components ((:module "background-worker"
                :components ((:file "global-background-worker")))))
