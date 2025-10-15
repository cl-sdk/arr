(asdf:defsystem #:arr.background-worker.test
  :author "Bruno Dias"
  :serial t
  :depends-on (#:fiveam
               #:arr
               #:bordeaux-threads
               #:arr.background-worker)
  :components ((:module "background-worker"
                :components ((:file "test")))))
