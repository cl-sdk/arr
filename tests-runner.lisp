(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))

(push *default-pathname-defaults* ql:*local-project-directories*)

(ql:quickload '(:arr.test :arr.background-worker.test))

(defun run-tests (coverage)
  (5am:run-all-tests)
  (when coverage
    (sb-cover:report #P"./coverage/")))

(setf *debugger-hook*
      (lambda (c h)
        (log:error c h)
        (uiop:quit -1))
      fiveam:*on-error* nil)

(unless (run-tests t)
  (exit :code 1 :abort t))
