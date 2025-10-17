(defpackage #:arr.global-background-worker
  (:use #:cl)
  (:export
   #:stop-application
   #:start-application))

(in-package :arr.global-background-worker)

(defparameter +app+ nil
  "Global background worker instance.")

(defmethod arr:execute-task ((app (eql :global-background-worker)) task-name &optional data)
  "Public function to enqueue a task."
  (assert (not (null +app+)))
  (arr:execute-task +app+ task-name data))

(defmethod arr:execute-task-at ((app (eql :global-background-worker)) time task-name &optional data)
  "Public function to enqueue a scheduled task."
  (assert (not (null +app+)))
  (arr:execute-task-at +app+ time task-name data))

(defun start-application (&key
                            (number-of-workers 1)
                            data-source)
  "Start the global state and thread."
  (assert (not (null data-source)))
  (when (< number-of-workers 1)
    (error "number of workers must be greater than 0."))
  (setf +app+ (arr.background-worker:start-application
               :number-of-workers number-of-workers
               :data-source data-source))
  t)

(defun stop-application ()
  "Stop the global state and thread."
  (when +app+
    (arr.background-worker:stop-application +app+)
    (setf +app+ nil)
    t))
