(in-package :arr)

(defun %runner (state)
  "Main function of the thread runner.
 It controls time and manage when to execute tasks.
 Task can be:
   - (:start) - start receiving the execution
   - (:stop) - stop the execution
   - (:immediate data) - goes to the immediate queue.
   - (:delayed time data) - goes to the scheduled queue."
  (labels ((thread-sleep () (sleep .1) (bt2:thread-yield)))
    (let ((current-time (local-time:now)))
      (loop
        (handler-case
            (progn
              (with-task-runner-state-lock state
                (if (not (task-runner-state-running state))
                    (thread-sleep)))
              (let ((m (task-runner-state-mailbox state)))
                (if (= 0 (sb-concurrency:mailbox-count m))
                    (thread-sleep)
                    (let ((command-msg (sb-concurrency:receive-message m)))
                      (destructuring-bind (kind &rest data)
                          command-msg
                        (case kind
                          (:immediate (task-execute (car data) (cdr data) :time current-time))
                          (:delayed (error "to be implemented.")))))))
              (setf current-time (local-time:now)))
          (t (err)
            (log:error "[arr] ~A" err)))))))

(defparameter +runner+ nil)

(defun start-thread ()
  "Start the global state and thread."
  (when (not +runner+)
    (setf +runner+
          (make-task-runner-state
           :lock (sb-thread:make-mutex)
           :mailbox (sb-concurrency:make-mailbox :name "task-runner-main-thread-mailbox")
           :main-worker nil)
          (task-runner-state-main-worker +runner+)
          (bt2:make-thread (lambda () (%runner +runner+))))
    t))

(defun stop-thread ()
  "Stop the global state and thread."
  (when +runner+
    (sb-thread:with-mutex ((task-runner-state-lock +runner+))
      (setf (task-runner-state-running +runner+) nil)
      (bt2:destroy-thread (task-runner-state-main-worker +runner+))
      (setf +runner+ nil))
    t))
