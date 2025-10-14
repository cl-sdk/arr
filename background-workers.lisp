(in-package :arr)

(defun %runner (state)
  "Main function of the thread runner.
 It controls time and manage when to execute tasks.
 Task can be:
   - (:start) - start receiving the execution
   - (:stop) - stop the execution
   - (:immediate data) - goes to the immediate queue.
   - (:scheduled-tasks time data) - goes to the scheduled queue."
  (labels ((thread-sleep () (sleep .1) (bt2:thread-yield))
           (process-message (state current-time command)
             (case (car command)
               (:immediate (task-execute (second command) (third command)
                                         :time current-time))
               (:scheduled-tasks (sb-concurrency:enqueue
                          (cdr command)
                          (task-runner-state-scheduled-tasks-queue state))))))
    (let ((current-time (get-universal-time)))
      (loop
        (handler-case
            (progn
              (with-task-runner-state-lock state
                (if (not (task-runner-state-running state))
                    (thread-sleep))
                (let ((m (task-runner-state-mailbox state)))
                  (if (= 0 (sb-concurrency:mailbox-count m))
                      (thread-sleep)
                      (process-message state
                                       current-time
                                       (sb-concurrency:receive-message m))))
                (let ((queue (task-runner-state-scheduled-tasks-queue state))
                      (next-queue))
                  (loop for task = (sb-concurrency:dequeue queue)
                        while task
                        do (let ((diff (- (car task) current-time)))
                             (if (<= diff 0)
                                 (task-execute (second task) (third task)
                                               :time current-time)
                                 (setf next-queue (pushnew task next-queue)))))
                  (loop for task in next-queue
                        do (sb-concurrency:enqueue task queue)))
                (setf current-time (get-universal-time))))
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
           :main-worker nil
           :scheduled-tasks-queue (sb-concurrency:make-queue :name "task-runner-scheduled-tasks-queue"))
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
