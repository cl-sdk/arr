(in-package :arr)

(defparameter +runner+ nil
  "Global instance of the task-runner.")

(defun %runner (state)
  "Private runner worker function to execute tasks.

 Tasks are in the form (time kind &rest data)"
  (labels ((thread-sleep () (sleep .1)))
    (loop
      (handler-case
          (loop for task = (sb-concurrency:dequeue (task-runner-state-queue state))
                while task
                do (destructuring-bind (time kind data)
                       task
                     (task-execute kind data :time time)))
        (t (err)
          (log:error "[arr:runner] ~A" err)))
      (thread-sleep))))

(defun %scheduler (state)
  "Private scheduler function to manage tasks.

 It controls time and schedule tasks to be executed.

 Task can be:

   - (:immediate data) - goes to the immediate queue.
   - (:scheduled-tasks time data) - goes to the scheduled queue.

 `data` is an implementation of the generic `arr:task-execute`."
  (labels ((thread-sleep () (sleep .1))
           (process-message (state current-time command)
             (case (car command)
               (:immediate (sb-concurrency:enqueue
                            (list current-time (cadr command) (cddr command))
                            (task-runner-state-queue state)))
               (:scheduled-tasks (setf
                                  (task-runner-state-scheduled-tasks-queue state)
                                  (append
                                   (list (cdr command))
                                   (task-runner-state-scheduled-tasks-queue state)))))))
    (let ((current-time (get-universal-time)))
      (loop
        (handler-case
            (progn
              (with-task-runner-state-lock state
                (when (task-runner-state-running state)
                  (let ((m (task-runner-state-mailbox state)))
                    (loop for task in (sb-concurrency:receive-pending-messages m)
                          do (process-message state current-time task)))
                  (setf
                   (task-runner-state-scheduled-tasks-queue state)
                   (reduce (lambda (acc task)
                             (if (> (- (car task) current-time) 0)
                                 (append (list task) acc)
                                 (prog1 acc
                                   (sb-concurrency:enqueue
                                    task
                                    (task-runner-state-queue state)))))
                           (task-runner-state-scheduled-tasks-queue state)
                           :initial-value nil)))
                (setf current-time (get-universal-time)))
              (thread-sleep))
          (t (err)
            (log:error "[arr:scheduler] ~A" err)))))))

(defun start-thread ()
  "Start the global state and thread."
  (when (not +runner+)
    (setf +runner+
          (make-task-runner-state
           :lock (sb-thread:make-mutex)
           :mailbox (sb-concurrency:make-mailbox :name "task-runner-main-thread-mailbox")
           :queue (sb-concurrency:make-queue :name "task-runner-queue"))
          (task-runner-state-main-worker +runner+)
          (bt2:make-thread (lambda () (%scheduler +runner+)))
          (task-runner-state-worker +runner+)
          (bt2:make-thread (lambda () (%runner +runner+))))
    t))

(defun stop-thread ()
  "Stop the global state and thread."
  (when +runner+
    (sb-thread:with-mutex ((task-runner-state-lock +runner+))
      (setf (task-runner-state-running +runner+) nil)
      (bt2:destroy-thread (task-runner-state-worker +runner+))
      (bt2:destroy-thread (task-runner-state-main-worker +runner+))
      (setf +runner+ nil))
    t))
