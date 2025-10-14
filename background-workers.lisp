(in-package :arr)

(defparameter +runner+ nil
  "Global instance of the task-runner.")

(defmethod task-runner ((state task-runner-state) &key &allow-other-keys)
  (labels ((thread-sleep () (sleep .1)))
    (loop
      (handler-case
          (loop for task = (sb-concurrency:dequeue (task-runner-state-queue state))
                while task
                do (destructuring-bind (scheduled-time time (kind &rest data))
                       task
                     (task kind data :time time)))
        (t (err)
          (log:error "[arr:runner] ~A" err)))
      (thread-sleep))))

(defmethod schedule-task ((kind (eql :immediate))
                          scheduled-time
                          data
                          &key
                            state
                          &allow-other-keys)
  (sb-concurrency:enqueue
   (list scheduled-time scheduled-time data)
   (task-runner-state-queue state)))

(defmethod schedule-task ((kind (eql :scheduled-tasks))
                          scheduled-time
                          data
                          &key
                            state
                          &allow-other-keys)
  (setf
   (task-runner-state-scheduled-tasks-queue state)
   (append
    (list (list scheduled-time (car data) (cdr data)))
    (task-runner-state-scheduled-tasks-queue state))))

(defmethod task-scheduler ((state task-runner-state) &key &allow-other-keys)
  "Private scheduler function to manage tasks.

 It controls time and schedule tasks to be executed.

 Task can be:

   - (:immediate data) - goes to the immediate queue.
   - (:scheduled-tasks time data) - goes to the scheduled queue.

 `data` is an implementation of the generic `arr:task`."
  (labels ((thread-sleep () (sleep .1))
           (schedule (state current-time)
             (let ((m (task-runner-state-mailbox state)))
               (loop for task in (sb-concurrency:receive-pending-messages m)
                     do (schedule-task (car task) current-time (cdr task)
                                       :state state))))
           (dispatch (state current-time)
             (setf
              (task-runner-state-scheduled-tasks-queue state)
              (reduce (lambda (acc task)
                        (if (> (- (second task) current-time) 0)
                            (append (list task) acc)
                            (prog1 acc
                              (sb-concurrency:enqueue
                               task
                               (task-runner-state-queue state)))))
                      (task-runner-state-scheduled-tasks-queue state)
                      :initial-value nil))))
    (let ((current-time (get-universal-time)))
      (loop
        (handler-case
            (progn
              (with-task-runner-state-lock state
                (when (task-runner-state-running state)
                  (schedule state current-time)
                  (dispatch state current-time))
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
          (bt2:make-thread (lambda () (funcall #'task-scheduler +runner+)))
          (task-runner-state-worker +runner+)
          (bt2:make-thread (lambda () (funcall #'task-runner +runner+))))
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
