(defpackage #:arr.background-worker
  (:use #:cl)
  (:export
   #:stop-application
   #:start-application))

(in-package :arr.background-worker)

(defparameter +runner+ nil
  "Global instance of the task-runner.")

(defstruct background-worker
  (lock nil :type sb-thread:mutex)
  (queue nil :type sb-concurrency:queue)
  (mailbox nil :type sb-concurrency:mailbox)
  (running t :type boolean)
  (main-worker nil :type (or bt2:thread null))
  (workers-pool nil :type list)
  (scheduled-tasks-queue nil :type list))

(defun task-runner-running-p (state)
  "Check if the current task-runner STATE is running."
  (not (null (background-worker-running state))))

(defmacro with-background-worker-lock (var &body body)
  `(sb-thread:with-mutex ((background-worker-lock ,var))
     ,@body))

(defmethod arr:schedule-task ((kind (eql :immediate))
                              scheduled-time
                              data
                              &key
                                app
                              &allow-other-keys)
  (sb-concurrency:enqueue
   (list scheduled-time scheduled-time data)
   (background-worker-queue app)))

(defmethod arr:schedule-task ((kind (eql :scheduled-tasks))
                              scheduled-time
                              data
                              &key
                                app
                              &allow-other-keys)
  (setf
   (background-worker-scheduled-tasks-queue app)
   (append
    (list (list scheduled-time (car data) (cdr data)))
    (background-worker-scheduled-tasks-queue app))))

(defmethod arr:task-runner ((app background-worker) &key &allow-other-keys)
  (labels ((thread-sleep () (sleep .1)))
    (loop
      (handler-case
          (loop for task = (sb-concurrency:dequeue (background-worker-queue app))
                while task
                do (destructuring-bind (scheduled-time time (kind &rest data))
                       task
                     (arr:task kind data :time time)))
        (t (err)
          (log:error "[arr:runner] ~A" err)))
      (thread-sleep))))

(defmethod arr:task-scheduler ((app background-worker) &key &allow-other-keys)
  "Private scheduler function to manage tasks.

 It controls time and schedule tasks to be executed.

 Task can be:

   - (:immediate data) - goes to the immediate queue.
   - (:scheduled-tasks time data) - goes to the scheduled queue.

 `data` is an implementation of the generic `arr:task`."
  (labels ((thread-sleep () (sleep .1))
           (schedule (app current-time)
             (let ((m (background-worker-mailbox app)))
               (loop for task in (sb-concurrency:receive-pending-messages m)
                     do (arr:schedule-task (car task) current-time (cdr task)
                                           :app app))))
           (dispatch (app current-time)
             (setf
              (background-worker-scheduled-tasks-queue app)
              (reduce (lambda (acc task)
                        (if (> (- (second task) current-time) 0)
                            (append (list task) acc)
                            (prog1 acc
                              (sb-concurrency:enqueue
                               task
                               (background-worker-queue app)))))
                      (background-worker-scheduled-tasks-queue app)
                      :initial-value nil))))
    (let ((current-time (get-universal-time)))
      (loop
        (handler-case
            (progn
              (with-background-worker-lock app
                (when (background-worker-running app)
                  (schedule app current-time)
                  (dispatch app current-time))
                (setf current-time (get-universal-time)))
              (thread-sleep))
          (t (err)
            (log:error "[arr:scheduler] ~A" err)))))))

(defmethod arr:execute-task ((app (eql :background-worker)) task &optional data)
  "Public function to enqueue a task."
  (with-background-worker-lock +runner+
    (sb-concurrency:send-message
     (background-worker-mailbox +runner+)
     (list :immediate task data))))

(defmethod arr:execute-task-at ((app (eql :background-worker)) time task &optional data)
  "Public function to enqueue a scheduled task."
  (with-background-worker-lock +runner+
    (sb-concurrency:send-message
     (background-worker-mailbox +runner+)
     (list :scheduled-tasks time task data))))

(defun start-application (&key (number-of-workers 1))
  "Start the global state and thread."
  (if (< number-of-workers 1)
      (error "number of workers must be greater than 0."))
  (when (not +runner+)
    (setf +runner+
          (make-background-worker
           :lock (sb-thread:make-mutex)
           :mailbox (sb-concurrency:make-mailbox :name "task-runner-main-thread-mailbox")
           :queue (sb-concurrency:make-queue :name "task-runner-queue"))
          (background-worker-main-worker +runner+)
          (bt2:make-thread (lambda () (funcall #'arr:task-scheduler +runner+))
                           :name "arr-scheduler-thread")
          (background-worker-workers-pool +runner+)
          (loop for x upto number-of-workers from 1
                collect (bt2:make-thread (lambda () (funcall #'arr:task-runner +runner+))
                                         :name (format nil "arr-runner-thread-~a" x))))
    t))

(defun stop-application ()
  "Stop the global state and thread."
  (when +runner+
    (sb-thread:with-mutex ((background-worker-lock +runner+))
      (setf (background-worker-running +runner+) nil)
      (loop for runner in (background-worker-workers-pool +runner+)
            do (bt2:destroy-thread runner))
      (bt2:destroy-thread (background-worker-main-worker +runner+))
      (setf +runner+ nil))
    t))
