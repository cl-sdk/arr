(defpackage #:arr.background-worker
  (:use #:cl)
  (:export
   #:stop-application
   #:start-application))

(in-package :arr.background-worker)

(defclass queue-data-source () ())

(defclass in-memory-queue (queue-data-source)
  ((queue :initarg :queue :type sb-concurrency:queue)))

(defstruct background-worker
  (lock nil :type sb-thread:mutex)
  data-source
  (mailbox nil :type sb-concurrency:mailbox)
  (running t :type boolean)
  (main-worker nil :type (or bt2:thread null))
  (workers-pool nil :type list))

(defun task-runner-running-p (state)
  "Check if the current task-runner STATE is running."
  (not (null (background-worker-running state))))

(defmacro with-background-worker-lock (var &body body)
  `(sb-thread:with-mutex ((background-worker-lock ,var))
     ,@body))

(defmethod arr:dequeue-task ((data-source in-memory-queue) &key &allow-other-keys)
  (with-slots (queue)
      data-source
    (sb-concurrency:dequeue queue)))

(defmethod arr:enqueue-task ((data-source in-memory-queue) task &key app &allow-other-keys)
  (with-slots (queue)
      data-source
    (sb-concurrency:enqueue task queue)))

(defmethod arr:schedule-task ((data-source in-memory-queue)
                              scheduled-time
                              data
                              &key
                                app
                              &allow-other-keys)
  (arr:enqueue-task
   (background-worker-data-source app)
   (cons scheduled-time data)
   :app app)
  t)

(defmethod arr:task-runner ((app background-worker) &key &allow-other-keys)
  (labels ((thread-sleep () (sleep .1)))
    (loop
      (handler-case
          (let ((current-time (get-universal-time))
                (task (arr:dequeue-task (background-worker-data-source app))))
            (when task
              (if (> (- (second task) current-time) 0)
                  (arr:enqueue-task (background-worker-data-source app)
                                    task
                                    :app app)
                  (destructuring-bind (scheduled-time time kind &rest data)
                      task
                    (declare (ignorable scheduled-time))
                    (arr:task kind data :time time)))))
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
  (labels ((thread-sleep () (sleep .1)))
    (let ((current-time (get-universal-time)))
      (loop
        (handler-case
            (progn
              (with-background-worker-lock app
                (when (background-worker-running app)
                  (loop for task in (sb-concurrency:receive-pending-messages
                                     (background-worker-mailbox app))
                        do (arr:schedule-task (background-worker-data-source app)
                                              current-time
                                              task
                                              :app app)))
                (setf current-time (get-universal-time)))
              (thread-sleep))
          (t (err)
            (log:error "[arr:scheduler] ~A" err)))))))

(defmethod arr:execute-task ((app background-worker) task-name &optional data)
  "Public function to enqueue a task."
  (with-background-worker-lock app
    (sb-concurrency:send-message
     (background-worker-mailbox app)
     (list (get-universal-time) task-name data))))

(defmethod arr:execute-task-at ((app background-worker) time task-name &optional data)
  "Public function to enqueue a scheduled task."
  (with-background-worker-lock app
    (sb-concurrency:send-message
     (background-worker-mailbox app)
     (list time task-name data))))

(defun start-application (&key (number-of-workers 1))
  "Start the global state and thread."
  (when (< number-of-workers 1)
    (error "number of workers must be greater than 0."))
  (let ((app (make-background-worker
              :lock (sb-thread:make-mutex)
              :mailbox (sb-concurrency:make-mailbox :name "task-runner-main-thread-mailbox")
              :data-source (make-instance 'in-memory-queue
                                          :queue (sb-concurrency:make-queue :name "task-runner-queue")))))
    (setf (background-worker-main-worker app)
          (bt2:make-thread (lambda () (funcall #'arr:task-scheduler app))
                           :name "arr-scheduler-thread")
          (background-worker-workers-pool app)
          (loop for x upto number-of-workers from 1
                collect (bt2:make-thread (lambda () (funcall #'arr:task-runner app))
                                         :name (format nil "arr-runner-thread-~a" x))))
    app))

(defun stop-application (app)
  "Stop the global state and thread."
  (when app
    (sb-thread:with-mutex ((background-worker-lock app))
      (setf (background-worker-running app) nil)
      (loop for runner in (background-worker-workers-pool app)
            do (bt2:destroy-thread runner))
      (bt2:destroy-thread (background-worker-main-worker app))
      (setf app nil))
    t))
