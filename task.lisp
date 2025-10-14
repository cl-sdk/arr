(in-package :arr)

(defstruct task-runner-state
  (lock nil :type sb-thread:mutex)
  (queue nil :type sb-concurrency:queue)
  (mailbox nil :type sb-concurrency:mailbox)
  (running t :type boolean)
  (main-worker nil :type (or bt2:thread null))
  (worker nil :type (or bt2:thread null))
  (scheduled-tasks-queue nil :type list))

(defun task-runner-running-p (state)
  "Check if the current task-runner STATE is running."
  (not (null (task-runner-state-running state))))

(defmacro with-task-runner-state-lock (var &body body)
  `(sb-thread:with-mutex ((task-runner-state-lock ,var))
     ,@body))

(defun execute-task (kind &optional data)
  "Public function to enqueue a task."
  (with-task-runner-state-lock +runner+
    (sb-concurrency:send-message
     (task-runner-state-mailbox +runner+)
     (list :immediate kind data))))

(defun execute-task-at (time kind &optional data)
  "Public function to enqueue a scheduled task."
  (with-task-runner-state-lock +runner+
    (sb-concurrency:send-message
     (task-runner-state-mailbox +runner+)
     (list :scheduled-tasks time kind data))))

(defgeneric task-execute (kind data &key time &allow-other-keys)
  (:documentation "Implementation of a task."))
