(in-package :arr)

(defstruct task-runner-state
  (lock nil :type sb-thread:mutex)
  (mailbox nil :type sb-concurrency:mailbox)
  (running t :type boolean)
  (main-worker nil :type (or bt2:thread null))
  (workers nil :type list))

(defun task-runner-running-p (state)
  (not (null (task-runner-state-running state))))

(defmacro with-task-runner-state-lock (var &body body)
  `(sb-thread:with-mutex ((task-runner-state-lock ,var))
     ,@body))

(defun execute-task (kind &optional data)
  (with-task-runner-state-lock +runner+
    (sb-concurrency:send-message
     (task-runner-state-mailbox +runner+)
     (list :immediate kind data))))

(defun execute-task-at (kind time &optional data)
  (with-task-runner-state-lock +runner+
    (sb-concurrency:send-message
     (task-runner-state-mailbox +runner+)
     (list :delayed time kind data))))

(defgeneric task-execute (kind data &key time &allow-other-keys)
  (:documentation "Tasks can be both a function (to be implemented)
 or an implementation of this generic."))
