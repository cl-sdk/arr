(in-package :arr)

(defgeneric task (kind data &key time &allow-other-keys)
  (:documentation "Defines the implementation of a task to be executed.

`kind` identifies the type of task (e.g., :email, :cleanup, etc.).
`data` contains task-specific parameters.
`time` represents the execution time if provided."))

(defgeneric schedule-task (kind scheduled-time data &key app &allow-other-keys)
  (:documentation "Schedules a task to be executed at a specific time.

`kind` identifies the task type.
`scheduled-time` specifies when the task should run.
`data` provides the task payload or arguments.
`state` is an optional context object used by the scheduler.

Implement this method to define custom scheduling behavior or
to integrate with external systems (e.g., queues, cron, or message brokers)."))

(defgeneric task-runner (app &key &allow-other-keys)
  (:documentation "Internal worker function responsible for executing tasks.

This function processes tasks in the form:

  (scheduled-time time kind &rest data)

It should be specialized to perform the actual task execution
loop or worker logic appropriate for your system."))

(defgeneric task-scheduler (app &key &allow-other-keys)
  (:documentation "Internal scheduler function responsible for managing task timing and dispatch.

It controls when tasks are moved from the scheduled queue to
the execution queue.

Implement this method to define how scheduling and timing are
handled (e.g., polling, event-driven, or hybrid approaches)."))

(defgeneric execute-task (app task &optional data)
  (:documentation "Public function to enqueue a task."))

(defgeneric execute-task-at (app time task &optional data)
  (:documentation "Public function to enqueue a scheduled task."))
