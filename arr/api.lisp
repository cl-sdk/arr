(in-package :arr)

(defgeneric task (task-name task-data &key time &allow-other-keys)
  (:documentation
   "Defines how a specific task identified by TASK-NAME is executed.
Each method specialized on TASK-NAME implements the behavior of that task,
using TASK-DATA as input. The optional TIME argument may indicate when the
task is being executed or provide scheduling context.

Arguments:
  TASK-NAME — keyword identifying the task (e.g. :send-email, :cleanup-cache).
  TASK-DATA — arbitrary data required for executing the task.
  :TIME — optional time context for the execution (creation, scheduling, or run time).

Returns:
  The result of executing the task, which depends on the implementation."))

(defgeneric schedule-task (data-source scheduled-time task &key app &allow-other-keys)
  (:documentation
   "Schedules TASK to be executed at SCHEDULED-TIME and stores it in DATA-SOURCE.
DATA-SOURCE is a storage system maintaining all scheduled tasks. APP is an optional
application context providing configuration or logging.

Arguments:
  DATA-SOURCE — task storage or queue system.
  SCHEDULED-TIME — time when the task should execute.
  TASK — task object as created by `task`.
  :APP — optional application context."))

(defgeneric enqueue-task (data-source task &key app &allow-other-keys)
  (:documentation
   "Adds TASK to the task queue within DATA-SOURCE.
Used for immediate or deferred execution depending on system policy.

Arguments:
  DATA-SOURCE — storage or queue manager.
  TASK — task object to enqueue.
  :APP — optional application context."))

(defgeneric dequeue-task (data-source &key app &allow-other-keys)
  (:documentation
   "Removes and returns the next available task from DATA-SOURCE’s queue.
If no task is available, returns NIL.

Arguments:
  DATA-SOURCE — queue or storage system holding pending tasks.
  :APP — optional application context.

Returns:
  A task or NIL when empty."))

(defgeneric task-runner (app &key &allow-other-keys)
  (:documentation
   "Main loop responsible for executing tasks for APP.
 The task-runner continuously (or periodically) dequeues pending tasks and
 executes them via `task` when they are read to be executed. It may serve
 as the core dispatcher, background worker, or event-driven processor.

 Arguments:
   APP — application context holding configuration and data source access."))

(defgeneric task-scheduler (app &key &allow-other-keys)
  (:documentation
   "Coordinates task scheduling for APP.
 Used to determine when and how a task is enqueued over time.

 Arguments:
   APP — application context with access to configuration and data source."))

(defgeneric execute-task (app task-name &optional data)
  (:documentation
   "Enqueue in application APP a specific task identified
 by TASK-NAME immediately.
 Dispatches to the appropriate method based on the task type.

Arguments:
  APP — application context.
  TASK-NAME — keyword identifying the task.
  DATA — optional data payload for task execution."))

(defgeneric execute-task-at (app time task-name &optional data)
  (:documentation
   "Enqueue in application APP a specific task identified
 by TASK-NAME at the specified TIME.
If TIME is in the future, the task may be scheduled for later execution;
if TIME is now or in the past, it is executed immediately.

Arguments:
  APP — application context.
  TIME — when to execute the task.
  TASK-NAME — keyword name of the task.
  DATA — optional payload for execution."))
