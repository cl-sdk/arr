(in-package :arr)

(defgeneric task (kind data &key time &allow-other-keys)
  (:documentation "Implementation of a task."))

(defgeneric schedule-task (kind scheduled-time data &key state &allow-other-keys)
  (:documentation "Implement this function to create a new way
 to schedule a task."))

(defgeneric task-runner (state &key &allow-other-keys)
  (:documentation "Private runner worker function to execute tasks.
 Tasks are in the form (scheduled-time time kind &rest data)"))

(defgeneric task-scheduler (state &key &allow-other-keys)
  (:documentation "Private scheduler function to manage tasks.

 It controls time and schedule tasks to be executed.

 Task can be:

   - (:immediate data) - goes to the immediate queue.
   - (:scheduled-tasks time data) - goes to the scheduled queue.

 `data` is an implementation of the generic `arr:task-execute`."))
