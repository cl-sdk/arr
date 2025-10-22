(defpackage #:arr.sqlite-queue
  (:use #:cl))

(in-package :arr.sqlite-queue)

(defun setup-queue (connection)
  (sqlite:execute-single connection
                         "CREATE TABLE IF NOT EXISTS queue (id serial, data text, created_at)"))

(defclass sqlite-queue (arr:queue-data-source)
  ((semaphore :initarg :semaphore :reader sqlite-queue-semaphore :type bt2:semaphore)
   (connection :initarg :connection :type sqlite:sqlite-handle)))

(defmethod arr:dequeue-task ((data-source sqlite-queue) &key &allow-other-keys)
  (bt2:wait-on-semaphore (sqlite-queue-semaphore data-source))
  (with-slots (queue)
      data-source
    (sqlite:execute-single +connection+ "INSERT INTO todo (id, title) values (?, ?)" id title))
  (bt2:signal-semaphore (sqlite-queue-semaphore data-source)))

(defmethod arr:enqueue-task ((data-source sqlite-queue) task &key app &allow-other-keys)
  (bt2:wait-on-semaphore (sqlite-queue-semaphore data-source))
  (with-slots (queue)
      data-source
    (sqlite:execute-single +connection+ "INSERT INTO todo (id, title) values (?, ?)" id title))
  (bt2:signal-semaphore (sqlite-queue-semaphore data-source)))

(defmethod arr:schedule-task ((data-source in-memory-queue)
                              scheduled-time
                              data
                              &key
                                app
                              &allow-other-keys)
  (arr:enqueue-task data-source
                    (cons scheduled-time data)
                    :app app)
  t)
