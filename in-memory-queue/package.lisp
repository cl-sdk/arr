(defpackage #:arr.in-memory-queue
  (:use #:cl)
  (:export
   #:in-memory-queue))

(in-package :arr.in-memory-queue)

(defclass in-memory-queue (arr:queue-data-source)
  ((queue :initarg :queue :type sb-concurrency:queue)))

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
  (arr:enqueue-task data-source
                    (cons scheduled-time data)
                    :app app)
  t)
