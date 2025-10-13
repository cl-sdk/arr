(defpackage #:arr.test
  (:use #:cl))

(in-package :arr.test)

(5am:def-suite* arr.suite
  :description "arr tests.")

(defmethod arr:task-execute ((kind (eql :testing)) data &key time &allow-other-keys)
  t)

(defparameter *enqueued* nil)

(defmethod arr:task-execute ((kind (eql :enqueue)) data &key time &allow-other-keys)
  (setf *enqueued* data))

(defmethod arr:task-execute ((kind (eql :raise-condition)) data &key time &allow-other-keys)
  (error "condition raised"))

(5am:def-test define-a-new-instance-to-execute ()
  (5am:is-true (arr:task-execute :testing nil)))

(5am:def-test enqueue-a-task-to-be-executed-immediately ()
  (5am:is-true (not *enqueued*))
  (arr:start-thread)
  (arr:execute-task :enqueue t)
  (sleep 1)
  (5am:is-true *enqueued*)
  (arr:stop-thread)
  (setf *enqueued* nil))

(5am:def-test should-not-kill-the-scheduler-thread-on-condition ()
  (5am:is-true (not *enqueued*))
  (arr:start-thread)
  (arr:execute-task :raise-condition)
  (sleep 1)
  (5am:is-true (not *enqueued*))
  (arr:execute-task :enqueue t)
  (sleep 1)
  (5am:is-true *enqueued*)
  (arr:stop-thread)
  (setf *enqueued* nil))
