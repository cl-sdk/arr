(defpackage #:arr.test
  (:use #:cl))

(in-package :arr.test)

(5am:def-suite* arr.suite
  :description "arr tests.")

(defmethod arr:task-execute ((kind (eql :testing)) data &key time &allow-other-keys)
  (declare (ignorable data time))
  t)

(defparameter *enqueued* nil)

(defmethod arr:task-execute ((kind (eql :enqueue)) data &key time &allow-other-keys)
  (declare (ignorable time))
  (setf *enqueued* data))

(defmethod arr:task-execute ((kind (eql :raise-condition)) data &key time &allow-other-keys)
  (declare (ignorable data time))
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

(5am:def-test schedule-a-task-for-2-seconds ()
  (5am:is-true (not *enqueued*))
  (arr:start-thread)
  (arr:execute-task-at (+ (get-universal-time) 2)
                       :enqueue t)
  (sleep 1)
  (5am:is-true (not *enqueued*))
  (sleep 2)
  (5am:is-true *enqueued*)
  (setf *enqueued* nil)
  (arr:stop-thread))

(defparameter *incr* 0)

(defmethod arr:task-execute ((kind (eql :increment)) data &key &allow-other-keys)
  (incf *incr*))

(5am:def-test schedule-two-tasks-for-2-seconds-and-4-seconds ()
  (5am:is-true (= 0 *incr*))
  (arr:start-thread)
  (arr:execute-task-at (+ (get-universal-time) 2)
                       :increment)
  (arr:execute-task-at (+ (get-universal-time) 4)
                       :increment)
  (5am:is-true (= 0 *incr*))
  (sleep 3)
  (5am:is-true (= 1 *incr*))
  (sleep 4)
  (5am:is-true (= 2 *incr*))
  (setf *incr* 0)
  (arr:stop-thread))
