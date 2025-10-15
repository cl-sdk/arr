(defpackage #:arr.background-worker.test
  (:use #:cl))

(in-package :arr.background-worker.test)

(5am:def-suite* arr.background-worker.suite
  :description "arr background worker tests.")

(defparameter *executed*
  (bt2:make-atomic-integer :value 0))

(defmethod arr:task ((kind (eql :increment)) data &key time &allow-other-keys)
  (log:info ":increment at ~a and run at ~a~%" time (- time (get-universal-time)))
  (bt2:atomic-integer-incf *executed*))

(defmethod arr:task ((kind (eql :raise-condition)) data &key time &allow-other-keys)
  (declare (ignorable data))
  (log:info ":raise-condition at ~a and run at ~a~%" time (- time (get-universal-time)))
  (error "condition raised"))

(5am:def-test enqueue-a-task-to-be-executed-immediately ()
  (5am:is-true (= 0 (bt2:atomic-integer-value *executed*)))
  (arr.background-worker:start-application)
  (arr:execute-task :background-worker :increment t)
  (sleep 1)
  (5am:is-true (= 1 (bt2:atomic-integer-value *executed*)))
  (arr.background-worker:stop-application)
  (setf *executed* (bt2:make-atomic-integer :value 0)))

(5am:def-test should-not-kill-the-scheduler-thread-on-condition ()
  (5am:is-true (= 0 (bt2:atomic-integer-value *executed*)))
  (arr.background-worker:start-application)
  (arr:execute-task :background-worker :raise-condition)
  (sleep 1)
  (5am:is-true (= 0 (bt2:atomic-integer-value *executed*)))
  (arr:execute-task :background-worker :increment t)
  (sleep 1)
  (5am:is-true (= 1 (bt2:atomic-integer-value *executed*)))
  (setf *executed* (bt2:make-atomic-integer :value 0))
  (arr.background-worker:stop-application))

(5am:def-test schedule-a-task-for-2-seconds ()
  (5am:is-true (= 0 (bt2:atomic-integer-value *executed*)))
  (arr.background-worker:start-application)
  (arr:execute-task-at :background-worker
                       (+ (get-universal-time) 2)
                       :increment t)
  (sleep 1)
  (5am:is-true (= 0 (bt2:atomic-integer-value *executed*)))
  (sleep 2)
  (5am:is-true (= 1 (bt2:atomic-integer-value *executed*)))
  (setf *executed* (bt2:make-atomic-integer :value 0))
  (arr.background-worker:stop-application))

(5am:def-test schedule-two-tasks-for-2-seconds-at-approximately-same-time ()
  (5am:is-true (= 0 (bt2:atomic-integer-value *executed*)))
  (arr.background-worker:start-application)
  (let ((time (+ (get-universal-time) 2)))
    (arr:execute-task-at :background-worker time :increment)
    (arr:execute-task-at :background-worker time :increment))
  (5am:is-true (= 0 (bt2:atomic-integer-value *executed*)))
  (sleep 3)
  (5am:is-true (= 2 (bt2:atomic-integer-value *executed*)))
  (setf *executed* (bt2:make-atomic-integer :value 0))
  (arr.background-worker:stop-application))

(5am:def-test schedule-two-tasks-for-2-seconds-and-4-seconds ()
  (5am:is-true (= 0 (bt2:atomic-integer-value *executed*)))
  (arr.background-worker:start-application)
  (arr:execute-task-at :background-worker
                       (+ (get-universal-time) 2)
                       :increment)
  (arr:execute-task-at :background-worker
                       (+ (get-universal-time) 4)
                       :increment)
  (5am:is-true (= 0 (bt2:atomic-integer-value *executed*)))
  (sleep 3)
  (5am:is-true (= 1 (bt2:atomic-integer-value *executed*)))
  (sleep 4)
  (5am:is-true (= 2 (bt2:atomic-integer-value *executed*)))
  (setf *executed* (bt2:make-atomic-integer :value 0))
  (arr.background-worker:stop-application))
