(defpackage #:arr.test
  (:use #:cl))

(in-package :arr.test)

(5am:def-suite* arr.suite
  :description "arr tests.")

(defparameter *test-lock*
  (bordeaux-threads-2:make-lock :name "test-lock"))

(defparameter *executed* 0)

(defmethod arr:task ((kind (eql :testing)) data &key time &allow-other-keys)
  t)

(defmethod arr:task ((kind (eql :increment)) data &key time &allow-other-keys)
  (log:info ":increment at ~a and run at ~a~%" time (- time (get-universal-time)))
  (bt2:with-lock-held (*test-lock*)
    (incf *executed*)))

(defmethod arr:task ((kind (eql :raise-condition)) data &key time &allow-other-keys)
  (declare (ignorable data))
  (log:info ":raise-condition at ~a and run at ~a~%" time (- time (get-universal-time)))
  (error "condition raised"))

(5am:def-test define-a-new-instance-to-execute ()
  (5am:is-true (arr:task :testing nil)))

(5am:def-test enqueue-a-task-to-be-executed-immediately ()
  (5am:is-true (= 0 *executed*))
  (arr:start-thread)
  (arr:execute-task :increment t)
  (sleep 1)
  (5am:is-true (= 1 *executed*))
  (arr:stop-thread)
  (setf *executed* 0))

(5am:def-test should-not-kill-the-scheduler-thread-on-condition ()
  (5am:is-true (= 0 *executed*))
  (arr:start-thread)
  (arr:execute-task :raise-condition)
  (sleep 1)
  (5am:is-true (= 0 *executed*))
  (arr:execute-task :increment t)
  (sleep 1)
  (5am:is-true (= 1 *executed*))
  (arr:stop-thread)
  (setf *executed* 0))

(5am:def-test schedule-a-task-for-2-seconds ()
  (5am:is-true (= 0 *executed*))
  (arr:start-thread)
  (arr:execute-task-at (+ (get-universal-time) 2)
                       :increment t)
  (sleep 1)
  (5am:is-true (= 0 *executed*))
  (sleep 2)
  (5am:is-true *executed*)
  (setf *executed* 0)
  (arr:stop-thread))

(5am:def-test schedule-two-tasks-for-2-seconds-and-4-seconds ()
   (5am:is-true (= 0 *executed*))
   (Arr:start-thread)
   (arr:execute-task-at (+ (get-universal-time) 2)
                        :increment)
   (arr:execute-task-at (+ (get-universal-time) 4)
                        :increment)
   (5am:is-true (= 0 *executed*))
   (sleep 3)
   (5am:is-true (= 1 *executed*))
   (sleep 4)
   (5am:is-true (= 2 *executed*))
   (setf *executed* 0)
   (arr:stop-thread))
