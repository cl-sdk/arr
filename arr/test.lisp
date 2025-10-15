(defpackage #:arr.test
  (:use #:cl))

(in-package :arr.test)

(5am:def-suite* arr.suite
  :description "arr tests.")

(defparameter *executed*
  (bt2:make-atomic-integer :value 0))

(defmethod arr:task ((kind (eql :testing)) data &key time &allow-other-keys)
  t)

(defmethod arr:task ((kind (eql :increment)) data &key time &allow-other-keys)
  (log:info ":increment at ~a and run at ~a~%" time (- time (get-universal-time)))
  (bt2:atomic-integer-incf *executed*))

(5am:def-test define-a-new-instance-to-execute ()
  (5am:is-true (arr:task :testing nil)))
