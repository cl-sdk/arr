(pushnew *default-pathname-defaults* ql:*local-project-directories*)

(ql:quickload '(:arr :cl-redis))

(arr:start-thread)
(arr:stop-thread)

(defmethod arr:task-execute ((kind (eql :print)) data &key time &allow-other-keys)
  (log:info data time))

(defmethod arr:task-execute ((kind (eql :raise-condition)) data &key time &allow-other-keys)
  (error "die thread"))

(arr:execute-task :print "ok")

(arr:execute-task :raise-condition nil)
