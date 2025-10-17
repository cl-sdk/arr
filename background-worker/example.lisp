(defpackage #:arr.background-worker.example
  (:use #:cl))

(in-package :arr.background-worker.example)

(defparameter +web-server-thread+ nil)
(defparameter +woo-web-server+ nil)

(defvar +database-path+ "./background-worker-todo-example.db")

(defvar +connection+ (sqlite:connect +database-path+ :busy-timeout 2000))

(sqlite:execute-single +connection+ "CREATE TABLE IF NOT EXISTS todo (id text, title text)")

(defun get-request-content (env)
  (flexi-streams:octets-to-string
   (flex::vector-stream-vector (getf env :raw-body))
   :external-format :utf-8))

(defmethod arr:task ((kind (eql :create-todo)) task &key time &allow-other-keys)
  (destructuring-bind (id title)
      (car task)
    (sqlite:execute-single +connection+ "INSERT INTO todo (id, title) values (?, ?)" id title)))

(defmethod arr:task ((kind (eql :delete-todo)) task &key time &allow-other-keys)
  (sqlite:execute-single +connection+ "DELETE FROM todo where id = ?" (caar task)))

(defun create-todo (env)
  (let ((title (get-request-content env))
        (id (fuuid:make-v4-string)))
    (arr:execute-task :global-background-worker :create-todo (list id title))
    id))

(defun delete-todo (env)
  (let ((id (get-request-content env)))
    (arr:execute-task-at :global-background-worker (+ (get-universal-time) 5) :delete-todo (list id))
    id))

(defun get-all-todos (env)
  (declare (ignorable env))
  (let ((todos (sqlite:execute-to-list +connection+ "SELECT * FROM todo"))
        (res ""))
    (dolist (todo todos)
      (setf res (concatenate 'string
                             res
                             (format nil "id=~a;title=~a~%"
                                     (first todo)
                                     (second todo)))))
    res))

(defun web-server (env)
  (let ((res (progn
               (case (getf env :request-method)
                 (:POST (funcall #'create-todo env))
                 (:DELETE (funcall #'delete-todo env))
                 (:GET (funcall #'get-all-todos env))))))
    (list 200 '(:content-type "text/plain") (list res))))

(defun start ()
  (arr.global-background-worker:start-application
   :number-of-workers 8
   :data-source (make-instance 'arr.in-memory-queue:in-memory-queue
                               :queue (sb-concurrency:make-queue :name "example-queue")))
  (setf +web-server-thread+
        (bt2:make-thread (lambda ()
                           (setf +woo-web-server+ (woo:run #'web-server))))))

(defun stop ()
  (arr.global-background-worker:stop-application)
  (let ((thread +web-server-thread+))
    (setf +web-server-thread+ nil)
    (when +woo-web-server+
      (woo:stop +woo-web-server+))
    (when thread
      (bt2:destroy-thread thread))))

(defun ensure-web-server-running ()
  (stop)
  (start))

(ensure-web-server-running)
