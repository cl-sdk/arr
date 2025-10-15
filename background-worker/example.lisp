(defpackage #:arr.background-worker.example
  (:use #:cl))

(in-package :arr.background-worker.example)

(arr.background-worker:start-application)

(defvar +database-path+ "./background-worker-todo-example.db")
(defvar +connection+ (sqlite:connect +database-path+ :busy-timeout 2000))

(sqlite:execute-single +connection+ "CREATE TABLE IF NOT EXISTS todo (id text, title text)")

(defmethod arr:task ((kind (eql :create-todo)) data &key time &allow-other-keys)
  (destructuring-bind (id title)
      (car data)
    (sqlite:execute-single +connection+ "INSERT INTO todo (id, title) values (?, ?)" id title)))

(defmethod arr:task ((kind (eql :delete-todo)) data &key time &allow-other-keys)
  (sqlite:execute-single +connection+ "DELETE FROM todo where id = ?" (caar data)))

(defun create-todo (env)
  (let ((title (flexi-streams:octets-to-string
                (flex::vector-stream-vector (getf env :raw-body))
                :external-format :utf-8))
        (id (fuuid:make-v4-string)))
    (log:info title)
    (arr:execute-task :background-worker :create-todo (list id title))
    id))

(defun delete-todo (env)
  (let ((id (flexi-streams:octets-to-string
             (flex::vector-stream-vector (getf env :raw-body))
             :external-format :utf-8)))
    (log:info id)
    (arr:execute-task-at :background-worker (+ (get-universal-time) 5) :delete-todo (list id))
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
                     (:GET (funcall #'get-all-todos env)))
                   )))
    (list 200 '(:content-type "text/plain") (list res))))

(defparameter +web-server-thread+ nil)
(defparameter +woo-web-server+ nil)

(defun ensure-web-server-running ()
  (let ((thread +web-server-thread+))
    (setf +web-server-thread+ nil)
    (when +woo-web-server+
      (woo:stop +woo-web-server+))
    (when thread
      (bt2:destroy-thread thread)))
  (setf +web-server-thread+
        (bt2:make-thread (lambda ()
                           (setf +woo-web-server+ (woo:run #'web-server))))))

(ensure-web-server-running)
