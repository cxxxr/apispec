#|
Usage:
  $ cd examples/
  $ clackup -s apispec -s ningle ningle/app.lisp
|#

(defpackage #:apispec/examples/ningle/app
  (:use #:cl)
  (:import-from #:apispec
                #:find-route
                #:spec-router
                #:validate-request
                #:validate-response
                #:request-path-parameters)
  (:import-from #:ningle
                #:*request*
                #:*response*)
  (:import-from #:lack.component
                #:call)
  (:import-from #:lack.response
                #:response-body
                #:response-headers
                #:response-status)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:*app*))
(in-package #:apispec/examples/ningle/app)

(defclass web (ningle:app)
  ((spec :initarg :spec)))

(defvar *operation*)

(defmethod ningle:make-request ((app web) env)
  (if *operation*
      (apispec:validate-request *operation* env
                                :path-parameters (getf env :apispec.path-parameters))
      (call-next-method)))

(defmethod ningle:process-response ((app web) result)
  (if *operation*
      (progn
        (when result
          (setf (response-body *response*) result))
        (call-next-method app
                          (apispec:validate-response *operation* *response*)))
      (call-next-method)))

(defmethod call :around ((app web) env)
  (multiple-value-bind (*operation* path-parameters)
      (apispec:find-route (spec-router (slot-value app 'spec))
                          (getf env :request-method)
                          (getf env :path-info))
    (call-next-method app
                      (append (list :apispec.path-parameters path-parameters)
                              env))))

(defparameter *app*
  (make-instance 'web
                 :spec (apispec:load-from-file #P"api.yaml")))

(defvar *db* '())
(defvar *id* 0)

(defclass user ()
  ((id :initform (incf *id*)
       :reader user-id)
   (name :initarg :name
         :reader user-name)
   (is-admin :initarg :is-admin
             :initform nil
             :reader user-admin-p)))

(defmethod apispec:encode-object ((user user))
  `(("id" . ,(user-id user))
    ("name" . ,(user-name user))
    ("is_admin" . ,(user-admin-p user))))

(setf (ningle:route *app* "/users" :method :GET)
      (lambda (params)
        (let ((name (aget params "name")))
          (setf (response-headers *response*)
                '(:content-type "application/json"))
          (if name
              (remove-if-not
                (lambda (user)
                  (search name (user-name user)))
                *db*)
              *db*))))

(setf (ningle:route *app* "/users" :method :POST)
      (lambda (params)
        (let ((new-user
                (make-instance 'user
                               :name (aget params "name")
                               :is-admin (aget params "is_admin"))))
          (push new-user *db*)
          (setf (response-status *response*) 204)
          (values))))

(setf (ningle:route *app* "/users/:id" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (let* ((id (aget (request-path-parameters *request*) "id"))
               (user (find id *db* :key #'user-id)))
          (or user
              (progn
                (setf (response-status *response*) 404)
                '(("error" . "User not found")))))))

*app*
