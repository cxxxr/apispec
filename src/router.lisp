(defpackage #:apispec/router
  (:use #:cl)
  (:import-from #:apispec/classes/path
                #:paths
                #:compile-paths
                #:find-operation)
  (:export #:router
           #:make-router
           #:find-route))
(in-package #:apispec/router)

(defstruct (router (:constructor %make-router))
  paths
  %dispatch-fn)

(defun make-router (paths)
  (%make-router :paths paths
                :%dispatch-fn (compile-paths paths)))

(defun find-path-item (router path-info)
  (funcall (router-%dispatch-fn router)
           path-info))

(defun find-route (router method path-info)
  (multiple-value-bind (matched-path path-parameters)
      (find-path-item router path-info)
    (when matched-path
      (values (find-operation matched-path method)
              path-parameters))))
