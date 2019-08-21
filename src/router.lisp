(defpackage #:apispec/router
  (:use #:cl)
  (:import-from #:apispec/classes/path
                #:paths
                #:compile-paths
                #:find-operation)
  (:export #:find-route))
(in-package #:apispec/router)

(defun find-route (paths method path-info)
  (let ((dispatcher (compile-paths paths)))
    (multiple-value-bind (matched-path path-parameters)
        (funcall dispatcher path-info)
      (when matched-path
        (values (find-operation matched-path method)
                path-parameters)))))
