(defpackage #:apispec/classes/path
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/parameter
                #:parameter)
  (:import-from #:apispec/classes/operation
                #:operation)
  (:import-from #:apispec/utils/path-template
                #:compile-path-template)
  (:export #:path-item
           #:path-item-summary
           #:path-item-description
           #:path-item-parameters
           #:path-item-get
           #:path-item-put
           #:path-item-post
           #:path-item-delete
           #:path-item-options
           #:path-item-head
           #:path-item-patch
           #:path-item-trace
           #:paths
           #:find-operation
           #:compile-paths))
(in-package #:apispec/classes/path)

(declaim-safety)

(defclass path-item ()
  ((summary :type (or string null)
            :initarg :summary
            :initform nil
            :reader path-item-summary)
   (description :type (or string null)
                :initarg :description
                :initform nil
                :reader path-item-description)
   (parameters :type (proper-list parameter)
               :initarg :parameters
               :initform nil
               :reader path-item-parameters)
   (get :type (or operation null)
        :initarg :get
        :initform nil
        :accessor path-item-get)
   (put :type (or operation null)
        :initarg :put
        :initform nil
        :accessor path-item-put)
   (post :type (or operation null)
         :initarg :post
         :initform nil
         :accessor path-item-post)
   (delete :type (or operation null)
           :initarg :delete
           :initform nil
           :accessor path-item-delete)
   (options :type (or operation null)
            :initarg :options
            :initform nil
            :accessor path-item-options)
   (head :type (or operation null)
         :initarg :head
         :initform nil
         :accessor path-item-head)
   (patch :type (or operation null)
          :initarg :patch
          :initform nil
          :accessor path-item-patch)
   (trace :type (or operation null)
          :initarg :trace
          :initform nil
          :accessor path-item-trace)))

(defun find-operation (path method)
  (check-type path path-item)
  (funcall
    (ecase method
      (:get #'path-item-get)
      (:put #'path-item-put)
      (:post #'path-item-post)
      (:delete #'path-item-delete)
      (:options #'path-item-options)
      (:head #'path-item-head)
      (:patch #'path-item-patch)
      (:trace #'path-item-trace))
    path))

(deftype paths ()
  '(association-list string path-item))

(defun compile-paths (paths)
  (assert (typep paths 'paths))
  (loop for (template . path) in paths
        collect (multiple-value-bind (matcher score template-regexp)
                    (compile-path-template template path)
                  (list score (length template-regexp) matcher))
        into matchers
        finally
        (return
          (let ((matchers (mapcar #'third (sort matchers
                                                (lambda (a b)
                                                  (or (< (first a) (first b))
                                                      (and (= (first a) (first b))
                                                           (>= (second a) (second b)))))))))
            (lambda (path-info)
              (loop for matcher in matchers
                    do (multiple-value-bind (matched-path parameters)
                           (funcall matcher path-info)
                         (when matched-path
                           (return (values matched-path parameters))))))))))

(undeclaim-safety)
