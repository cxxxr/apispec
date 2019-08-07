(defpackage #:apispec/operation
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/request
                #:parameter
                #:request-body)
  (:import-from #:apispec/response
                #:responses)
  (:export #:operation
           #:operation-tags
           #:operation-summary
           #:operation-description
           #:operation-id
           #:operation-parameters
           #:operation-request-body
           #:operation-responses
           #:operation-deprecated-p))
(in-package #:apispec/operation)

(declaim-safety)

;; TODO: 'externalDocs', 'callbacks', 'security' and 'servers'.
(defclass operation ()
  ((tags :type (proper-list string)
         :initarg :tags
         :initform nil
         :reader operation-tags)
   (summary :type (or string null)
            :initarg :summary
            :initform nil
            :reader operation-summary)
   (description :type (or string null)
                :initarg :description
                :initform nil
                :reader operation-description)
   (id :type (or string null)
       :initarg :id
       :initform nil
       :reader operation-id)
   (parameters :type (proper-list parameter)
               :initarg :parameters
               :initform nil
               :reader operation-parameters)
   (request-body :type (or request-body null)
                 :initarg :request-body
                 :initform nil
                 :reader operation-request-body)
   (responses :type responses
              :initarg :responses
              :initform (error ":responses is required for OPERATION")
              :reader operation-responses)
   (deprecated :type boolean
               :initarg :deprecated
               :initform nil
               :reader operation-deprecated-p)))

(undeclaim-safety)
