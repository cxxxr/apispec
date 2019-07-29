(defpackage #:apispec/operation/request-body
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/operation/media-type
                #:media-type)
  (:export #:request-body
           #:request-body-description
           #:request-body-content
           #:request-body-required-p))
(in-package #:apispec/operation/request-body)

(declaim-safety)

(defclass request-body ()
  ((description :type (or string null)
                :initarg :description
                :initform nil
                :reader request-body-description)
   (content :type (and (association-list string media-type)
                       (not null))
            :initarg :content
            :initform (error ":content is required for REQUEST-BODY")
            :reader request-body-content)
   (required :type boolean
             :initarg :required
             :initform nil
             :reader request-body-required-p)))

(undeclaim-safety)
