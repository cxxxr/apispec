(defpackage #:apispec/classes/response/class
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/header
                #:header)
  (:import-from #:apispec/classes/media-type
                #:media-type)
  (:import-from #:cl-ppcre)
  (:export #:response
           #:response-description
           #:response-headers
           #:response-content
           #:responses))
(in-package #:apispec/classes/response/class)

(declaim-safety)

(defclass response ()
  ((description :type string
                :initarg :description
                :initform (error ":description is required for RESPONSE")
                :reader response-description)
   (headers :type (association-list string header)
            :initarg :headers
            :initform nil
            :reader response-headers)
   (content :type (association-list string (or media-type null))
            :initarg :content
            :initform (error ":content is required for RESPONSE")
            :reader response-content)))

(defun http-status-code-p (value)
  (and (ppcre:scan "[1-5](?:\\d\\d|XX)" value)
       t))

(deftype http-status-code ()
  '(satisfies http-status-code-p))

(defun default-response-p (value)
  (equal value "default"))

(deftype responses-keys ()
  '(and string
        (or http-status-code
            (satisfies default-response-p))))

(defun responsesp (responses)
  (and (not (null responses))
       (association-list-p responses 'responses-keys 'response)))

(deftype responses ()
  '(satisfies responsesp))

(undeclaim-safety)
