(defpackage #:apispec/request/encoding/core
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/schema
                #:schema)
  (:import-from #:apispec/header
                #:header)
  (:export #:encoding
           #:encoding-content-type
           #:encoding-headers
           #:encoding-style
           #:encoding-explode-p
           #:encoding-allow-reserved-p
           #:media-type
           #:media-type-schema
           #:media-type-encoding))
(in-package #:apispec/request/encoding/core)

(declaim-safety)

(defun encoding-style-string-p (style)
  (and (member style '("matrix" "label" "form" "simple" "spaceDelimited" "pipeDelimited" "deepObject")
               :test #'equal)
       t))

(deftype encoding-style ()
  '(satisfies encoding-style-string-p))

(defclass encoding ()
  ((content-type :type (or string null)
                 :initarg :content-type
                 :initform nil
                 :reader encoding-content-type)
   (headers :type (association-list string header)
            :initarg :headers
            :initform nil
            :reader encoding-headers)
   (style :type encoding-style
          :initarg :style
          :initform "form"
          :reader encoding-style)
   (explode :type boolean
            :initarg :explode)
   (allow-reserved :type boolean
                   :initarg :allow-reserved
                   :initform nil
                   :reader encoding-allow-reserved-p)))

(defun encoding-explode-p (encoding)
  (check-type encoding encoding)
  (if (slot-boundp encoding 'explode)
      (slot-value encoding 'explode)
      (let ((style (encoding-style encoding)))
        (if (equal style "form")
            t
            nil))))

(defclass media-type ()
  ((schema :type (or schema null)
           :initarg :schema
           :initform nil
           :reader media-type-schema)
   (encoding :type (association-list string encoding)
             :initarg :encoding
             :initform nil
             :reader media-type-encoding)))

(undeclaim-safety)
