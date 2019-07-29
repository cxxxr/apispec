(defpackage #:apispec/operation/media-type
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/operation/header
                #:header)
  (:import-from #:apispec/parameter
                #:parameter-style)
  (:export #:encoding
           #:media-type
           #:media-type-schema
           #:media-type-encoding))
(in-package #:apispec/operation/media-type)

(declaim-safety)

(defclass encoding ()
  ((content-type :type string)
   (headers :type (association-list string header)
            :initarg :headers
            :initform nil
            :reader encoding-headers)
   (style :type parameter-style
          :initarg :style)
   (explode :type boolean
            :initarg :explode)
   (allow-reserved :type boolean
                   :initarg :allow-reserved
                   :initform nil
                   :reader encoding-allow-reserved-p)))

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
