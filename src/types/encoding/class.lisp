(defpackage #:apispec/types/encoding/class
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/types/schema
                #:schema)
  (:import-from #:apispec/types/header
                #:header)
  (:import-from #:apispec/types/complex
                #:complex-style)
  (:export #:encoding
           #:encoding-content-type
           #:encoding-headers
           #:encoding-style
           #:encoding-explode-p
           #:encoding-allow-reserved-p))
(in-package #:apispec/types/encoding/class)

(declaim-safety)

(defclass encoding ()
  ((content-type :type (or string null)
                 :initarg :content-type
                 :initform nil
                 :reader encoding-content-type)
   (headers :type (association-list string header)
            :initarg :headers
            :initform nil
            :reader encoding-headers)
   (style :type complex-style
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

(undeclaim-safety)
