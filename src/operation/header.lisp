(defpackage #:apispec/operation/header
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/schema
                #:schema)
  (:import-from #:apispec/parameter
                #:parameter-style)
  (:export #:header
           #:header-required-p
           #:header-schema
           #:header-style
           #:header-explode-p))
(in-package #:apispec/operation/header)

(declaim-safety)

(defclass header ()
  ((required :type boolean
             :initarg :required
             :initform nil
             :reader header-required-p)
   (schema :type (or schema null)
           :initarg :schema
           :initform nil
           :reader header-schema)
   (style :type parameter-style
          :initarg :style
          :initform "simple"
          :reader header-style)
   (explode :type boolean
            :initarg :explode)))

(defun header-explode-p (header)
  (check-type header header)
  (if (slot-boundp header 'explode)
      (slot-value header 'explode)
      (let ((style (header-style header)))
        (if (equal style "form")
            t
            nil))))

(undeclaim-safety)
