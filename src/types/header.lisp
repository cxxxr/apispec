(defpackage #:apispec/types/header
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/types/schema
                #:schema)
  (:export #:header
           #:header-required-p
           #:header-schema
           #:header-style
           #:header-explode-p))
(in-package #:apispec/types/header)

(declaim-safety)

(defun header-style-string-p (style)
  (and (member style '("matrix" "label" "form" "simple" "spaceDelimited" "pipeDelimited" "deepObject")
               :test #'equal)
       t))

(deftype header-style ()
  '(satisfies header-style-string-p))

(defclass header ()
  ((required :type boolean
             :initarg :required
             :initform nil
             :reader header-required-p)
   (schema :type (or schema null)
           :initarg :schema
           :initform nil
           :reader header-schema)
   (style :type header-style
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
