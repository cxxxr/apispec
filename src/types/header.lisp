(defpackage #:apispec/types/header
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/types/schema
                #:schema
                #:coerce-data)
  (:import-from #:apispec/types/complex
                #:parse-simple-value)
  (:export #:header
           #:header-required-p
           #:header-schema
           #:header-explode-p
           #:header-missing
           #:coerce-with-header))
(in-package #:apispec/types/header)

(declaim-safety)

(define-condition header-missing (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (princ "Header is missing" stream))))

(defclass header ()
  ((required :type boolean
             :initarg :required
             :initform nil
             :reader header-required-p)
   (schema :type (or schema null)
           :initarg :schema
           :initform nil
           :reader header-schema)
   (explode :type boolean
            :initarg :explode
            :initform nil
            :reader header-explode-p)))

(defun coerce-with-header (value header)
  (check-type value (or string null))
  (check-type header header)
  (when (and (null value)
             (header-required-p header))
    (error 'header-missing))
  (coerce-data
    (parse-simple-value value
                        :as (header-schema header)
                        :explode (header-explode-p header))
    (or (header-schema header) t)))

(undeclaim-safety)
