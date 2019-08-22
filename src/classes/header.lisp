(defpackage #:apispec/classes/header
  (:use #:cl
        #:apispec/utils
        #:apispec/errors)
  (:import-from #:apispec/classes/schema
                #:schema
                #:coerce-data)
  (:import-from #:apispec/complex
                #:parse-simple-value)
  (:export #:header
           #:header-required-p
           #:header-schema
           #:header-explode-p
           #:header-error
           #:header-missing
           #:coerce-with-header))
(in-package #:apispec/classes/header)

(declaim-safety)

(define-condition header-error (apispec-error) ())

(define-condition header-missing (header-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (princ "Header is missing" stream))))

(defclass header ()
  ((required :type boolean
             :initarg :required
             :initform nil
             :reader header-required-p)
   (schema :type (or schema (eql t))
           :initarg :schema
           :initform t
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
    (header-schema header)))

(undeclaim-safety)
