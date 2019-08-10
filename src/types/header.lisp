(defpackage #:apispec/types/header
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/types/schema
                #:schema)
  (:export #:header
           #:header-required-p
           #:header-schema
           #:header-explode-p))
(in-package #:apispec/types/header)

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
   (explode :type boolean
            :initarg :explode
            :initform nil
            :reader header-explode-p)))

(undeclaim-safety)
