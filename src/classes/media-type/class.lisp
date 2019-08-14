(defpackage #:apispec/classes/media-type/class
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/encoding
                #:encoding)
  (:import-from #:apispec/classes/schema
                #:schema)
  (:export #:media-type
           #:media-type-schema
           #:media-type-encoding))
(in-package #:apispec/classes/media-type/class)

(defclass media-type ()
  ((schema :type (or schema null)
           :initarg :schema
           :initform nil
           :reader media-type-schema)
   (encoding :type (association-list string encoding)
             :initarg :encoding
             :initform nil
             :reader media-type-encoding)))

