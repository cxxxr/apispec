(defpackage #:apispec/body/encoder/base
  (:use #:cl)
  (:import-from #:apispec/classes/schema
                #:make-schema)
  (:import-from #:apispec/classes/schema/core
                #:parse-schema-definition)
  (:export #:encoder
           #:encode-data))
(in-package #:apispec/body/encoder/base)

(defclass encoder () ())

(defgeneric encode-data (value encoder schema)
  (:method (value (encoder symbol) schema)
    (encode-data value (make-instance encoder) schema))
  (:method (value encoder (schema symbol))
    (encode-data value encoder (make-schema schema)))
  (:method (value encoder (schema cons))
    (encode-data value encoder (multiple-value-bind (type args)
                                   (parse-schema-definition schema)
                                 (apply #'make-schema type args)))))
