(defpackage #:apispec/body/encoder/json
  (:use #:cl
        #:apispec/body/encoder/base)
  (:import-from #:apispec/classes/schema
                #:object
                #:object-properties
                #:schema-nullable-p
                #:property-name
                #:property-type)
  (:shadowing-import-from #:apispec/classes/schema
                          #:number
                          #:string
                          #:boolean
                          #:array
                          #:array-items)
  (:export #:json-encoder))
(in-package #:apispec/body/encoder/json)

(defclass json-encoder (encoder) ())

(defmethod encode-data ((value null) (encoder json-encoder) schema)
  (if (typep schema '(or boolean array))
      (call-next-method)
      (princ "null"))
  (values))

(defmethod encode-data (value (encoder json-encoder) (schema number))
  (princ value)
  (values))

(defmethod encode-data (value (encoder json-encoder) (schema boolean))
  (princ (ecase value
           (t "true")
           ('nil "false")))
  (values))

(defmethod encode-data (value (encoder json-encoder) (schema string))
  (check-type value cl:string)
  (let ((*print-pretty* nil)
        (*print-escape* nil))
    (prin1 value))
  (values))

(defmethod encode-data (value (encoder json-encoder) (schema array))
  (let ((items-schema (array-items schema)))
    (write-char #\[)
    (if (listp value)
        (mapl (lambda (items)
                (encode-data (first items) encoder items-schema)
                (when (rest items)
                  (write-char #\,)))
              value)
        (loop with first = t
              for item across value
              do (unless first
                   (write-char #\,))
                 (setf first nil)
                 (encode-data item encoder items-schema)))
    (write-char #\]))
  (values))

(defmethod encode-data (value (encoder json-encoder) (schema object))
  (write-char #\{)
  (loop for (prop . rest) on (object-properties schema)
        for (key . field-value) = (find (property-name prop)
                                        value
                                        :key #'car
                                        :test #'equal)
        if (and (null key)
                (not (schema-nullable-p (property-type prop))))
          do (error "Property ~S is required" (property-name prop))
        else
          do (prin1 (property-name prop))
             (write-char #\:)
             (encode-data field-value encoder (property-type prop))
        when rest
          do (write-char #\,))
  (write-char #\})
  (values))
