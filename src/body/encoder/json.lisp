(defpackage #:apispec/body/encoder/json
  (:use #:cl)
  (:import-from #:apispec/classes/schema
                #:object
                #:binary
                #:object-properties
                #:schema-nullable-p
                #:property-name
                #:property-type)
  (:shadowing-import-from #:apispec/classes/schema
                          #:schema
                          #:number
                          #:string
                          #:boolean
                          #:array
                          #:array-items)
  (:export #:encode-data-to-json))
(in-package #:apispec/body/encoder/json)

(declaim (ftype (function (t schema)) encode-data-to-json))

(defun encode-object (value schema)
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
             (encode-data-to-json field-value (property-type prop))
        when rest
          do (write-char #\,))
  (write-char #\})
  (values))

(defun encode-array (value schema)
  (let ((items-schema (array-items schema)))
    (write-char #\[)
    (if (listp value)
        (mapl (lambda (items)
                (encode-data-to-json (first items) items-schema)
                (when (rest items)
                  (write-char #\,)))
              value)
        (loop with first = t
              for item across value
              do (unless first
                   (write-char #\,))
                 (setf first nil)
                 (encode-data-to-json item items-schema)))
    (write-char #\])))

(defun encode-boolean (value)
  (princ (ecase value
           (t "true")
           ('nil "false")))
  (values))

(defun encode-data-to-json (value schema)
  (typecase schema
    (object (encode-object value schema))
    (array (encode-array value schema))
    (boolean (encode-boolean value))
    (binary (error "Can't encode binary data to JSON"))
    (otherwise
      (typecase value
        (null
          (if (schema-nullable-p schema)
              (princ "null")
              (error "Not nullable")))
        (otherwise (jojo:with-output (*standard-output*)
                     (jojo:%to-json value))))))
  (values))
