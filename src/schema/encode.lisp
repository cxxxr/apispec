(defpackage #:apispec/schema/encode
  (:use #:cl
        #:apispec/schema/core)
  (:shadowing-import-from #:apispec/schema/core
                          #:number
                          #:boolean
                          #:name
                          #:type
                          #:nullable
                          #:parse-schema-definition)
  (:import-from #:jonathan)
  (:export #:encode-data))
(in-package #:apispec/schema/encode)

(defgeneric encode-data (value schema)
  (:method (value schema)
    (jojo:%to-json (if (and (null value)
                            (slot-boundp schema 'nullable)
                            (slot-value schema 'nullable))
                       :null
                       value)))
  (:method (value (schema symbol))
    (encode-data value (make-schema schema)))
  (:method (value (schema cons))
    (encode-data value
                 (multiple-value-bind (type args)
                     (parse-schema-definition schema)
                   (apply #'make-schema type args)))))

(defmethod encode-data (value (schema boolean))
  (jojo:%to-json (if value
                           t
                           :false)))

(defmethod encode-data (value (schema object))
  (jojo:with-object
    (loop for prop in (object-properties schema)
          for (key . field-value) = (find (slot-value prop 'name)
                                          value
                                          :key #'car
                                          :test #'equal)
          when key
            do (jojo:write-key (cl:string key))
               (write-char #\: jonathan.encode::*stream*)
               (encode-data field-value (slot-value prop 'type)))))
