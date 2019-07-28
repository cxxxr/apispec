(defpackage #:apispec/schema/coerce
  (:use #:cl
        #:apispec/schema/core
        #:apispec/schema/validate
        #:apispec/utils
        #:parse-number)
  (:shadowing-import-from #:apispec/schema/core
                          #:number
                          #:float
                          #:double
                          #:integer
                          #:string
                          #:boolean
                          #:array

                          #:items
                          #:name
                          #:type
                          #:default

                          #:parse-schema-definition)
  (:import-from #:cl-ppcre)
  (:import-from #:local-time)
  (:export #:coerce-failed
           #:coerce-data))
(in-package #:apispec/schema/coerce)

(define-condition coerce-failed (error)
  ((value :initarg :value)
   (schema :initarg :schema))
  (:report (lambda (condition stream)
             (with-slots (value schema) condition
               (format stream "~S cannot be coerced to ~S"
                       value
                       (type-of schema))))))

(defgeneric coerce-data (value schema)
  (:method (value (schema symbol))
    (coerce-data value (make-schema schema)))
  (:method (value (schema cons))
    (coerce-data value
                 (multiple-value-bind (type args)
                     (parse-schema-definition schema)
                   (apply #'make-schema type args))))
  (:method (value schema)
    ;; Don't raise COERCE-FAILED when the value is NIL.
    ;; If the schema is not nullable, it'll be catched in VALIDATE-DATA.
    (when value
      (error 'coerce-failed
             :value value
             :schema schema)))
  (:method :around (value (schema schema))
    (let ((result (if (and (null value)
                           (slot-boundp schema 'default)
                           (slot-value schema 'default))
                      (coerce-data (slot-value schema 'default) schema)
                      (call-next-method))))
      (validate-data result schema)
      result)))

;;
;; Number Types

(defmethod coerce-data ((value cl:number) (schema number))
  (typecase schema
    (integer (coerce value 'cl:integer))
    (float (coerce value 'cl:float))
    (double (coerce value 'cl:double-float))
    (otherwise value)))

(defmethod coerce-data ((value cl:string) (schema number))
  (coerce-data (parse-number value) schema))

(defmethod coerce-data ((value cl:string) (schema float))
  (coerce-data (parse-number value :float-format 'cl:single-float) schema))

(defmethod coerce-data ((value cl:string) (schema double))
  (coerce-data (parse-number value :float-format 'cl:double-float) schema))


;;
;; String Types

(defmethod coerce-data ((value cl:string) (schema string))
  (princ-to-string value))

(defmethod coerce-data ((value cl:string) (schema date))
  (check-type value cl:string)
  (ppcre:register-groups-bind ((#'parse-integer year month date))
      ("(\\d{4})-(\\d{2})-(\\d{2})" value)
    (local-time:universal-to-timestamp (encode-universal-time 0 0 0 date month year))))

(defmethod coerce-data ((value cl:string) (schema date-time))
  (check-type value cl:string)
  (local-time:parse-rfc3339-timestring value))

(defmethod coerce-data (value (schema boolean))
  (etypecase value
    (cl:string
     (cond
       ((equal value "true") t)
       ((equal value "false") nil)
       (t (error 'coerce-failed :value value :schema schema))))
    (cl:boolean value)))


;;
;; Array Type

(defmethod coerce-data (value (schema array))
  (if (slot-boundp schema 'items)
      (map 'vector
           (lambda (item)
             (coerce-data item (slot-value schema 'items)))
           value)
      (coerce value 'vector)))


;;
;; Object Type

(defmethod coerce-data (value (schema object))
  (check-type value association-list)

  (let ((properties (object-properties schema)))
    (nconc
     (loop with additional-properties = (object-additional-properties schema)
           for (key . field-value) in value
           for prop = (find key properties
                            :key (lambda (prop) (slot-value prop 'name))
                            :test #'equal)
           collect
           (cons key
                 (cond
                   ((or prop
                        (typep additional-properties 'schema))
                    (handler-case (coerce-data field-value (if prop
                                                               (slot-value prop 'type)
                                                               additional-properties))
                      (validation-failed (e)
                        (error 'validation-failed
                               :value value
                               :schema schema
                               :message (format nil "Validation failed at ~S:~%  ~S"
                                                key
                                                (slot-value e 'message))))))
                   ((not additional-properties)
                    (error 'validation-failed
                           :value value
                           :schema schema
                           :message (format nil "Unpermitted property: ~S" key)))
                   ((eq additional-properties t)
                    field-value)
                   (t (error "Not allowed branch. Perhaps a bug of apispec.")))))
     (loop for prop in properties
           for type = (slot-value prop 'type)
           when (and (slot-boundp type 'default)
                     (not (find (slot-value prop 'name)
                                value
                                :key #'car
                                :test #'equal)))
             collect
             (cons (slot-value prop 'name)
                   (slot-value type 'default))))))
