(uiop:define-package #:apispec/classes/schema/coerce
  (:mix #:apispec/classes/schema/core
        #:cl)
  (:use #:apispec/classes/schema/errors
        #:apispec/utils
        #:parse-number)
  (:import-from #:apispec/classes/schema/core
                #:parse-schema-definition)
  (:import-from #:apispec/classes/schema/validate
                #:validate-data)
  (:import-from #:apispec/classes/schema/errors
                #:message)
  (:import-from #:apispec/errors
                #:read-new-value)
  (:import-from #:cl-ppcre)
  (:import-from #:local-time)
  (:export #:coerce-data
           #:*coerce-integer-string-to-boolean*))
(in-package #:apispec/classes/schema/coerce)

(defgeneric coerce-data (value schema)
  (:method (value (schema symbol))
    (if (eq schema t)
        value
        (coerce-data value (make-schema schema))))
  (:method (value (schema cons))
    (coerce-data value
                 (multiple-value-bind (type args)
                     (parse-schema-definition schema)
                   (apply #'make-schema type args))))
  (:method (value schema)
    ;; Don't raise COERCE-FAILED when the value is NIL.
    ;; If the schema is not nullable, it'll be catched in VALIDATE-DATA.
    (when value
      (error 'schema-coercion-failed
             :value value
             :schema schema)))
  (:method :around (value (schema schema))
    (if (and (null value)
             (schema-has-default-p schema)
             (schema-default schema))
        (schema-default schema)
        (restart-case
            (call-next-method)
          (use-value (&optional (new-value nil new-value-supplied))
            (unless new-value-supplied
              (setf new-value (read-new-value)))
            new-value)))))

(defmacro with-validation (schema &body body)
  (let ((results (gensym "RESULTS")))
    `(let ((,results (progn ,@body)))
       (validate-data ,results ,schema)
       ,results)))

;;
;; Number Types

(defmethod coerce-data :around (value (schema number))
  (with-validation schema
    (call-next-method)))

(defmethod coerce-data ((value cl:number) (schema number))
  (handler-case (typecase schema
                  (integer (coerce value 'cl:integer))
                  (float (coerce value 'cl:float))
                  (double (coerce value 'cl:double-float))
                  (otherwise value))
    (error () (error 'schema-coercion-failed
                     :value value
                     :schema schema))))

(defmethod coerce-data ((value cl:string) (schema number))
  (coerce-data
    (handler-case (parse-number value)
      (error () (error 'schema-coercion-failed
                       :value value
                       :schema schema)))
    schema))

(defmethod coerce-data ((value cl:string) (schema float))
  (coerce-data
    (handler-case (parse-number value :float-format 'cl:single-float)
      (error () (error 'schema-coercion-failed
                       :value value
                       :schema schema)))
    schema))

(defmethod coerce-data ((value cl:string) (schema double))
  (coerce-data
    (handler-case (parse-number value :float-format 'cl:double-float)
      (error () (error 'schema-coercion-failed
                       :value value
                       :schema schema)))
    schema))


;;
;; String Types

(defmethod coerce-data :around (value (schema string))
  (with-validation schema
    (call-next-method)))

(defmethod coerce-data ((value cl:string) (schema string))
  (princ-to-string value))

(defmethod coerce-data ((value vector) (schema binary))
  value)

(defmethod coerce-data ((value stream) (schema binary))
  value)

(defmethod coerce-data ((value cl:string) (schema date))
  (check-type value cl:string)
  (ppcre:register-groups-bind ((#'parse-integer year month date))
      ("(\\d{4})-(\\d{2})-(\\d{2})" value)
    (local-time:universal-to-timestamp (encode-universal-time 0 0 0 date month year))))

(defmethod coerce-data ((value cl:string) (schema date-time))
  (check-type value cl:string)
  (local-time:parse-rfc3339-timestring value))

(defvar *coerce-integer-string-to-boolean* nil)

(defmethod coerce-data (value (schema boolean))
  (etypecase value
    (cl:string
      (unless *coerce-integer-string-to-boolean*
        (error 'schema-coercion-failed :value value :schema schema))
      (cond
        ((equal value "1") t)
        ((equal value "0") nil)
        (t (error 'schema-coercion-failed :value value :schema schema))))
    (cl:boolean value)))


;;
;; Array Type

(defmethod coerce-data :around (value (schema array))
  (with-validation schema
    (call-next-method)))

(defmethod coerce-data (value (schema array))
  (if (array-items schema)
      (map 'vector
           (lambda (item)
             (coerce-data item (array-items schema)))
           value)
      (coerce value 'vector)))


;;
;; Object Type

(defmethod coerce-data :around (value (schema object))
  (with-validation schema
    (call-next-method)))

(defparameter *force-allow-additional-properties* nil)

(defmethod coerce-data (value (schema object))
  (unless (typep value 'association-list)
    (error 'schema-coercion-failed
           :value value
           :schema schema))

  (let ((properties (object-properties schema)))
    (nconc
      (loop with additional-properties = (object-additional-properties schema)
            for (key . field-value) in value
            for prop = (find key properties
                             :key #'property-name
                             :test #'equal)
            if prop
              collect (cons key
                            (and field-value
                                 (coerce-data field-value (property-type prop))))
            else if additional-properties
              collect (cons key
                            (and field-value
                                 (coerce-data field-value additional-properties)))
            else if (not *force-allow-additional-properties*)
              do (error 'schema-coercion-failed
                        :value value
                        :schema schema
                        :message (format nil "Unpermitted property: ~S" key)))
      (loop for prop in properties
            for type = (property-type prop)
            when (and (schema-has-default-p type)
                      (not (find (property-name prop)
                                 value
                                 :key #'car
                                 :test #'equal)))
            collect
            (cons (property-name prop)
                  (schema-default type))))))
