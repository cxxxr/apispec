(uiop:define-package #:apispec/classes/schema/validate
  (:mix #:apispec/classes/schema/core
        #:cl)
  (:use #:apispec/classes/schema/errors)
  (:import-from #:apispec/classes/schema/core
                #:parse-schema-definition)
  (:import-from #:apispec/utils
                #:association-list
                #:email-format-p)
  (:import-from #:cl-ppcre)
  (:import-from #:local-time)
  (:export #:schema-validation-failed
           #:validate-data))
(in-package #:apispec/classes/schema/validate)

(defgeneric validate-data (value schema)
  (:method (value (schema symbol))
    (validate-data value (make-schema schema)))
  (:method (value (schema cons))
    (validate-data value
                   (multiple-value-bind (type args)
                       (parse-schema-definition schema)
                     (apply #'make-schema type args))))
  (:method :around ((value null) (schema schema))
    (unless (or (typep schema 'boolean)  ;; BOOLEAN can be NIL
                (schema-nullable-p schema)
                (typep schema 'object))
      (error 'schema-validation-failed
             :value value
             :schema schema
             :message "Not nullable"))
    (if (typep schema 'object)
        (call-next-method)
        t))
  (:method (value (schema schema))
    t)
  (:method :around (value (schema schema))
    (restart-case (call-next-method)
      (skip-validation () value))))


;;
;; Number Types

(defmethod validate-data (value (schema number))
  (unless (numberp value)
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message "Not a number"))

  (unless (and (or (not (number-minimum schema))
                   (funcall (if (number-exclusive-minimum-p schema)
                                #'<
                                #'<=)
                            (number-minimum schema)
                            value))
               (or (not (number-maximum schema))
                   (funcall (if (number-exclusive-maximum-p schema)
                                #'<
                                #'<=)
                            value
                            (number-maximum schema))))
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message
           (with-output-to-string (*standard-output*)
             (princ "Not in range of ")
             (when (number-minimum schema)
               (princ (number-minimum schema))
               (write-char #\Space)
               (if (number-exclusive-minimum-p schema)
                   (princ "< ")
                   (princ "<= ")))
             (princ "value ")
             (when (number-maximum schema)
               (if (number-exclusive-maximum-p schema)
                   (princ "< ")
                   (princ "<= "))
               (princ (number-maximum schema))))))
  (when (number-multiple-of schema)
    (unless (= (mod value (number-multiple-of schema)) 0)
      (error 'schema-validation-failed
             :value value
             :schema schema
             :message (format nil "Not multiple of ~A" (number-multiple-of schema)))))

  t)


;;
;; String Types

(defmethod validate-data (value (schema string))
  (unless (stringp value)
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message "Not a string"))

  (unless (and (or (not (string-min-length schema))
                   (<= (string-min-length schema) (length value)))
               (or (not (string-max-length schema))
                   (<= (length value) (string-max-length schema))))
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message (format nil "The length not in the range~@[ from ~A~]~@[ to ~A~]"
                            (string-min-length schema)
                            (string-max-length schema))))

  (unless (or (not (string-pattern schema))
              (ppcre:scan (string-pattern schema) value))
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message (format nil "Not match to ~S"
                            (string-pattern schema))))

  (unless (or (not (equal "email" (schema-format schema)))
              (email-format-p value))
    (error 'schema-validation-failed
           :value value
           :schema schema))
  t)

(defmethod validate-data (value (schema binary))
  (unless (typep value '(or (vector (unsigned-byte 8))
                            stream))
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message "Not a byte vector"))

  t)

(defmethod validate-data (value (schema date))
  (unless (typep value 'local-time:timestamp)
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message "Not a LOCAL-TIME:TIMESTAMP"))

  t)

(defmethod validate-data (value (schema date-time))
  (unless (typep value 'local-time:timestamp)
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message "Not a LOCAL-TIME:TIMESTAMP"))

  t)


;;
;; Array Type

(defmethod validate-data (value (schema array))
  (unless (arrayp value)
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message "Not an array"))

  (unless (and (or (not (array-min-items schema))
                   (<= (array-min-items schema) (length value)))
               (or (not (array-max-items schema))
                   (<= (length value) (array-max-items schema))))
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message (format nil "The length not in the range~@[ from ~A~]~@[ to ~A~]"
                            (array-min-items schema)
                            (array-max-items schema))))

  (when (array-unique-items-p schema)
    (unless (= (length (remove-duplicates value :test #'equal))
               (length value))
      (error 'schema-validation-failed
             :value value
             :schema schema
             :message "The items are not unique"))))


;;
;; Object Type

(defmethod validate-data (value (schema object))
  (unless (typep value 'association-list)
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message "Not an association list"))

  (unless (object-properties schema)
    (return-from validate-data value))

  (let ((invalid-keys '())
        (unpermitted-keys '()))
    (loop for (key . field-value) in value
          for prop = (find key (object-properties schema)
                           :key #'property-name
                           :test #'equal)
          do (if prop
                 (handler-case (validate-data field-value (property-type prop))
                   (schema-validation-failed ()
                     (push key invalid-keys)))
                 (let ((additional-properties (object-additional-properties schema)))
                   (etypecase additional-properties
                     (null (push key unpermitted-keys))
                     ((eql t))
                     (schema (validate-data field-value additional-properties))))))
    (let ((missing-keys
            (loop for key in (object-required schema)
                  unless (find key value :key #'car :test #'equal)
                  collect key)))
      (when (or invalid-keys
                missing-keys
                unpermitted-keys)
        (error 'schema-object-error
               :invalid-keys (nreverse invalid-keys)
               :missing-keys missing-keys
               :unpermitted-keys (nreverse unpermitted-keys)
               :value value
               :schema schema))
      (unless (and (or (not (object-min-properties schema))
                       (nthcdr (object-min-properties schema) value))
                   (or (not (object-max-properties schema))
                       (nthcdr (object-max-properties schema) value)))
        (error 'schema-validation-failed
               :value value
               :schema schema
               :message
               (format nil "The number of properties has to be in the range of~@[ ~A <=~] (length properties)~@[ <= ~A~]"
                       (object-min-properties schema)
                       (object-max-properties schema))))
      t)))
