(uiop:define-package #:apispec/classes/schema/validate
  (:mix #:apispec/classes/schema/core
        #:cl)
  (:use #:apispec/classes/schema/errors)
  (:import-from #:apispec/classes/schema/core
                #:parse-schema-definition)
  (:import-from #:apispec/utils
                #:association-list
                #:email-format-p
                #:uuid-format-p
                #:json-string-p)
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

  (when (schema-enum schema)
    (unless (find value (schema-enum schema) :test #'string=)
      (error 'schema-validation-failed
             :value value
             :schema schema)))

  (unless (or (not (equal "email" (schema-format schema)))
              (email-format-p value))
    (error 'schema-validation-failed
           :value value
           :schema schema))

  (unless (or (not (equal "uuid" (schema-format schema)))
              (uuid-format-p value))
    (error 'schema-validation-failed
           :value value
           :schema schema))

  (when (and (equal "json" (schema-format schema))
             (not (json-string-p value)))
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

(defun ensure-data-1 (value ensure-fn)
  (typecase value
    (cl:string
     (handler-case (funcall ensure-fn value)
       (error ()
         nil)))
    (local-time:timestamp
     value)
    (otherwise
     nil)))

(defun ensure-date (value)
  (ensure-data-1 value
                 (lambda (value)
                   ;; https://tools.ietf.org/html/rfc3339#section-5.6 full-date
                   (and (ppcre:scan "^\\d{4}-\\d{2}-\\d{2}$" value)
                        (local-time:parse-rfc3339-timestring value :allow-missing-time-part t)))))

(defun ensure-date-time (value)
  (ensure-data-1 value
                 (lambda (value)
                   ;; https://tools.ietf.org/html/rfc3339#section-5.6 date-time
                   (and (ppcre:scan "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?(?:Z|[+-]\\d{2}:\\d{2})" value)
                        (local-time:parse-rfc3339-timestring value)))))

(defmethod validate-data (value (schema date))
  (unless (ensure-date value)
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message "Not a LOCAL-TIME:TIMESTAMP"))
  t)

(defmethod validate-data (value (schema date-time))
  (unless (ensure-date-time value)
    (error 'schema-validation-failed
           :value value
           :schema schema
           :message "Not a LOCAL-TIME:TIMESTAMP"))
  t)


;;
;; Array Type

(defmethod validate-data (value (schema array))
  (unless (and (not (stringp value))
               (arrayp value))
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
             :message "The items are not unique")))

  (when (array-items schema)
    (map nil
         (lambda (item)
           (validate-data item (array-items schema)))
         value))

  t)


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

  (let ((invalid '())
        (unpermitted '()))
    (loop for (key . field-value) in value
          for prop = (find key (object-properties schema)
                           :key #'property-name
                           :test #'equal)
          do (if prop
                 (handler-case (validate-data field-value (property-type prop))
                   (schema-validation-failed (e)
                     (push (cons key e) invalid)))
                 (let ((additional-properties (object-additional-properties schema)))
                   (etypecase additional-properties
                     (null (push key unpermitted))
                     ((eql t))
                     (schema (validate-data field-value additional-properties))))))
    (let ((missing
            (loop for key in (object-required schema)
                  unless (find key value :key #'car :test #'equal)
                  collect key)))
      (when (or invalid
                missing
                unpermitted)
        (error 'schema-object-error
               :invalid (nreverse invalid)
               :missing missing
               :unpermitted (nreverse unpermitted)
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
