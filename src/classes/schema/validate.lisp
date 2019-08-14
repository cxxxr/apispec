(uiop:define-package #:apispec/classes/schema/validate
    (:mix #:apispec/classes/schema/core
          #:cl)
  (:import-from #:cl-ppcre)
  (:export #:validation-failed
           #:validate-data))
(in-package #:apispec/classes/schema/validate)

(define-condition validation-failed (error)
  ((value :initarg :value)
   (schema :initarg :schema)
   (message :initarg :message
            :initform nil))
  (:report (lambda (condition stream)
             (with-slots (value schema message) condition
               (format stream "~S is invalid for ~S~@[:~%  ~A~]"
                       value
                       (type-of schema)
                       message)))))

(defgeneric validate-data (value schema)
  (:method (value (schema symbol))
    (validate-data value (make-schema schema)))
  (:method :around ((value null) (schema schema))
    (unless (or (typep schema 'boolean)  ;; BOOLEAN can be NIL
                (schema-nullable-p schema))
      (error 'validation-failed
             :value value
             :schema schema
             :message "Not nullable"))
    t)
  (:method (value (schema schema))
    t))


;;
;; Number Types

(defmethod validate-data (value (schema number))
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
    (error 'validation-failed
           :value value
           :schema schema
           :message
           (with-output-to-string (*standard-output*)
             (princ "Not in range of ")
             (when (number-minimum schema)
               (princ (number-minimum schema))
               (if (number-exclusive-minimum-p schema)
                   (princ " <")
                   (princ " <=")))
             (princ " value ")
             (when (number-maximum schema)
               (if (number-exclusive-maximum-p schema)
                   (princ "< ")
                   (princ "<= "))
               (princ (number-maximum schema))))))
  (when (number-multiple-of schema)
    (unless (= (mod value (number-multiple-of schema)) 0)
      (error 'validation-failed
             :value value
             :schema schema
             :message (format nil "Not multiple of ~A" (number-multiple-of schema)))))

  t)


;;
;; String Types

(defmethod validate-data (value (schema string))
  (unless (and (or (not (string-min-length schema))
                   (<= (string-min-length schema) (length value)))
               (or (not (string-max-length schema))
                   (<= (length value) (string-max-length schema))))
    (error 'validation-failed
           :value value
           :schema schema
           :message (format nil "The length not in the range~@[ from ~A~]~@[ to ~A~]"
                            (string-min-length schema)
                            (string-max-length schema))))

  (unless (or (not (string-pattern schema))
              (ppcre:scan (string-pattern schema) value))
    (error 'validation-failed
           :value value
           :schema schema
           :message (format nil "Not match to ~S"
                            (string-pattern schema))))

  t)


;;
;; Array Type

(defmethod validate-data (value (schema array))
  (unless (and (or (not (array-min-items schema))
                   (<= (array-min-items schema) (length value)))
               (or (not (array-max-items schema))
                   (<= (length value) (array-max-items schema))))
    (error 'validation-failed
           :value value
           :schema schema
           :message (format nil "The length not in the range~@[ from ~A~]~@[ to ~A~]"
                            (array-min-items schema)
                            (array-max-items schema))))

  (when (array-unique-items-p schema)
    (unless (= (length (remove-duplicates value :test #'equal))
               (length value))
      (error 'validation-failed
             :value value
             :schema schema
             :message "The items are not unique"))))


;;
;; Object Type

(defmethod validate-data (value (schema object))
  (unless (object-properties schema)
    (return-from validate-data value))

  (loop for (key . field-value) in value
        for prop = (find key (object-properties schema)
                         :key #'property-name
                         :test #'equal)
        do (if prop
               (handler-case (validate-data field-value (property-type prop))
                 (validation-failed (e)
                   (error 'validation-failed
                          :value value
                          :schema schema
                          :message (format nil "Validation failed at ~S:~%  ~S"
                                           key
                                           (slot-value e 'message)))))
               (let ((additional-properties (object-additional-properties schema)))
                 (etypecase additional-properties
                   (null (error 'validation-failed
                                :value value
                                :schema schema
                                :message (format nil "Undefined property: ~S" key)))
                   ((eql t))
                   (schema (validate-data field-value additional-properties))))))
  (loop for key in (object-required schema)
        unless (find key value :key #'car :test #'equal)
          collect key into missing-keys
        finally
           (when missing-keys
             (error 'validation-failed
                    :value value
                    :schema schema
                    :message (format nil "Missing required keys: ~S" missing-keys))))
  (unless (and (or (not (object-min-properties schema))
                   (nthcdr (object-min-properties schema) value))
               (or (not (object-max-properties schema))
                   (nthcdr (object-max-properties schema) value)))
    (error 'validation-failed
           :value value
           :schema schema
           :message
           (format nil "The number of properties has to be in the range of~@[ ~A <=~] (length properties)~@[ <= ~A~]"
                   (object-min-properties schema)
                   (object-max-properties schema))))

  t)
