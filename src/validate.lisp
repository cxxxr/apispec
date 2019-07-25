(defpackage #:apispec/validate
  (:use #:cl
        #:apispec/schema)
  (:shadowing-import-from #:apispec/schema
                          #:number
                          #:float
                          #:double
                          #:integer
                          #:string
                          #:boolean
                          #:array

                          #:multiple-of
                          #:maximum
                          #:exclusive-maximum
                          #:minimum
                          #:exclusive-minimum
                          #:max-length
                          #:min-length
                          #:pattern
                          #:min-items
                          #:max-items
                          #:unique-items
                          #:required
                          #:properties
                          #:min-properties
                          #:max-properties
                          #:type
                          #:nullable)
  (:import-from #:cl-ppcre)
  (:export #:validation-failed
           #:validate-data))
(in-package #:apispec/validate)

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
  (:method (value (schema schema))
    t))


;;
;; Number Types

(defmethod validate-data (value (schema number))
  (unless (and (or (not (slot-boundp schema 'minimum))
                   (funcall (if (and (slot-boundp schema 'exclusive-minimum)
                                     (slot-value schema 'exclusive-minimum))
                                #'<
                                #'<=)
                            (slot-value schema 'minimum)
                            value))
               (or (not (slot-boundp schema 'maximum))
                   (funcall (if (and (slot-boundp schema 'exclusive-maximum)
                                     (slot-value schema 'exclusive-maximum))
                                #'<
                                #'<=)
                            value
                            (slot-value schema 'maximum))))
    (error 'validation-failed
           :value value
           :schema schema
           :message
           (with-output-to-string (*standard-output*)
             (princ "Not in range of ")
             (when (slot-boundp schema 'minimum)
               (princ (slot-value schema 'minimum))
               (if (and (slot-boundp schema 'exclusive-minimum)
                        (slot-value schema 'exclusive-minimum))
                   (princ " <")
                   (princ " <=")))
             (princ " value ")
             (when (slot-boundp schema 'maximum)
               (if (and (slot-boundp schema 'exclusive-maximum)
                        (slot-value schema 'exclusive-maximum))
                   (princ "< ")
                   (princ "<= "))
               (princ (slot-value schema 'maximum))))))
  (when (slot-boundp schema 'multiple-of)
    (unless (= (mod value (slot-value schema 'multiple-of)) 0)
      (error 'validation-failed
             :value value
             :schema schema
             :message (format nil "Not multiple of ~A" (slot-value schema 'multiple-of)))))

  t)


;;
;; String Types

(defmethod validate-data (value (schema string))
  (unless (and (or (not (slot-boundp schema 'min-length))
                   (<= (slot-value schema 'min-length) (length value)))
               (or (not (slot-boundp schema 'max-length))
                   (<= (length value) (slot-value schema 'max-length))))
    (error 'validation-failed
           :value value
           :schema schema
           :message (format nil "The length not in the range~@[ from ~A~]~@[ to ~A~]"
                            (and (slot-boundp schema 'min-length)
                                 (slot-value schema 'min-length))
                            (and (slot-boundp schema 'max-length)
                                 (slot-value schema 'max-length)))))

  (unless (or (not (slot-boundp schema 'pattern))
              (ppcre:scan (slot-value schema 'pattern) value))
    (error 'validation-failed
           :value value
           :schema schema
           :message (format nil "Not match to ~S"
                            (slot-value schema 'pattern))))

  t)


;;
;; Array Type

(defmethod validate-data (value (schema array))
  (unless (and (or (not (slot-boundp schema 'min-items))
                   (<= (slot-value schema 'min-items) (length value)))
               (or (not (slot-boundp schema 'max-items))
                   (<= (length value) (slot-value schema 'max-items))))
    (error 'validation-failed
           :value value
           :schema schema
           :message (format nil "The length not in the range~@[ from ~A~]~@[ to ~A~]"
                            (and (slot-boundp schema 'min-items)
                                 (slot-value schema 'min-items))
                            (and (slot-boundp schema 'max-items)
                                 (slot-value schema 'max-items)))))

  (when (and (slot-boundp schema 'unique-items)
             (slot-value schema 'unique-items))
    (unless (= (length (remove-duplicates value :test #'equal))
               (length value))
      (error 'validation-failed
             :value value
             :schema schema
             :message "The items are not unique"))))


;;
;; Object Type

(defmethod validate-data (value (schema object))
  (loop for (key . field-value) in value
        for prop = (find key (slot-value schema 'properties)
                         :key (lambda (x) (slot-value x 'name))
                         :test #'equal)
        do (unless prop
             (error 'validation-failed
                    :value value
                    :schema schema
                    :message (format nil "Undefined property: ~S" key)))
           (if field-value
               (handler-case (validate-data field-value (slot-value prop 'type))
                 (validation-failed (e)
                   (error 'validation-failed
                          :value value
                          :schema schema
                          :message (format nil "Validation failed at ~S:~%  ~S"
                                           key
                                           (slot-value e 'message)))))
               (unless (and (slot-boundp prop 'nullable)
                            (slot-value prop 'nullable))
                 (error 'validation-failed
                        :value value
                        :schema schema
                        :message (format nil "~S is not nullable" key)))))
  (loop for key in (and (slot-boundp schema 'required)
                        (slot-value schema 'required))
        unless (find key value :key #'car :test #'equal)
          collect key into missing-keys
        finally
           (error 'validation-failed
                  :value value
                  :schema schema
                  :message (format nil "Missing required keys: ~S" missing-keys)))
  (unless (and (or (not (slot-boundp schema 'min-properties))
                   (nthcdr (slot-value schema 'min-properties) value))
               (or (not (slot-boundp schema 'max-properties))
                   (nthcdr (slot-value schema 'max-properties) value)))
    (error 'validation-failed
           :value value
           :schema schema
           :message
           (format nil "The number of properties has to be in the range of~@[ ~A <=~] (length properties)~@[ <= ~A~]"
                   (and (slot-boundp schema 'min-properties)
                        (slot-value schema 'min-properties))
                   (and (slot-boundp schema 'max-properties)
                        (slot-value schema 'max-properties)))))
  t)
