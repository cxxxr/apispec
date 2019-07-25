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

                          #:multiple-of
                          #:maximum
                          #:exclusive-maximum
                          #:minimum
                          #:exclusive-minimum
                          #:max-length
                          #:min-length
                          #:pattern)
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
