(defpackage #:apispec/classes/schema/errors
  (:use #:cl
        #:apispec/errors)
  (:export #:schema-error
           #:schema-coercion-failed
           #:schema-coercion-failed-value
           #:schema-coercion-failed-schema
           #:schema-object-error
           #:schema-object-unpermmited-key
           #:schema-object-invalid-value
           #:schema-validation-failed))
(in-package #:apispec/classes/schema/errors)

(define-condition schema-error (apispec-error) ())

(define-condition schema-coercion-failed (schema-error coercion-failed)
  ((value :initarg :value :reader schema-coercion-failed-value)
   (schema :initarg :schema :reader schema-coercion-failed-schema))
  (:report (lambda (condition stream)
             (with-slots (value schema) condition
               (format stream "~S cannot be coerced to ~A"
                       value
                       schema)))))

(define-condition schema-validation-failed (schema-error validation-failed)
  ((value :initarg :value
          :reader schema-validation-failed-value)
   (schema :initarg :schema
           :reader schema-validation-failed-schema)
   (message :initarg :message
            :initform nil))
  (:report (lambda (condition stream)
             (with-slots (value schema message) condition
               (format stream "~S is invalid for ~A~@[:~%  ~A~]"
                       value
                       schema
                       message)))))

(define-condition schema-object-error (schema-error)
  ((key :initarg :key
        :reader schema-object-error-key)
   (value :initarg :value
          :reader schema-object-value)
   (schema :initarg :schema
           :reader schema-object-schema)))

(define-condition schema-object-unpermmited-key (schema-object-error)
  ())

(define-condition schema-object-invalid-value (schema-object-error)
  ())
