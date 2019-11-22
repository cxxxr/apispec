(defpackage #:apispec/classes/schema/errors
  (:use #:cl
        #:apispec/errors)
  (:export #:schema-error
           #:schema-coercion-failed
           #:schema-coercion-failed-value
           #:schema-coercion-failed-schema
           #:schema-object-coercion-failed
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

(define-condition schema-object-coercion-failed (schema-coercion-failed)
  ((key :initarg :key
        :reader schema-object-coercion-failed-key)))

(define-condition schema-object-unpermmited-key (schema-object-coercion-failed)
  ())

(define-condition schema-object-invalid-value (schema-object-coercion-failed)
  ())

(define-condition schema-validation-failed (schema-error validation-failed)
  ((value :initarg :value)
   (schema :initarg :schema)
   (message :initarg :message
            :initform nil))
  (:report (lambda (condition stream)
             (with-slots (value schema message) condition
               (format stream "~S is invalid for ~A~@[:~%  ~A~]"
                       value
                       schema
                       message)))))
