(defpackage #:apispec/classes/schema/errors
  (:use #:cl
        #:apispec/errors)
  (:export #:schema-error
           #:schema-coercion-failed
           #:schema-coercion-failed-value
           #:schema-coercion-failed-schema
           #:schema-object-error
           #:schema-object-error-missing-keys
           #:schema-object-error-invalid-keys
           #:schema-object-error-unpermitted-keys
           #:schema-object-error-value
           #:schema-object-error-schema
           #:schema-validation-failed
           #:schema-oneof-error
           #:schema-anyof-error
           #:schema-allof-error))
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
  ((missing-keys :initarg :missing-keys
                 :reader schema-object-error-missing-keys)
   (invalid-keys :initarg :invalid-keys
                 :reader schema-object-error-invalid-keys)
   (unpermitted-keys :initarg :unpermitted-keys
                     :reader schema-object-error-unpermitted-keys)
   (value :initarg :value
          :reader schema-object-value)
   (schema :initarg :schema
           :reader schema-object-schema)))

(define-condition schema-multiple-error (schema-error)
  ((subschemas
    :initarg :subschemas
    :reader schema-multiple-error-subschemas)))

(define-condition schema-oneof-error (schema-multiple-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "Multiple schemas are possible for oneOf composition schema: ~{~A~^ ~}"
                     (schema-multiple-error-subschemas condition)))))

(define-condition schema-anyof-error (schema-multiple-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "Every schemas aren't possible for anyOf composition schema: ~{~A~^ ~}"
                     (schema-multiple-error-subschemas condition)))))

(define-condition schema-allof-error (schema-multiple-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "Possible for negative schema"
                     (schema-multiple-error-subschemas condition)))))
