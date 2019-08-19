(defpackage #:apispec/classes/schema/errors
  (:use #:cl
        #:apispec/errors)
  (:export #:schema-error
           #:schema-coercion-failed
           #:schema-validation-failed))
(in-package #:apispec/classes/schema/errors)

(define-condition schema-error (apispec-error) ())

(define-condition schema-coercion-failed (schema-error coercion-failed)
  ((value :initarg :value)
   (schema :initarg :schema))
  (:report (lambda (condition stream)
             (with-slots (value schema) condition
               (format stream "~S cannot be coerced to ~A"
                       value
                       schema)))))

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
