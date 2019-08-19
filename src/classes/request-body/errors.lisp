(defpackage #:apispec/classes/request-body/errors
  (:use #:cl
        #:apispec/errors)
  (:export #:request-body-error
           #:request-body-content-type-mismatch
           #:request-body-parse-error
           #:request-body-validation-failed))
(in-package #:apispec/classes/request-body/errors)

(define-condition request-body-error (apispec-error) ())

(define-condition request-body-content-type-mismatch (request-body-error)
  ((given :initarg :given)
   (expected :initarg :expected))
  (:report (lambda (condition stream)
             (with-slots (given expected) condition
               (format stream "The Content-Type must be one of ~{'~A'~^, ~}, but it's '~A'"
                       expected given)))))

(define-condition request-body-parse-error (request-body-error)
  ((content-type :initarg :content-type))
  (:report (lambda (condition stream)
             (format stream "The request body is invalid for '~A'"
                     (slot-value condition 'content-type)))))

(define-condition request-body-validation-failed (request-body-error)
  ((value :initarg :value)
   (schema :initarg :schema))
  (:report (lambda (condition stream)
             (with-slots (value schema) condition
               (format stream "~S is invalid for ~A"
                       value schema)))))
