(defpackage #:apispec/classes/response/errors
  (:use #:cl
        #:apispec/errors)
  (:export #:response-error
           #:response-not-defined
           #:response-body-not-allowed
           #:response-header-validation-failed
           #:response-validation-failed))
(in-package #:apispec/classes/response/errors)

(define-condition response-error (apispec-error) ())

(define-condition response-not-defined (response-error)
  ((code :type (or string integer null)
         :initarg :code
         :initform nil)
   (content-type :type (or string null)
                 :initarg :content-type
                 :initform nil))
  (:report (lambda (condition stream)
             (with-slots (code content-type) condition
               (format stream "Response is not defined for~@[ code=~S~]~@[ content-type=~S~]"
                       code content-type)))))

(define-condition response-body-not-allowed (response-error)
  ((code :type (or string integer null)
         :initarg :code
         :initform nil))
  (:report (lambda (condition stream)
             (format stream "Response body is not allowed for HTTP status ~A"
                     (slot-value condition 'code)))))

(define-condition response-header-validation-failed (response-error)
  ((name :initarg :name)
   (value :initarg :value)
   (header :initarg :header))
  (:report (lambda (condition stream)
             (with-slots (name value header) condition
               (format stream "Header ~A=~S is invalid for ~A"
                       name value header)))))

(define-condition response-validation-failed (response-error)
  ((value :initarg :value)
   (schema :initarg :schema)
   (content-type :initarg :content-type)
   (reason :initarg :reason))
  (:report (lambda (condition stream)
             (with-slots (value schema content-type reason) condition
               (format stream "~S is invalid for ~A to encode to '~A':~%  ~A"
                       value schema content-type reason)))))
