(defpackage #:apispec/body/errors
  (:use #:cl
        #:apispec/errors)
  (:export #:body-parse-error
           #:body-encode-error
           #:body-encode-object-error))
(in-package #:apispec/body/errors)

(define-condition body-parse-error (apispec-error)
  ((value :initarg :value)
   (content-type :initarg :content-type))
  (:report (lambda (condition stream)
             (with-slots (value content-type) condition
               (format stream "Failed to parse ~A as Content-Type '~A'"
                       value content-type)))))

(define-condition body-encode-error (apispec-error)
  ((value :initarg :value)
   (schema :initarg :schema))
  (:report (lambda (condition stream)
             (with-slots (value schema missing unpermitted) condition
               (format stream "~S is invalid for ~A"
                       value schema)))))

(define-condition body-encode-object-error (body-encode-error)
  ((missing :initarg :missing
            :initform nil)
   (unpermitted :initarg :unpermitted
                :initform nil))
  (:report (lambda (condition stream)
             (with-slots (value schema missing unpermitted) condition
               (format stream "~S is invalid for ~A~@[:~%  missing: ~{~A~^, ~}~]~@[~%  unpermitted: ~{~A~^, ~}~]"
                       value schema
                       missing unpermitted)))))
