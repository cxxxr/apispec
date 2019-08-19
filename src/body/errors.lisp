(defpackage #:apispec/body/errors
  (:use #:cl
        #:apispec/errors)
  (:export #:body-parse-error))
(in-package #:apispec/body/errors)

(define-condition body-parse-error (apispec-error)
  ((value :initarg :value)
   (content-type :initarg :content-type))
  (:report (lambda (condition stream)
             (with-slots (value content-type) condition
               (format stream "Failed to parse ~A as Content-Type '~A'"
                       value content-type)))))
