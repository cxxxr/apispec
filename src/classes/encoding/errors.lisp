(defpackage #:apispec/classes/encoding/errors
  (:use #:cl
        #:apispec/errors)
  (:export #:encoding-error
           #:encoding-content-type-mismatch
           #:encoding-header-missing))
(in-package #:apispec/classes/encoding/errors)

(define-condition encoding-error (apispec-error) ())

(define-condition encoding-content-type-mismatch (encoding-error)
  ((given :initarg :given)
   (expected :initarg :expected)))

(define-condition encoding-header-missing (encoding-error)
  ((header :initarg :header))
  (:report (lambda (condition stream)
             (format stream "Header '~A' is missing" (slot-value condition 'header)))))
