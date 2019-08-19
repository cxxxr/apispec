(defpackage #:apispec/errors
  (:use #:cl)
  (:export #:apispec-error
           #:input-error
           #:coercion-failed
           #:validation-failed
           #:complex-parse-failed
           #:read-new-value))
(in-package #:apispec/errors)

;;
;; Base Error Class

(define-condition apispec-error (error) ())

;;
;; Input Error

(define-condition input-error (apispec-error) ())

(define-condition coercion-failed (input-error) ())

(define-condition validation-failed (input-error) ())

(define-condition complex-parse-failed (input-error) ())

;;
;; For restarting

(defun read-new-value ()
  (format t "Enter a new value: ")
  (eval (read)))
