(defpackage #:apispec/classes/parameter/errors
  (:use #:cl
        #:apispec/utils
        #:apispec/errors)
  (:import-from #:apispec/classes/schema
                #:schema-error)
  (:export #:parameter-error
           #:parameter-parse-failed
           #:parameter-validation-failed
           #:missing-parameters
           #:unpermitted-parameters
           #:invalid-parameters))
(in-package #:apispec/classes/parameter/errors)

(define-condition parameter-error (apispec-error) ())

(define-condition parameter-parse-failed (parameter-error)
  ((value :initarg :value))
  (:report (lambda (condition stream)
             (format stream "Failed to parse: '~A'" (slot-value condition 'value)))))

(define-condition parameter-validation-failed (parameter-error)
  ((in :type string
       :initarg :in)
   (missing :type (proper-list string)
            :initarg :missing
            :initform nil
            :reader missing-parameters)
   (unpermitted :type (proper-list string)
                :initarg :unpermitted
                :initform nil
                :reader unpermitted-parameters)
   (invalid :type (association-list string schema-error)
            :initarg :invalid
            :initform nil
            :reader invalid-parameters))
  (:report (lambda (condition stream)
             (with-slots (in missing unpermitted invalid)
                 condition
               (format stream "Invalid ~A parameters:" in)
               (when missing
                 (format stream "~%  missing: ~{~A~^, ~}" missing))
               (when unpermitted
                 (format stream "~%  unpermitted: ~{~A~^, ~}" unpermitted))
               (when invalid
                 (format stream "~%  invalid:")
                 (dolist (pair invalid)
                   (format stream "~%    ~A: ~A" (car pair) (cdr pair))))))))
