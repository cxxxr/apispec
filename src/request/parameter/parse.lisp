(defpackage #:apispec/request/parameter/parse
  (:use #:cl)
  (:import-from #:apispec/request/parameter/core
                #:parameter-name
                #:parameter-style
                #:parameter-schema
                #:parameter-explode-p)
  (:import-from #:apispec/request/encoding
                #:parse-complex-string)
  (:shadowing-import-from #:apispec/schema
                          #:coerce-data)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-with-parameter))
(in-package #:apispec/request/parameter/parse)

(define-condition parse-failed (error)
  ((message :initarg :message))
  (:report (lambda (condition stream)
             (with-slots (message) condition
               (princ message stream)))))

(defun parse-with-parameter (value parameter)
  (let ((style (parameter-style parameter))
        (name (parameter-name parameter))
        (empty '#:empty))
    (let* ((parsed-value (parse-complex-string value
                                               style
                                               (parameter-explode-p parameter)
                                               (parameter-schema parameter)))
           (result
             (if (member style '("label" "simple" "spaceDelimited" "pipeDelimited")
                         :test #'string=)
                 (or parsed-value empty)
                 (aget parsed-value name empty))))
      (if (eq result empty)
          (error 'parse-failed
                 :message (format nil "Missing parameter: ~S" name))
          (coerce-data result (parameter-schema parameter))))))
