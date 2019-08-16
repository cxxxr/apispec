(defpackage #:apispec/classes/parameter/parse
  (:use #:cl
        #:apispec/utils
        #:apispec/classes/parameter/class)
  (:import-from #:apispec/classes/schema
                #:coerce-data
                #:*coerce-integer-string-to-boolean*)
  (:import-from #:apispec/complex
                #:parse-complex-string
                #:parse-complex-parameter)
  (:import-from #:quri
                #:url-decode-params)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parameter-validation-failed
           #:parse-query-string
           #:parse-path-parameters
           #:parse-headers
           #:parse-cookie-string))
(in-package #:apispec/classes/parameter/parse)

(define-condition parameter-validation-failed (error)
  ((message :type string
            :initarg :message))
  (:report (lambda (condition stream)
             (princ (slot-value condition 'message) stream))))

(defvar *empty* '#:empty)

(defun parse-query-string (query-string parameters)
  (check-type query-string (or null string))
  (assert (proper-list-p parameters 'query-parameter))

  (when query-string
    (let ((query-parameters (quri:url-decode-params query-string :lenient t)))
      (loop for parameter in parameters
            for value = (aget query-parameters (parameter-name parameter) *empty*)
            if (eq value *empty*)
            collect (cons (parameter-name parameter)
                          (if (parameter-required-p parameter)
                              (error 'parameter-validation-failed
                                     :message (format nil "Query parameter ~S is required but missing"
                                                      (parameter-name parameter)))
                              nil))
            else
            collect (cons (parameter-name parameter)
                          (parse-complex-parameter query-parameters
                                                   (parameter-name parameter)
                                                   (parameter-style parameter)
                                                   (parameter-explode-p parameter)
                                                   (parameter-schema parameter)))))))

(defun parse-path-parameters (path-parameters parameters)
  (assert (association-list-p path-parameters 'string 'string))
  (assert (proper-list-p parameters 'path-parameter))

  (when path-parameters
    (loop for parameter in parameters
          for value = (aget path-parameters (parameter-name parameter) *empty*)
          if (eq value *empty*)
          do (error 'parameter-validation-failed
                    :message (format nil "Path parameter ~S is missing"
                                     (parameter-name parameter)))
          else
          collect (cons (parameter-name parameter)
                        (coerce-data
                          (parse-complex-string value
                                                (parameter-style parameter)
                                                (parameter-explode-p parameter)
                                                (parameter-schema parameter))
                          (parameter-schema parameter))))))

(defun parse-headers (headers parameters)
  (check-type headers (or hash-table null))
  (assert (proper-list-p parameters 'header-parameter))

  (when headers
    (loop for parameter in parameters
          for value = (gethash (string-downcase (parameter-name parameter)) headers *empty*)
          if (eq value *empty*)
          collect (cons (parameter-name parameter)
                        (if (parameter-required-p parameter)
                            (error 'parameter-validation-failed
                                   :message (format nil "Header ~S is required but missing"
                                                    (parameter-name parameter)))
                            nil))
          else
          collect (cons (parameter-name parameter)
                        (coerce-data
                          (parse-complex-string value
                                                (parameter-style parameter)
                                                (parameter-explode-p parameter)
                                                (parameter-schema parameter))
                          (parameter-schema parameter))))))

(defun decode-cookie-params (cookie-string)
  (loop for part in (ppcre:split "\\s*;\\s*" cookie-string)
        for (key value) = (ppcre:split "=" part :limit 2)
        collect (cons key value)))

(defun parse-cookie-string (cookie-string parameters)
  (check-type cookie-string (or string null))
  (assert (proper-list-p parameters 'cookie-parameter))

  (when cookie-string
    (let ((cookies (decode-cookie-params cookie-string)))
      (loop for parameter in parameters
            for value = (aget cookies (parameter-name parameter) *empty*)
            if (eq value *empty*)
            collect (cons (parameter-name parameter)
                          (if (parameter-required-p parameter)
                              (error 'parameter-validation-failed
                                     :message (format nil "Cookie ~S is required but missing"
                                                      (parameter-name parameter)))
                              nil))
            else
            collect (cons (parameter-name parameter)
                          (let ((*coerce-integer-string-to-boolean* t))
                            (coerce-data
                              (parse-complex-parameter cookies
                                                       (parameter-name parameter)
                                                       (parameter-style parameter)
                                                       (parameter-explode-p parameter)
                                                       (parameter-schema parameter))
                              (parameter-schema parameter))))))))
