(defpackage #:apispec/classes/parameter/parse
  (:use #:cl
        #:apispec/utils
        #:apispec/classes/parameter/class
        #:apispec/classes/parameter/errors)
  (:import-from #:apispec/classes/schema
                #:coerce-data
                #:*coerce-string-to-boolean*)
  (:import-from #:apispec/complex
                #:parse-complex-string
                #:parse-complex-parameter)
  (:import-from #:apispec/errors
                #:apispec-error)
  (:import-from #:quri
                #:url-decode-params)
  (:import-from #:assoc-utils
                #:aget
                #:delete-from-alist)
  (:export #:parse-query-string
           #:parse-path-parameters
           #:parse-headers
           #:parse-cookie-string))
(in-package #:apispec/classes/parameter/parse)

(defvar *empty* '#:empty)

(defun parse-query-string (query-string parameters)
  (check-type query-string (or null string))
  (assert (proper-list-p parameters 'query-parameter))

  (let ((query-parameters (and query-string
                               (handler-case
                                   (quri:url-decode-params query-string :lenient t)
                                 ((or quri:uri-malformed-urlencoded-string
                                      quri:url-decoding-error) ()
                                   (error 'parameter-parse-failed
                                          :value query-string)))))
        results missing invalid)
    (dolist (parameter parameters)
      (let* ((name (parameter-name parameter))
             (value (aget query-parameters name *empty*)))
        (cond
          ((eq value *empty*)
           (when (parameter-required-p parameter)
             (push name missing))
           #+(or)
           (push (cons name nil) results))
          (t
           (let ((parsed-value
                   (handler-case
                       (parse-complex-parameter query-parameters
                                                name
                                                (parameter-style parameter)
                                                (parameter-explode-p parameter)
                                                (parameter-schema parameter))
                     (apispec-error (e)
                       (push (cons name e) invalid)
                       nil))))
             (push (cons name parsed-value) results)
             (setf query-parameters (delete-from-alist query-parameters name)))))))
    (when (or missing invalid query-parameters)
      (error 'parameter-validation-failed
             :in "query"
             :missing (nreverse missing)
             :unpermitted (mapcar #'car query-parameters)
             :invalid (nreverse invalid)))
    (nreverse results)))

(defun parse-path-parameters (path-parameters parameters)
  (assert (association-list-p path-parameters 'string 'string))
  (assert (proper-list-p parameters 'path-parameter))

  (let (results missing invalid)
    (dolist (parameter parameters)
      (let* ((name (parameter-name parameter))
             (value (aget path-parameters name *empty*)))
        (cond
          ((eq value *empty*)
           (push name missing))
          (t
           (push (cons (parameter-name parameter)
                       (handler-case
                           (coerce-data
                             (parse-complex-string value
                                                   (parameter-style parameter)
                                                   (parameter-explode-p parameter)
                                                   (parameter-schema parameter))
                             (parameter-schema parameter))
                         (apispec-error (e)
                           (push (cons name e) invalid)
                           nil)))
                 results)
           (setf path-parameters (delete-from-alist path-parameters name))))))
    (when (or missing invalid path-parameters)
      (error 'parameter-validation-failed
             :in "path"
             :missing (nreverse missing)
             :unpermitted (mapcar #'car path-parameters)
             :invalid (nreverse invalid)))
    (nreverse results)))

(defun parse-headers (headers parameters)
  (check-type headers (or hash-table null))
  (assert (proper-list-p parameters 'header-parameter))

  (let ((headers (or headers (make-hash-table)))
        results missing invalid)
    (dolist (parameter parameters)
      (let* ((name (parameter-name parameter))
             (value (gethash (string-downcase name) headers *empty*)))
        (cond
          ((eq value *empty*)
           (when (parameter-required-p parameter)
             (push name missing))
           (push (cons name nil) results))
          (t
           (push (cons (parameter-name parameter)
                       (handler-case
                           (coerce-data
                             (parse-complex-string value
                                                   (parameter-style parameter)
                                                   (parameter-explode-p parameter)
                                                   (parameter-schema parameter))
                             (parameter-schema parameter))
                         (apispec-error (e)
                           (push (cons name e) invalid)
                           nil)))
                 results)))))
    (when (or missing invalid)
      (error 'parameter-validation-failed
             :in "header"
             :missing (nreverse missing)
             :invalid (nreverse invalid)))
    (nreverse results)))

(defun decode-cookie-params (cookie-string)
  (loop for part in (ppcre:split "\\s*;\\s*" cookie-string)
        for (key value) = (ppcre:split "=" part :limit 2)
        collect (cons key value)))

(defun parse-cookie-string (cookie-string parameters)
  (check-type cookie-string (or string null))
  (assert (proper-list-p parameters 'cookie-parameter))

  (let ((cookies (and cookie-string
                      (decode-cookie-params cookie-string)))
        results missing invalid)
    (dolist (parameter parameters)
      (let* ((name (parameter-name parameter))
             (value (aget cookies name *empty*)))
        (cond
          ((eq value *empty*)
           (when (parameter-required-p parameter)
             (push name missing))
           (push (cons name nil) results))
          (t
           (push (cons (parameter-name parameter)
                       (let ((*coerce-string-to-boolean* t))
                         (handler-case
                             (coerce-data
                               (parse-complex-parameter cookies
                                                        (parameter-name parameter)
                                                        (parameter-style parameter)
                                                        (parameter-explode-p parameter)
                                                        (parameter-schema parameter))
                               (parameter-schema parameter))
                           (apispec-error (e)
                             (push (cons name e) invalid)
                             nil))))
                 results)))))
    (when (or missing invalid)
      (error 'parameter-validation-failed
             :missing (nreverse missing)
             :invalid (nreverse invalid)))
    (nreverse results)))
