(defpackage #:apispec/request/validate
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/parameter
                #:parameter
                #:parameter-in
                #:parameter-name
                #:parameter-required-p
                #:parameter-style
                #:parameter-explode-p
                #:parameter-schema)
  (:import-from #:apispec/classes/request-body
                #:request-body
                #:find-request-body-media-type)
  (:import-from #:apispec/complex
                #:parse-complex-string
                #:parse-complex-parameter)
  (:import-from #:apispec/classes/media-type
                #:parse-with-media-type)
  (:import-from #:apispec/classes/schema
                #:coerce-data)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:quri
                #:url-decode-params)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:request-validation-failed
           #:validate-request))
(in-package #:apispec/request/validate)

(define-condition request-validation-failed (error)
  ((message :type string
            :initarg :message))
  (:report (lambda (condition stream)
             (princ (slot-value condition 'message) stream))))

(defun parse-cookie-string (cookie)
  (when cookie
    (loop for kv in (ppcre:split "\\s*[,;]\\s*" cookie)
          append (quri:url-decode-params kv :lenient t))))

(defun validate-request (headers path-parameters query-string raw-body
                         parameters request-body)
  (check-type headers (or hash-table null))
  (check-type path-parameters (association-list string string))
  (check-type query-string (or string null))
  (check-type raw-body (or stream null))
  (check-type parameters (proper-list parameter))
  (check-type request-body (or request-body null))

  (unless headers
    (setf headers (make-hash-table)))

  (append
   (loop with empty = '#:empty
         with cookies = (parse-cookie-string (gethash "cookie" headers))
         with query-parameters = (and query-string
                                      (quri:url-decode-params query-string :lenient t))
         for parameter in parameters
         for in = (parameter-in parameter)
         for value = (cond
                       ((string= in "path")
                        (let ((path-val (aget path-parameters (parameter-name parameter) empty)))
                          (if (eq path-val empty)
                              empty
                              (parse-complex-string path-val
                                                    (parameter-style parameter)
                                                    (parameter-explode-p parameter)
                                                    (parameter-schema parameter)))))
                       ((string= in "query")
                        (if (eq empty (aget query-parameters (parameter-name parameter) empty))
                            empty
                            (parse-complex-parameter query-parameters
                                                     (parameter-name parameter)
                                                     (parameter-style parameter)
                                                     (parameter-explode-p parameter)
                                                     (parameter-schema parameter))))
                       ((string= in "header")
                        (let ((header-val (gethash (parameter-name parameter) headers empty)))
                          (if (eq header-val empty)
                              empty
                              (parse-complex-string header-val
                                                    (parameter-style parameter)
                                                    (parameter-explode-p parameter)
                                                    (parameter-schema parameter)))))
                       ((string= in "cookie")
                        (if (eq empty (aget cookies (parameter-name parameter) empty))
                            empty
                            (parse-complex-parameter cookies
                                                     (parameter-name parameter)
                                                     (parameter-style parameter)
                                                     (parameter-explode-p parameter)
                                                     (parameter-schema parameter))))
                       (t (error "Unexpected in: ~S" in)))
         if (eq value empty)
           collect (cons (parameter-name parameter)
                         (if (parameter-required-p parameter)
                             (error 'request-validation-failed
                                    :message (format nil "Missing parameter: ~S" (parameter-name parameter)))
                             nil))
         else
           collect (cons (parameter-name parameter)
                         (coerce-data value (parameter-schema parameter))))
   (let ((content-type (gethash "content-type" headers)))
     (when (and request-body
                raw-body
                content-type)
       (let ((media-type (find-request-body-media-type request-body content-type)))
         (unless media-type
           (error 'request-validation-failed
                  :message (format nil "Request body Content-Type ~S is not allowed" content-type)))
         (parse-with-media-type raw-body media-type content-type))))))
