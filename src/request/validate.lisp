(defpackage #:apispec/request/validate
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/request/parameter
                #:parameter
                #:parameter-in
                #:parameter-name
                #:parameter-required-p
                #:parameter-style
                #:parameter-explode-p
                #:parameter-schema)
  (:import-from #:apispec/encoding
                #:parse-with-media-type
                #:parse-complex-string
                #:parse-complex-parameters)
  (:import-from #:apispec/request/request-body
                #:request-body
                #:request-body-media-type)
  (:import-from #:apispec/schema
                #:coerce-data)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:quri
                #:url-decode-params)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:validate-request))
(in-package #:apispec/request/validate)

(defun parse-cookie-string (cookie)
  (when cookie
    (loop for kv in (ppcre:split "\\s*[,;]\\s*" cookie)
          append (quri:url-decode-params kv :lenient t))))

(defun validate-request (headers path-parameters query-string raw-body
                         parameters request-body)
  (check-type headers hash-table)
  (check-type path-parameters (association-list string string))
  (check-type query-string (or string null))
  (check-type raw-body (or stream null))
  (check-type parameters (proper-list parameter))
  (check-type request-body (or request-body null))

  (append
   (loop with empty = '#:empty
         with cookies = (parse-cookie-string (gethash "cookie" headers))
         with query-parameters = (and query-string
                                      (quri:url-decode-params query-string :lenient t))
         for parameter in parameters
         for in = (parameter-in parameter)
         for value = (cond
                       ((equal in "path")
                        (let ((path-val (aget path-parameters (parameter-name parameter) empty)))
                          (if (eq path-val empty)
                              empty
                              (parse-complex-string path-val
                                                    (parameter-style parameter)
                                                    (parameter-explode-p parameter)
                                                    (parameter-schema parameter)))))
                       ((equal in "query")
                        (if (eq empty (aget query-parameters (parameter-name parameter) empty))
                            empty
                            (parse-complex-parameters query-parameters
                                                      (parameter-name parameter)
                                                      (parameter-style parameter)
                                                      (parameter-explode-p parameter)
                                                      (parameter-schema parameter))))
                       ((equal in "header")
                        (let ((header-val (gethash (parameter-name parameter) headers empty)))
                          (if (eq header-val empty)
                              empty
                              (parse-complex-string header-val
                                                    (parameter-style parameter)
                                                    (parameter-explode-p parameter)
                                                    (parameter-schema parameter)))))
                       ((equal in "cookie")
                        (if (eq empty (aget cookies (parameter-name parameter) empty))
                            empty
                            (parse-complex-parameters cookies
                                                      (parameter-name parameter)
                                                      (parameter-style parameter)
                                                      (parameter-explode-p parameter)
                                                      (parameter-schema parameter))))
                       (t (error "Unexpected in: ~S" in)))
         if (eq value empty)
           collect (cons (parameter-name parameter)
                         (if (parameter-required-p parameter)
                             (error "Missing parameter: ~S" (parameter-name parameter))
                             nil))
         else
           collect (cons (parameter-name parameter)
                         (coerce-data value (parameter-schema parameter))))
   (let ((content-type (gethash "content-type" headers)))
     (when (and request-body
                raw-body
                content-type)
       (let ((media-type (request-body-media-type request-body content-type)))
         (parse-with-media-type raw-body media-type
                                content-type (gethash "content-length" headers)))))))
