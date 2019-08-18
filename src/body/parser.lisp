(defpackage #:apispec/body/parser
  (:use #:cl
        #:apispec/body/parser/json
        #:apispec/body/parser/urlencoded
        #:apispec/body/parser/multipart)
  (:import-from #:apispec/utils
                #:detect-charset
                #:slurp-stream)
  (:import-from #:babel)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:parse-body))
(in-package #:apispec/body/parser)

(defun parse-body (value content-type)
  (check-type value (or string stream))
  (check-type content-type string)
  (cond
    ((starts-with-subseq "application/json" (string-downcase content-type))
     (etypecase value
       (string (parse-json-string value content-type))
       (stream (parse-json-stream value content-type))))
    ((starts-with-subseq "application/x-www-form-urlencoded" (string-downcase content-type))
     (etypecase value
       (string (parse-urlencoded-string value))
       (stream (parse-urlencoded-stream value))))
    ((starts-with-subseq "multipart/" (string-downcase content-type))
     (etypecase value
       (string (parse-multipart-string value content-type))
       (stream (parse-multipart-stream value content-type))))
    ((starts-with-subseq "application/octet-stream" (string-downcase content-type))
     (slurp-stream value))
    (t
     (etypecase value
       (string value)
       (stream (babel:octets-to-string (slurp-stream value)
                                       :encoding (detect-charset content-type)))))))
