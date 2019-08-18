(defpackage #:apispec/body/parser/multipart
  (:use #:cl)
  (:import-from #:apispec/body/parser/json
                #:parse-json-stream)
  (:import-from #:apispec/body/parser/urlencoded
                #:parse-urlencoded-stream)
  (:import-from #:apispec/utils
                #:slurp-stream
                #:detect-charset)
  (:import-from #:fast-http
                #:make-multipart-parser)
  (:import-from #:flexi-streams)
  (:import-from #:babel)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:import-from #:cl-utilities
                #:with-collectors)
  (:export #:parse-multipart-stream
           #:parse-multipart-string
           #:*multipart-force-stream*))
(in-package #:apispec/body/parser/multipart)

(defvar *multipart-force-stream* t)

(defun parse-multipart-stream (stream content-type)
  (check-type stream stream)
  (check-type content-type string)
  (let ((results (with-collectors (collect-body collect-headers)
    (let ((parser (make-multipart-parser
                    content-type
                    (lambda (name headers field-meta body)
                      (declare (ignore field-meta))
                      (collect-body (cons name
                                          (if *multipart-force-stream*
                                              body
                                              (let ((content-type (gethash "content-type" headers)))
                                                (cond
                                                  ((starts-with-subseq "application/json" (string-downcase content-type))
                                                   (parse-json-stream body content-type))
                                                  ((starts-with-subseq "application/x-www-form-urlencoded" (string-downcase content-type))
                                                   (parse-urlencoded-stream body))
                                                  ((starts-with-subseq "multipart/" (string-downcase content-type))
                                                   (parse-multipart-stream body content-type))
                                                  ((starts-with-subseq "application/octet-stream" (string-downcase content-type))
                                                   body)
                                                  (t
                                                   (babel:octets-to-string (slurp-stream body)
                                                                           :encoding (detect-charset content-type))))))))
                      (collect-headers (cons name headers))))))
      (loop with buffer = (make-array 1024 :element-type '(unsigned-byte 8))
            for read-bytes = (read-sequence buffer stream)
            do (funcall parser (subseq buffer 0 read-bytes))
            while (= read-bytes 1024))))))
    (if (every (lambda (pair) (null (car pair))) results)
        (if (null (rest results))
            ;; Single multipart chunk
            (cdr (first results))
            (mapcar #'cdr results))
        results)))

(defun parse-multipart-string (string content-type)
  (parse-multipart-stream
    (flex:make-in-memory-input-stream (babel:string-to-octets string))
    content-type))
