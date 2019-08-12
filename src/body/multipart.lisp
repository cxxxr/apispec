(defpackage #:apispec/body/multipart
  (:use #:cl)
  (:import-from #:fast-http
                #:make-multipart-parser)
  (:import-from #:flexi-streams)
  (:import-from #:babel)
  (:import-from #:cl-utilities
                #:with-collectors)
  (:export #:parse-multipart-stream
           #:parse-multipart-string))
(in-package #:apispec/body/multipart)

(defun parse-multipart-stream (stream content-type)
  (check-type stream stream)
  (check-type content-type string)
  (with-collectors (collect-body collect-headers)
    (let ((parser (make-multipart-parser
                    content-type
                    (lambda (name headers field-meta body)
                      (let ((content-type (gethash "content-type" headers)))
                        (collect-body (cons name body))
                        (collect-headers (cons name headers)))))))
      (loop with buffer = (make-array 1024 :element-type '(unsigned-byte 8))
            for read-bytes = (read-sequence buffer stream)
            do (funcall parser (subseq buffer 0 read-bytes))
            while (= read-bytes 1024)))))

(defun parse-multipart-string (string content-type)
  (parse-multipart-stream
    (flex:make-in-memory-input-stream (babel:string-to-octets string))
    content-type))
