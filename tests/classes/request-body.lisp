(defpackage #:apispec/tests/classes/request-body
  (:use #:cl
        #:rove
        #:apispec/classes/request-body)
  (:import-from #:apispec/classes/media-type
                #:media-type)
  (:import-from #:apispec/classes/encoding
                #:encoding)
  (:import-from #:apispec/classes/schema
                #:schema
                #:object)
  (:import-from #:cl-interpol)
  (:import-from #:flexi-streams)
  (:import-from #:babel)
  (:import-from #:assoc-utils
                #:alist=))
(in-package #:apispec/tests/classes/request-body)

(named-readtables:in-readtable :interpol-syntax)

(defun make-stream (data)
  (flex:make-in-memory-input-stream (babel:string-to-octets data)))

(defvar *multipart-data*
  (concatenate 'string
               #?"-----------------------------186454651713519341951581030105\r\n"
               #?"Content-Disposition: form-data; name=\"id\"\r\n"
               #?"Content-Type: text/plain\r\n"
               #?"\r\n"
               #?"1\r\n"
               #?"-----------------------------186454651713519341951581030105\r\n"
               #?"Content-Disposition: form-data; name=\"address\"\r\n"
               #?"Contest-Type: text/plain\r\n"
               #?"\r\n"
               #?"東京都台東区上野２丁目７−１２\r\n"
               #?"-----------------------------186454651713519341951581030105\r\n"
               #?"Content-Disposition: form-data; name=\"historyMetadata\"\r\n"
               #?"Content-Type: application/json\r\n"
               #?"\r\n"
               #?"{\"type\":\"culture\"}\r\n"
               #?"-----------------------------186454651713519341951581030105--\r\n"))

(deftest parse-request-body-tests
  (let* ((schema (schema
                   (object
                     (("id" integer)
                      ("address" string)
                      ("historyMetadata"
                       (object
                         (("type" string))))))))
         (request-body
           (make-instance 'request-body
                          :content `(("multipart/form-data"
                                      . ,(make-instance 'media-type :schema schema))
                                     ("application/json"
                                      . ,(make-instance 'media-type :schema schema))
                                     ("application/x-www-form-urlencoded"
                                      . ,(make-instance 'media-type
                                                        :schema schema
                                                        :encoding
                                                        `(("historyMetadata"
                                                           . ,(make-instance 'encoding
                                                                             :content-type "application/json")))))))))
    (ok (alist=
          (parse-request-body
            (make-stream *multipart-data*)
            "multipart/form-data; boundary=\"---------------------------186454651713519341951581030105\""
            nil
            request-body)
          '(("id" . 1)
            ("address" . "東京都台東区上野２丁目７−１２")
            ("historyMetadata" . (("type" . "culture"))))))
    (ok (alist=
          (parse-request-body
            (make-stream "{\"id\":1,\"address\":\"東京都台東区上野２丁目７−１２\",\"historyMetadata\":{\"type\":\"culture\"}}")
            "application/json"
            nil
            request-body)
          '(("id" . 1)
            ("address" . "東京都台東区上野２丁目７−１２")
            ("historyMetadata" . (("type" . "culture"))))))
    (ok (alist=
          (parse-request-body
            (make-stream
              (format nil "id=1&address=~A&historyMetadata=~A"
                      (quri:url-encode "東京都台東区上野２丁目７−１２")
                      (quri:url-encode "{\"type\":\"culture\"}")))
            "application/x-www-form-urlencoded"
            nil
            request-body)
          '(("id" . 1)
            ("address" . "東京都台東区上野２丁目７−１２")
            ("historyMetadata" . (("type" . "culture"))))))))

(deftest invalid-format-tests
  (ok (signals (parse-request-body
                 (make-stream "blah")
                 "multipart/form-data"
                 nil
                 (make-instance 'request-body
                                :content
                                `(("multipart/form-data" . ,(make-instance 'media-type :schema (schema object))))))
               'request-body-parse-error))
  (ok (signals (parse-request-body
                 (make-stream *multipart-data*)
                 "multipart/form-data; boundary=\"---------------------------186454651713519341951581030105\""
                 nil
                 (make-instance 'request-body
                                :content
                                `(("application/json" . ,(make-instance 'media-type :schema (schema object))))))
               'request-body-content-type-mismatch)))
