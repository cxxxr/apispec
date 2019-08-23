(defpackage #:apispec/body/parser/json
  (:use #:cl)
  (:import-from #:apispec/utils
                #:detect-charset
                #:slurp-stream)
  (:import-from #:jonathan)
  (:import-from #:babel)
  (:export #:parse-json-stream
           #:parse-json-string))
(in-package #:apispec/body/parser/json)

(defun parse-json-stream (stream content-type content-length)
  (jojo:parse
    (babel:octets-to-string (slurp-stream stream content-length)
                            :encoding (detect-charset content-type :utf-8))
    :as :alist))

(defun parse-json-string (string content-type)
  (declare (ignore content-type))
  (jojo:parse string :as :alist))
