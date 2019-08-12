(defpackage #:apispec/body/json
  (:use #:cl)
  (:import-from #:apispec/utils
                #:detect-charset
                #:slurp-stream)
  (:import-from #:jonathan)
  (:import-from #:babel)
  (:export #:parse-json-stream
           #:parse-json-string))
(in-package #:apispec/body/json)

(defun parse-json-stream (stream content-type)
  (jojo:parse
    (babel:octets-to-string (slurp-stream stream)
                            :encoding (detect-charset content-type :utf-8))
    :as :alist))

(defun parse-json-string (string content-type)
  (declare (ignore content-type))
  (jojo:parse string :as :alist))
