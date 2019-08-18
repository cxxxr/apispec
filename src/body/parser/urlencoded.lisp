(defpackage #:apispec/body/parser/urlencoded
  (:use #:cl)
  (:import-from #:apispec/utils
                #:slurp-stream)
  (:import-from #:quri
                #:url-decode-params)
  (:export #:parse-urlencoded-stream
           #:parse-urlencoded-string))
(in-package #:apispec/body/parser/urlencoded)

(defun parse-urlencoded-stream (stream)
  (url-decode-params (slurp-stream stream) :lenient t))

(defun parse-urlencoded-string (string)
  (url-decode-params string :lenient t))
