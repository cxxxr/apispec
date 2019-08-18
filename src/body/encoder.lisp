(defpackage #:apispec/body/encoder
  (:use #:cl)
  (:import-from #:apispec/body/encoder/json
                #:encode-data-to-json)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:encode-data))
(in-package #:apispec/body/encoder)

(defun encode-data (value schema content-type)
  (check-type content-type string)
  (cond
    ((starts-with-subseq "application/json" content-type)
     (with-output-to-string (*standard-output*)
       (encode-data-to-json value schema)))
    (t value)))
