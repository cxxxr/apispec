(defpackage #:apispec/body/encoder
  (:use #:cl)
  (:import-from #:apispec/body/encoder/json
                #:encode-data-to-json)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:encode-data
           #:encode-object))
(in-package #:apispec/body/encoder)

(defun encode-data (value schema content-type)
  (check-type content-type string)
  (cond
    ((starts-with-subseq "application/json" content-type)
     (with-output-to-string (*standard-output*)
       (encode-data-to-json (encode-object value) schema)))
    (t (encode-object value))))

(defgeneric encode-object (value)
  (:method (value)
    (typecase value
      ((or standard-object
           structure-object) (call-next-method))
      (otherwise value))))
