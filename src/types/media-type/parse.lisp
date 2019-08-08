(defpackage #:apispec/types/media-type/parse
  (:use #:cl)
  (:import-from #:apispec/types/media-type/class
                #:media-type
                #:media-type-encoding
                #:media-type-schema)
  (:import-from #:apispec/types/encoding
                #:encoding-style
                #:encoding-explode-p
                #:parse-complex-string)
  (:import-from #:apispec/types/schema
                #:object-properties
                #:property-name
                #:coerce-data)
  (:import-from #:babel
                #:octets-to-string)
  (:import-from #:http-body)
  (:import-from #:http-body.util
                #:detect-charset
                #:slurp-stream)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-with-media-type))
(in-package #:apispec/types/media-type/parse)

(defun parse-with-media-type (stream media-type content-type content-length)
  (check-type media-type media-type)
  (check-type content-type string)
  (check-type content-length (or integer null))
  (let* ((value
           (multiple-value-bind (parsed-value success)
               (http-body:parse content-type nil stream)
             (if success
                 parsed-value
                 (slurp-stream stream content-length))))
         (value (if (starts-with-subseq "text/" content-type)
                    (babel:octets-to-string value
                                            :encoding (detect-charset content-type))
                    value)))
    (coerce-data
      (if (and (media-type-encoding media-type)
               (or (starts-with-subseq "application/x-www-form-urlencoded" content-type)
                   (starts-with-subseq "multipart/form-data" content-type)))
          (mapc (lambda (pair)
                  (let ((encoding (aget (media-type-encoding media-type) (car pair)))
                        (schema (and (media-type-schema media-type)
                                     (find (car pair) (object-properties (media-type-schema media-type))
                                           :key #'property-name
                                           :test #'equal))))
                    (when encoding
                      (setf (cdr pair)
                            (parse-complex-string (cdr pair)
                                                  (encoding-style encoding)
                                                  (encoding-explode-p encoding)
                                                  schema)))))
                value)
          value)
      (or (media-type-schema media-type) t))))
