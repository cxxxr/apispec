(defpackage #:apispec/types/media-type/parse
  (:use #:cl)
  (:import-from #:apispec/types/media-type/class
                #:media-type
                #:media-type-encoding
                #:media-type-schema)
  (:import-from #:apispec/types/encoding
                #:encoding-style
                #:encoding-explode-p
                #:parse-complex-string
                #:check-encoding)
  (:import-from #:apispec/types/schema
                #:binary
                #:object
                #:array-items
                #:object-properties
                #:property-name
                #:property-type
                #:coerce-data)
  (:shadowing-import-from #:apispec/types/schema
                          #:number
                          #:string
                          #:byte
                          #:array)
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

(defun default-content-type (schema)
  (etypecase schema
    ((or byte binary) "application/octet-stream")
    ((or number string boolean) "text/plain")
    (object "application/json")
    (array
      (default-content-type (array-items schema)))
    (null nil)))

(defun parse-with-media-type (stream media-type content-type &optional content-length)
  (check-type media-type media-type)
  (check-type content-type cl:string)
  (check-type content-length (or integer null))
  (let ((content-type (string-downcase content-type)))
    (multiple-value-bind (parsed-value success field-meta all-headers)
        (http-body:parse content-type nil stream)
      (declare (ignore field-meta))
      (let* ((value (if success
                        parsed-value
                        (slurp-stream stream content-length)))
             (value (if (starts-with-subseq "text/" content-type)
                        (babel:octets-to-string value
                                                :encoding (detect-charset content-type))
                        value)))
        (coerce-data
          (if (and (media-type-encoding media-type)
                   ;; The encoding object SHALL only apply to requestBody objects
                   ;; when the media type is multipart or application/x-www-form-urlencoded.
                   (or (starts-with-subseq "application/x-www-form-urlencoded" content-type)
                       (starts-with-subseq "multipart/" content-type)))
              (mapc (lambda (pair)
                      (let* ((encoding (aget (media-type-encoding media-type) (car pair)))
                             (property (and (media-type-schema media-type)
                                            (find (car pair) (object-properties (media-type-schema media-type))
                                                  :key #'property-name
                                                  :test #'equal)))
                             (schema (property-type property)))
                        (when (and encoding
                                   all-headers)
                          (let* ((headers (aget all-headers (car pair)))
                                 (content-type (gethash "content-type" headers
                                                        (default-content-type schema))))
                            (check-encoding encoding content-type headers)))
                        (when (and encoding
                                   (starts-with-subseq "application/x-www-form-urlencoded"
                                                       content-type)
                                   (encoding-style encoding))
                          (setf (cdr pair)
                                (parse-complex-string (cdr pair)
                                                      (encoding-style encoding)
                                                      (encoding-explode-p encoding)
                                                      schema)))))
                    value)
              value)
          (or (media-type-schema media-type) t))))))
