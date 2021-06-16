(defpackage #:apispec/classes/encoding/parse
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/encoding/class
                #:encoding
                #:encoding-content-type
                #:encoding-headers
                #:encoding-style
                #:encoding-explode-p)
  (:import-from #:apispec/classes/schema
                #:schema
                #:binary
                #:object
                #:array-items
                #:coerce-data
                #:*coerce-string-to-boolean*)
  (:shadowing-import-from #:apispec/classes/schema
                          #:byte
                          #:number
                          #:string
                          #:boolean
                          #:array)
  (:import-from #:apispec/classes/header
                #:header-missing
                #:coerce-with-header)
  (:import-from #:apispec/complex
                #:parse-complex-parameters)
  (:import-from #:apispec/body
                #:parse-body)
  (:import-from #:apispec/body/parser/multipart
                #:*multipart-force-stream*)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:parse-with-encoding
           #:encoding-content-type-mismatch))
(in-package #:apispec/classes/encoding/parse)

(defun default-content-type (schema)
  (etypecase schema
    ((or byte binary) "application/octet-stream")
    ((or number string boolean) "text/plain")
    (object "application/json")
    (array
      (default-content-type (array-items schema)))
    ((eql t) "text/plain")))

(defun parse-with-encoding (value encoding schema headers)
  (check-type encoding encoding)
  (check-type schema (or schema (eql t)))
  (check-type headers (or hash-table null))
  (let ((content-type (and headers
                           (gethash "content-type" headers))))
    (when (and content-type
               (encoding-content-type encoding))
      (or (handler-case
              (match-content-type (encoding-content-type encoding)
                                  content-type
                                  :comma-separated t)
            (error ()
              nil))
          (error 'encoding-content-type-mismatch
                 :given content-type
                 :expected (encoding-content-type encoding))))
    (let ((content-type (or content-type
                            (encoding-content-type encoding)
                            (default-content-type schema))))
      (when (and (encoding-headers encoding)
                 headers
                 content-type
                 (starts-with-subseq "multipart/" (string-downcase content-type)))
        (loop for (header-name . header-object) in (encoding-headers encoding)
              for header-name-downcased = (string-downcase header-name)
              for given-header-value = (gethash header-name-downcased headers)
              ;; Content-Type is ignored
              if (not (string= header-name-downcased "content-type"))
              do (handler-case
                     (coerce-with-header given-header-value header-object)
                   (header-missing ()
                     (error 'encoding-header-missing
                            :header header-name-downcased)))))
      ;; TODO: Respect encoding-allow-reserved-p if it's urlencoded.
      (multiple-value-bind (parsed-values parsed-headers)
          (let ((*multipart-force-stream* nil))
            (parse-body value content-type))
        (declare (ignore parsed-headers))
        (let ((*coerce-string-to-boolean*
                (starts-with-subseq "application/x-www-form-urlencoded" (string-downcase content-type))))
          (if (and (starts-with-subseq "application/x-www-form-urlencoded" (string-downcase content-type))
                   (encoding-style encoding))
              (parse-complex-parameters parsed-values
                                        (encoding-style encoding)
                                        (encoding-explode-p encoding)
                                        schema)
              (coerce-data parsed-values schema)))))))
