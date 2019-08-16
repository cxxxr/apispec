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
                #:*coerce-integer-string-to-boolean*)
  (:shadowing-import-from #:apispec/classes/schema
                          #:byte
                          #:number
                          #:string
                          #:boolean
                          #:array)
  (:import-from #:apispec/classes/header
                #:coerce-with-header)
  (:import-from #:apispec/complex
                #:parse-complex-parameters)
  (:import-from #:apispec/body
                #:parse-body)
  (:import-from #:apispec/body/multipart
                #:*multipart-force-stream*)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:parse-with-encoding
           #:encoding-content-type-mismatch))
(in-package #:apispec/classes/encoding/parse)

(define-condition encoding-content-type-mismatch (error)
  ((message :type string
            :initarg :message))
  (:report (lambda (condition stream)
             (princ (slot-value condition 'message) stream))))

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
      (handler-case (match-content-type (encoding-content-type encoding)
                                        content-type
                                        :comma-separated t)
        (error (e)
          (error 'encoding-content-type-mismatch
                 :message (princ-to-string e)))))
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
              do (coerce-with-header given-header-value header-object)))
      ;; TODO: Respect encoding-allow-reserved-p if it's urlencoded.
      (multiple-value-bind (parsed-values parsed-headers)
          (let ((*multipart-force-stream* nil))
            (parse-body value content-type))
        (declare (ignore parsed-headers))
        (when (and (starts-with-subseq "application/x-www-form-urlencoded" (string-downcase content-type))
                   (encoding-style encoding))
          (setf parsed-values
                (parse-complex-parameters parsed-values
                                          (encoding-style encoding)
                                          (encoding-explode-p encoding)
                                          schema)))
        (let ((*coerce-integer-string-to-boolean*
                (starts-with-subseq "application/x-www-form-urlencoded" (string-downcase content-type))))
          (coerce-data parsed-values schema))))))
