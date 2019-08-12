(defpackage #:apispec/types/encoding/parse
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/types/encoding/class
                #:encoding
                #:encoding-content-type
                #:encoding-headers
                #:encoding-style
                #:encoding-explode-p)
  (:import-from #:apispec/types/schema
                #:schema
                #:binary
                #:object
                #:array-items
                #:coerce-data)
  (:shadowing-import-from #:apispec/types/schema
                          #:byte
                          #:number
                          #:string
                          #:boolean
                          #:array)
  (:import-from #:apispec/types/header
                #:coerce-with-header)
  (:import-from #:apispec/types/complex
                #:parse-complex-string)
  (:import-from #:apispec/body
                #:parse-body)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:parse-with-encoding))
(in-package #:apispec/types/encoding/parse)

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
    (null nil)))

(defun parse-with-encoding (value encoding schema headers)
  (check-type encoding encoding)
  (check-type schema schema)
  (check-type headers hash-table)
  (let ((content-type (gethash "content-type" headers
                               (default-content-type schema))))
    (when (and content-type
               (encoding-content-type encoding))
      (handler-case (match-content-type (encoding-content-type encoding)
                                        content-type
                                        :comma-separated t)
        (error (e)
          (error 'encoding-content-type-mismatch
                 :message (princ-to-string e)))))
    (let ((content-type (string-downcase
                          (or content-type
                              (encoding-content-type encoding)
                              "text/plain"))))
      (when (and (encoding-headers encoding)
                 headers
                 content-type
                 (starts-with-subseq (string-downcase content-type) "multipart/"))
        (loop for (header-name . header-object) in (encoding-headers encoding)
              for header-name-downcased = (string-downcase header-name)
              for given-header-value = (gethash header-name-downcased headers)
              ;; Content-Type is ignored
              if (not (string= header-name-downcased "content-type"))
              do (coerce-with-header given-header-value header-object)))
      ;; TODO: Respect encoding-allow-reserved-p if it's urlencoded.
      (multiple-value-bind (parsed-values parsed-headers)
          (parse-body value content-type)
        (declare (ignore parsed-headers))
        (when (and (starts-with-subseq "application/x-www-form-urlencoded" content-type)
                   (encoding-style encoding))
          (mapc (lambda (pair)
                  (setf (cdr pair)
                        (parse-complex-string (cdr pair)
                                              (encoding-style encoding)
                                              (encoding-explode-p encoding)
                                              schema)))
                parsed-values))
        ;; TODO: multipartのvalueは全部streamなので、coerceの時点で変換が必要
        (coerce-data parsed-values schema)))))
