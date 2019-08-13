(defpackage #:apispec/types/media-type/parse
  (:use #:cl)
  (:import-from #:apispec/types/media-type/class
                #:media-type
                #:media-type-encoding
                #:media-type-schema)
  (:import-from #:apispec/types/encoding
                #:encoding
                #:parse-with-encoding)
  (:import-from #:apispec/types/schema
                #:coerce-data
                #:find-object-property
                #:property-type)
  (:import-from #:apispec/body
                #:parse-body)
  (:import-from #:apispec/utils
                #:association-list-p)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-with-media-type))
(in-package #:apispec/types/media-type/parse)

(defun parse-with-media-type (stream media-type content-type)
  (check-type stream stream)
  (check-type media-type media-type)
  (check-type content-type cl:string)
  (multiple-value-bind (parsed-values parsed-headers)
      (parse-body stream content-type)
    (coerce-data
      (if (and (media-type-encoding media-type)
               ;; The encoding object SHALL only apply to requestBody objects
               ;; when the media type is multipart or application/x-www-form-urlencoded.
               (or (starts-with-subseq "application/x-www-form-urlencoded" (string-downcase content-type))
                   (starts-with-subseq "multipart/" (string-downcase content-type))))
          (progn
            (assert (association-list-p parsed-values 'string t))
            (mapc (lambda (pair)
                    (let ((encoding (aget (media-type-encoding media-type) (car pair)
                                          (make-instance 'encoding)))
                          (property (or (and (media-type-schema media-type)
                                             (find-object-property (media-type-schema media-type)
                                                                   (car pair)))
                                        t)))
                      (setf (cdr pair)
                            (parse-with-encoding (cdr pair)
                                                 encoding
                                                 (property-type property)
                                                 (aget parsed-headers (car pair))))))
                  parsed-values))
          parsed-values)
      (or (media-type-schema media-type) t))))
