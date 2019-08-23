(defpackage #:apispec/classes/media-type/parse
  (:use #:cl)
  (:import-from #:apispec/classes/media-type/class
                #:media-type
                #:media-type-encoding
                #:media-type-schema)
  (:import-from #:apispec/classes/encoding
                #:encoding
                #:parse-with-encoding)
  (:import-from #:apispec/classes/schema
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
(in-package #:apispec/classes/media-type/parse)

(defun parse-with-media-type (stream media-type content-type content-length)
  (check-type stream stream)
  (check-type media-type media-type)
  (check-type content-type cl:string)
  (check-type content-length (or integer null))
  (multiple-value-bind (parsed-values parsed-headers)
      (parse-body stream content-type content-length)
    (coerce-data
      (if ;; The encoding object SHALL only apply to requestBody objects
        ;; when the media type is multipart or application/x-www-form-urlencoded.
        (or (starts-with-subseq "application/x-www-form-urlencoded" (string-downcase content-type))
            (starts-with-subseq "multipart/" (string-downcase content-type)))
        (progn
          (assert (association-list-p parsed-values 'string t))
          (mapc (lambda (pair)
                  (let ((encoding (aget (media-type-encoding media-type) (car pair)
                                        (make-instance 'encoding)))
                        (property (and (media-type-schema media-type)
                                       (find-object-property (media-type-schema media-type)
                                                             (car pair)))))
                    (setf (cdr pair)
                          (parse-with-encoding (cdr pair)
                                               encoding
                                               (if property
                                                   (property-type property)
                                                   t)
                                               (aget parsed-headers (car pair))))))
                parsed-values))
        parsed-values)
      (media-type-schema media-type))))
