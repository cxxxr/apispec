(defpackage #:apispec/classes/request-body/parse
  (:use #:cl
        #:apispec/classes/request-body/class
        #:apispec/classes/request-body/errors)
  (:import-from #:apispec/classes/media-type
                #:parse-with-media-type)
  (:import-from #:apispec/classes/schema
                #:schema-error)
  (:import-from #:apispec/errors
                #:apispec-error)
  (:export #:parse-request-body))
(in-package #:apispec/classes/request-body/parse)

(defun parse-request-body (body-stream content-type content-length request-body)
  (check-type body-stream (or stream null))
  (check-type content-type (or string null))
  (check-type content-length (or integer null))
  (check-type request-body request-body)

  (when body-stream
    (unless content-type
      (return-from parse-request-body body-stream))

    (let ((media-type (find-request-body-media-type request-body content-type)))
      (unless media-type
        (error 'request-body-content-type-mismatch
               :given content-type
               :expected (mapcar #'car (request-body-content request-body))))
      (handler-case
          (parse-with-media-type body-stream media-type content-type content-length)
        (schema-error (e)
          (error e))
        (apispec-error ()
          (error 'request-body-parse-error
                 :content-type content-type))))))
