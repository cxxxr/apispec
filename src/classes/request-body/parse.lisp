(defpackage #:apispec/classes/request-body/parse
  (:use #:cl
        #:apispec/classes/request-body/class)
  (:import-from #:apispec/classes/media-type
                #:parse-with-media-type)
  (:export #:request-body-validation-failed
           #:parse-request-body))
(in-package #:apispec/classes/request-body/parse)

(define-condition request-body-validation-failed (error)
  ((message :type string
            :initarg :message))
  (:report (lambda (condition stream)
             (princ (slot-value condition 'message) stream))))

(defun parse-request-body (body-stream content-type request-body)
  (check-type body-stream (or stream null))
  (check-type content-type (or string null))
  (check-type request-body request-body)

  (when body-stream
    (unless content-type
      (return-from parse-request-body body-stream))

    (let ((media-type (find-request-body-media-type request-body content-type)))
      (unless media-type
        (error 'request-body-validation-failed
               :message (format nil "Request body Content-Type ~S is not allowed"
                                content-type)))
      (parse-with-media-type body-stream media-type content-type))))
