(defpackage #:apispec/classes/response/encode
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/response/class
                #:responses
                #:http-status-code
                #:response-content)
  (:import-from #:apispec/classes/schema
                #:encode-data)
  (:import-from #:apispec/classes/media-type
                #:media-type-schema)
  (:import-from #:cl-ppcre)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:encode-response))
(in-package #:apispec/classes/response/encode)

(defun find-response (status content-type responses)
  (check-type content-type string)
  (or (aget responses status)
      (aget responses "default")
      (error "Response is not defined for code=~S, content-type=~S"
             status content-type)))

(defun find-media-type (content-type response)
  (cdr (or (find-if (lambda (media-type-string)
                      (string-equal media-type-string content-type))
                    (response-content response)
                    :key #'car)
           (find-if (lambda (media-type-string)
                      (and (not (string= media-type-string "*/*"))
                           (match-content-type media-type-string content-type)))
                    (response-content response)
                    :key #'car)
           (find "*/*" (response-content response)
                 :key #'car
                 :test #'equal))))

(defun encode-response (status headers data responses)
  (check-type status http-status-code)
  (check-type headers (association-list string))
  (check-type responses responses)
  ;; TODO: Think of the case when the Content-Type is not specified
  (let* ((content-type (aget headers "content-type"))
         (content-type (and (stringp content-type)
                            (ppcre:scan-to-strings "[^;\\s]+" content-type)))
         (response (find-response status content-type responses))
         (media-type (find-media-type content-type response)))
    (cond
      ((equal content-type "application/json")
       (if (and media-type
                (media-type-schema media-type))
           (encode-data data (media-type-schema media-type))
           data))
      (t data))))
