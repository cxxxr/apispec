(defpackage #:apispec/response/encode
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/response/class
                #:responses
                #:http-status-code
                #:response-content)
  (:import-from #:apispec/types/schema
                #:encode-data)
  (:import-from #:apispec/types/media-type
                #:media-type-schema)
  (:import-from #:cl-ppcre)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:encode-response))
(in-package #:apispec/response/encode)

(defun parse-media-type (value)
  (let ((matches
          (nth-value 1
                     (ppcre:scan-to-strings "^([0-9a-zA-Z!#$%&'+-.^_`|~]+|\\*)/([0-9a-zA-Z!#$%&'+-.^_`|~]+|\\*)$" value))))
    (when matches
      (values (aref matches 0) (aref matches 1)))))

(defun match-content-type (pattern content-type)
  (multiple-value-bind (type subtype)
      (parse-media-type pattern)
    (unless type
      (error "Invalid media type: ~S" pattern))
    (multiple-value-bind (type2 subtype2)
        (parse-media-type content-type)
      (unless type2
        (error "Invalid content type: ~S" content-type))
      (and (or (equal type "*")
               (equal type type2))
           (or (equal subtype "*")
               (equal subtype subtype2))))))

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
