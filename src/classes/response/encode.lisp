(defpackage #:apispec/classes/response/encode
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/response/class
                #:response
                #:responses
                #:response-content
                #:response-headers)
  (:import-from #:apispec/classes/media-type
                #:media-type-schema)
  (:import-from #:apispec/body
                #:encode-data)
  (:import-from #:cl-ppcre)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:response-not-defined
           #:find-response
           #:find-media-type
           #:encode-response))
(in-package #:apispec/classes/response/encode)

(define-condition response-error (error) ())

(define-condition response-not-defined (response-error)
  ((code :type (or string integer null)
         :initarg :code
         :initform nil)
   (content-type :type (or string null)
                 :initarg :content-type
                 :initform nil))
  (:report (lambda (condition stream)
             (with-slots (code content-type) condition
               (format stream "Response is not defined for~@[ code=~S~]~@[ content-type=~S~]"
                       code content-type)))))

(defun find-response (responses status)
  (check-type responses responses)
  (or (aget responses (write-to-string status))
      (aget responses (format nil "~DXX" (floor (/ status 100))))
      (aget responses "default")
      (error 'response-not-defined
             :code status)))

(defun find-media-type (response content-type)
  (check-type response response)
  (check-type content-type string)
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
                 :test #'equal)
           (error 'response-not-defined
                  :content-type content-type))))

(defun encode-response (status headers data responses)
  (check-type status (integer 100 599))
  (assert (association-list-p headers 'string t))
  (check-type responses responses)
  ;; TODO: Think of the case when the Content-Type is not specified
  (let* ((content-type (aget headers "content-type"))
         (content-type (and (stringp content-type)
                            (ppcre:scan-to-strings "[^;\\s]+" content-type)))
         (response (find-response responses status))
         (media-type (find-media-type response content-type)))
    (list status
          (loop for (header-name . header-value) in headers
                ;; TODO: Header validation
                for response-header = (aget (response-headers response)
                                            (string-downcase header-name))
                append (list (intern (string-upcase header-name) :keyword)
                             header-value))
          (list (encode-data data
                             (or (and media-type
                                      (media-type-schema media-type))
                                 t)
                             content-type)))))
