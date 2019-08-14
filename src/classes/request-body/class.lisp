(defpackage #:apispec/classes/request-body/class
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/media-type
                #:media-type)
  (:import-from #:cl-ppcre
                #:scan-to-strings)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:request-body
           #:request-body-description
           #:request-body-content
           #:request-body-required-p
           #:request-body-media-type
           #:find-request-body-media-type))
(in-package #:apispec/classes/request-body/class)

(declaim-safety)

(defclass request-body ()
  ((description :type (or string null)
                :initarg :description
                :initform nil
                :reader request-body-description)
   (content :type (and (association-list string media-type)
                       (not null))
            :initarg :content
            :initform (error ":content is required for REQUEST-BODY")
            :reader request-body-content)
   (required :type boolean
             :initarg :required
             :initform nil
             :reader request-body-required-p)))

(defun find-request-body-media-type (request-body content-type)
  (let ((content (request-body-content request-body)))
    (cdr (or (find-if (lambda (type)
                        (starts-with-subseq type content-type))
                      content
                      :key #'car)
             (find-if (lambda (type)
                        (let ((matched (ppcre:scan-to-strings "[^/]+/(?=\\*)" type)))
                          (and matched
                               (starts-with-subseq matched content-type))))
                      content
                      :key #'car)))))

(undeclaim-safety)
