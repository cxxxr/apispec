(defpackage #:apispec/utils/media-type
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:export #:parse-media-type
           #:match-content-type))
(in-package #:apispec/utils/media-type)

(defun parse-media-type (value)
  (let ((matches
          (nth-value 1
                     (ppcre:scan-to-strings "^([0-9a-zA-Z!#$%&'+-.^_`|~]+|\\*)/([0-9a-zA-Z!#$%&'+-.^_`|~]+|\\*)" value))))
    (when matches
      (values (aref matches 0) (aref matches 1)))))

(defun match-content-type (pattern content-type &key comma-separated)
  (every (lambda (pattern)
           (multiple-value-bind (type subtype)
               (parse-media-type pattern)
             (unless type
               (error "Invalid media type: ~S" pattern))
             (multiple-value-bind (type2 subtype2)
                 (parse-media-type content-type)
               (unless type2
                 (error "Invalid content type: ~S" content-type))
               (and (or (string= type "*")
                        (string-equal type type2))
                    (or (string= subtype "*")
                        (string-equal subtype subtype2))))))
         (if comma-separated
             (ppcre:split "\\s*,\\s*" pattern)
             (list pattern))))
