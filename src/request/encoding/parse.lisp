(defpackage #:apispec/request/encoding/parse
  (:use #:cl
        #:cl-utilities)
  (:import-from #:apispec/request/encoding/core
                #:media-type
                #:media-type-encoding
                #:media-type-schema
                #:encoding-style
                #:encoding-explode-p)
  (:import-from #:apispec/schema
                #:object
                #:coerce-data
                #:object-properties
                #:property-name)
  (:shadowing-import-from #:apispec/schema
                          #:array)
  (:import-from #:cl-ppcre)
  (:import-from #:babel
                #:string-to-octets)
  (:import-from #:flexi-streams
                #:make-in-memory-input-stream)
  (:import-from #:http-body)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-complex-string
           #:parse-with-media-type))
(in-package #:apispec/request/encoding/parse)

(define-condition parse-failed (error)
  ((message :initarg :message))
  (:report (lambda (condition stream)
             (princ (slot-value condition 'message) stream))))

(defun parse-matrix-value (value &key as explode)
  (typecase as
    (array
     (if explode
         (let ((results '()))
           (ppcre:do-register-groups (k v)
               (";([^=;]+)(?:=([^;]+))?" value)
             (push v (aget results k)))
           (loop for (k . v) in results
                 collect (cons k (coerce (nreverse v) 'vector))))
         (collecting
           (ppcre:do-register-groups (k v)
               (";([^=;]+)(?:=([^;]+))?" value)
             (collect (cons k
                            (coerce (ppcre:split "," v) 'vector)))))))
    (object
     (collecting
       (ppcre:do-register-groups (k v)
           (";([^=;]+)(?:=([^;]+))?" value)
         (collect
             (cons k
                   (if explode
                       v
                       (loop for (k v) on (ppcre:split "," v) by #'cddr
                             collect (cons k v))))))))
    (otherwise
     (collecting
       (ppcre:do-register-groups (k v)
           (";([^=;]+)(?:=([^;]+))?" value)
         (collect (cons k v)))))))

(defun parse-label-value (value &key as explode)
  (typecase as
    (array
     (coerce (collecting
               (ppcre:do-matches-as-strings (matches "(?<=\\.)([^\\.]+)" value)
                 (collect matches)))
             'vector))
    (object
     (if explode
         (collecting
           (ppcre:do-register-groups (kv) ("\\.([^\\.]+)" value)
             (let ((kv (ppcre:split "=" kv)))
               (collect (cons (first kv) (second kv))))))
         (loop for (k v) on (collecting
                              (ppcre:do-register-groups (kv) ("\\.([^\\.]+)" value)
                                (collect kv))) by #'cddr
               collect (cons k v))))
    (otherwise
     (values (ppcre:scan-to-strings "(?<=\\.)([^\\.]+)" value)))))

(defun parse-form-value (value &key as explode)
  (typecase as
    (array
     (let ((results '()))
       (dolist (kv (ppcre:split "&" value))
         (destructuring-bind (k v) (ppcre:split "=" kv :limit 2)
           (if explode
               (push v (aget results k))
               (setf results
                     (nconc results (list (cons k (ppcre:split "," v))))))))
       (if explode
           (loop for (k . v) in results
                 collect (cons k (coerce (nreverse v) 'vector)))
           (loop for (k . v) in results
                 collect (cons k (coerce v 'vector))))))
    (object
     (collecting
       (dolist (kv (ppcre:split "&" value))
         (destructuring-bind (k v) (ppcre:split "=" kv :limit 2)
           (collect
               (if explode
                   (cons k v)
                   (cons k
                         (loop for (k v) on (ppcre:split "," v) by #'cddr
                               collect (cons k v)))))))))
    (otherwise
     (collecting
       (ppcre:do-register-groups (k v)
           ("([^=]+)(?:=([^&]+))?" value)
         (collect (cons k v)))))))

(defun parse-simple-value (value &key as explode)
  (let ((key-values (ppcre:split "," value)))
    (typecase as
      (array
       (coerce (ppcre:split "," value) 'vector))
      (object
       (if explode
           (loop for kv in key-values
                 collect (apply #'cons (ppcre:split "=" kv :limit 2)))
           (loop for (k v) on key-values by #'cddr
                 collect (cons k v))))
      (otherwise
       value))))

(defun %parse-delimited-value (delimiter value &key as)
  (let ((values (ppcre:split (ppcre:quote-meta-chars delimiter) value)))
    (typecase as
      (array (coerce values 'vector))
      (object
       (loop for (k v) on values by #'cddr
             collect (cons k v)))
      (otherwise
       (error 'parse-failed
              :message (format nil "~S can't be type '~A'" value (type-of as)))))))

(defun parse-space-delimited-value (value &key as)
  (%parse-delimited-value "%20" value :as as))

(defun parse-pipe-delimited-value (value &key as)
  (%parse-delimited-value "|" value :as as))

(defun parse-deep-object-value (value)
  (let ((results '()))
    (dolist (key-values (ppcre:split "&" value) results)
      (destructuring-bind (k v)
          (ppcre:split "=" key-values :limit 2)
        (destructuring-bind (k prop)
            (coerce
             (nth-value 1 (ppcre:scan-to-strings "([^\\[]+)\\[([^\\]+])\\]" k))
             'list)
          (setf (aget results k)
                (append (aget results k) (list (cons prop v)))))))))

(defun parse-complex-string (value style explode schema)
  (check-type value string)
  (check-type style string)
  (cond
    ((equal style "matrix")
     (parse-matrix-value value
                         :as schema
                         :explode explode))
    ((equal style "label")
     (parse-label-value value
                        :as schema
                        :explode explode))
    ((equal style "form")
     (parse-form-value value
                       :as schema
                       :explode explode))
    ((equal style "simple")
     (parse-simple-value value
                         :as schema
                         :explode explode))
    ((equal style "spaceDelimited")
     (parse-space-delimited-value value :as schema))
    ((equal style "pipeDelimited")
     (parse-pipe-delimited-value value :as schema))
    ((equal style "deepObject")
     (parse-deep-object-value value))
    (t
     (error "Unexpected style: ~S" style))))

(defun parse-with-media-type (value media-type content-type)
  (check-type media-type media-type)
  (check-type content-type string)
  (let ((value
          (if (starts-with-subseq "text/" content-type)
              value
              (let* ((value (etypecase value
                              (string (babel:string-to-octets value))
                              ((vector (unsigned-byte 8)) value)))
                     (stream (flex:make-in-memory-input-stream value)))
                (multiple-value-bind (parsed-value success)
                    (http-body:parse content-type nil stream)
                  (if success
                      parsed-value
                      value))))))
    (coerce-data
     (if (and (media-type-encoding media-type)
              (or (starts-with-subseq "application/x-www-form-urlencoded" content-type)
                  (starts-with-subseq "multipart/form-data" content-type)))
         (mapc (lambda (pair)
                 (let ((encoding (aget (media-type-encoding media-type) (car pair)))
                       (schema (and (media-type-schema media-type)
                                    (find (car pair) (object-properties (media-type-schema media-type))
                                          :key #'property-name
                                          :test #'equal))))
                   (when encoding
                     (setf (cdr pair)
                           (parse-complex-string (cdr pair)
                                                 (encoding-style encoding)
                                                 (encoding-explode-p encoding)
                                                 schema)))))
               value)
         value)
     (or (media-type-schema media-type) t))))
