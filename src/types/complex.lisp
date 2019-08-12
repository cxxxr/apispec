(defpackage #:apispec/types/complex
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/types/schema
                #:object
                #:coerce-data
                #:find-object-property
                #:property-type)
  (:shadowing-import-from #:apispec/types/schema
                          #:array)
  (:import-from #:cl-ppcre)
  (:import-from #:cl-utilities
                #:collecting
                #:collect)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:complex-parser-failed
           #:complex-style-string-p
           #:complex-style
           #:parse-matrix-value
           #:parse-label-value
           #:parse-form-value
           #:parse-simple-value
           #:parse-space-delimited-value
           #:parse-pipe-delimited-value
           #:parse-deep-object-value
           #:parse-complex-string
           #:parse-complex-parameters))
(in-package #:apispec/types/complex)

(define-condition complex-parser-failed (error)
  ((message :initarg :message))
  (:report (lambda (condition stream)
             (princ (slot-value condition 'message) stream))))

(defun complex-style-string-p (style)
  (and (member style '("matrix" "label" "form" "simple" "spaceDelimited" "pipeDelimited" "deepObject")
               :test #'equal)
       t))

(deftype complex-style ()
  '(satisfies complex-style-string-p))

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

(defun parse-form-value (parameters name &key as explode)
  (coerce-data
    (typecase as
      (array
        (if explode
            (map 'vector #'cdr
                 (remove-if-not (lambda (param-name)
                                  (equal param-name name))
                                parameters
                                :key #'car))
            (when-let (val (aget parameters name))
              (coerce (ppcre:split "," val) 'vector))))
      (object
        (if explode
            parameters
            (when-let (val (aget parameters name))
              (loop for (k v) on (ppcre:split "," val) by #'cddr
                    collect (cons k v)))))
      (otherwise
        (aget parameters name)))
    (or as t)))

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
    (coerce-data
      (typecase as
        (array (coerce values 'vector))
        (object
          (loop for (k v) on values by #'cddr
                collect (cons k v)))
        (otherwise
          (error 'complex-parser-failed
                 :message (format nil "~S can't be type '~A'" value (type-of as)))))
      (or as t))))

(defun parse-space-delimited-value (value &key as)
  (%parse-delimited-value " " value :as as))

(defun parse-pipe-delimited-value (value &key as)
  (%parse-delimited-value "|" value :as as))

(defun parse-deep-object-value (parameters)
  (let ((results '()))
    (loop for (key . val) in parameters
          do (destructuring-bind (key prop)
                 (coerce
                  (nth-value 1 (ppcre:scan-to-strings "([^\\[]+)\\[([^\\]+])\\]" key))
                  'list)
               (setf (aget results key)
                     (append (aget results key) (list (cons prop val))))))
    results))

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
    ((equal style "simple")
     (parse-simple-value value
                         :as schema
                         :explode explode))
    (t
     (error "Unexpected style: ~S" style))))

(defun parse-complex-parameters (alist style explode schema)
  (check-type alist (association-list string string))
  (check-type schema object)
  (cond
    ((equal style "form")
     (loop for (k . v) in alist
           for property = (find-object-property schema k)
           collect (cons k
                         (if property
                             (parse-form-value alist k
                                               :as (property-type property)
                                               :explode explode)
                             v))))
    ((equal style "spaceDelimited")
     (loop for (k . v) in alist
           for property = (find-object-property schema k)
           collect (cons k
                         (if property
                             (parse-space-delimited-value v
                                                          :as (property-type property))
                             v))))
    ((equal style "pipeDelimited")
     (loop for (k . v) in alist
           for property = (find-object-property schema k)
           collect (cons k
                         (if property
                             (parse-pipe-delimited-value v
                                                         :as (property-type property))
                             v))))
    ((equal style "deepObject")
     (coerce-data (parse-deep-object-value alist) schema))
    (t (error "Unexpected style: ~S" style))))
