(defpackage #:apispec/types/encoding/parse
  (:use #:cl
        #:apispec/utils
        #:cl-utilities)
  (:import-from #:apispec/types/schema
                #:object)
  (:shadowing-import-from #:apispec/types/schema
                          #:array)
  (:import-from #:cl-ppcre)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-complex-string
           #:parse-complex-parameters))
(in-package #:apispec/types/encoding/parse)

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

(defun parse-form-value (parameters name &key as explode)
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
     (aget parameters name))))

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

(defun parse-deep-object-value (parameters name)
  (let ((results '()))
    (loop for (key . val) in parameters
          do (destructuring-bind (key prop)
                 (coerce
                  (nth-value 1 (ppcre:scan-to-strings "([^\\[]+)\\[([^\\]+])\\]" key))
                  'list)
               (when (equal name key)
                 (setf results
                       (append results (list (cons prop val)))))))
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

(defun parse-complex-parameters (parameters name style explode schema)
  (check-type parameters (association-list string string))
  (check-type name string)
  (cond
    ((equal style "form")
     (parse-form-value parameters name
                       :as schema
                       :explode explode))
    ((equal style "spaceDelimited")
     (when-let (pair (assoc name parameters :test #'equal))
       (parse-space-delimited-value (cdr pair) :as schema)))
    ((equal style "pipeDelimited")
     (when-let (pair (assoc name parameters :test #'equal))
       (parse-pipe-delimited-value (cdr pair) :as schema)))
    ((equal style "deepObject")
     (parse-deep-object-value parameters name))
    (t (error "Unexpected style: ~S" style))))
