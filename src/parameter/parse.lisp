(defpackage #:apispec/parameter/parse
  (:use #:cl
        #:cl-utilities)
  (:import-from #:apispec/parameter/core
                #:parameter-name
                #:parameter-style
                #:parameter-schema
                #:parameter-explode-p)
  (:shadowing-import-from #:apispec/schema
                          #:array
                          #:object
                          #:coerce-data)
  (:import-from #:cl-ppcre)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-value
           #:parse-failed))
(in-package #:apispec/parameter/parse)

(define-condition parse-failed (error)
  ((message :initarg :message
            :initform nil))
  (:report (lambda (condition stream)
             (with-slots (message) condition
               (princ message stream)))))

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

(defun parse-value (value parameter)
  (let ((style (parameter-style parameter))
        (name (parameter-name parameter))
        (empty '#:empty))
    (let ((result
            (cond
              ((equal style "matrix")
               (aget (parse-matrix-value value
                                         :as (parameter-schema parameter)
                                         :explode (parameter-explode-p parameter))
                     name
                     empty))
              ((equal style "label")
               (or (parse-label-value value
                                      :as (parameter-schema parameter)
                                      :explode (parameter-explode-p parameter))
                   empty))
              ((equal style "form")
               (aget (parse-form-value value
                                       :as (parameter-schema parameter)
                                       :explode (parameter-explode-p parameter))
                     name
                     empty))
              ((equal style "simple")
               (or (parse-simple-value value
                                       :as (parameter-schema parameter)
                                       :explode (parameter-explode-p parameter))
                   empty))
              ((equal style "spaceDelimited")
               (or (parse-space-delimited-value value
                                                :as (parameter-schema parameter))
                   empty))
              ((equal style "pipeDelimited")
               (or (parse-pipe-delimited-value value
                                               :as (parameter-schema parameter))
                   empty))
              ((equal style "deepObject")
               (aget (parse-deep-object-value value)
                     name
                     empty))
              (t
               (error "Unexpected style: ~S" style)))))
      (if (eq result empty)
          (error 'parse-failed
                 :message (format nil "Missing parameter: ~S" name))
          (coerce-data result (parameter-schema parameter))))))
