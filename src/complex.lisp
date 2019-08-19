(defpackage #:apispec/complex
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/schema
                #:object
                #:find-object-property
                #:property-type
                #:schema
                #:coerce-data
                #:*coerce-integer-string-to-boolean*)
  (:shadowing-import-from #:apispec/classes/schema
                          #:array)
  (:import-from #:cl-ppcre)
  (:import-from #:cl-utilities
                #:collecting
                #:collect)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:assoc-utils
                #:aget
                #:alist-keys)
  (:export #:complex-style-string-p
           #:complex-style
           #:parse-matrix-value
           #:parse-label-value
           #:parse-form-value
           #:parse-simple-value
           #:parse-space-delimited-value
           #:parse-pipe-delimited-value
           #:parse-deep-object-value
           #:parse-complex-string
           #:parse-complex-parameter
           #:parse-complex-parameters))
(in-package #:apispec/complex)

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
  (let ((*coerce-integer-string-to-boolean* t))
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
      (or as t))))

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
          (if (rest values)
              values
              (first values))))
      (or as t))))

(defun parse-comma-separated-value (value &key as)
  (%parse-delimited-value "," value :as as))

(defun parse-space-delimited-value (value &key as)
  (%parse-delimited-value " " value :as as))

(defun parse-pipe-delimited-value (value &key as)
  (%parse-delimited-value "|" value :as as))

(defun parse-deep-object-value (parameters &optional name)
  (let ((results '()))
    (loop for (key . val) in parameters
          do (destructuring-bind (key prop)
                 (coerce
                  (nth-value 1 (ppcre:scan-to-strings "([^\\[]*)(?:\\[([^\\]+])\\])?" key))
                  'list)
               (when (or (null name) (string= name key))
                 (if prop
                     (setf (aget results key)
                           (append (aget results key)
                                   (list (cons prop val))))
                     (setf (aget results key) val)))))
    (if name
        (aget results name)
        results)))

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

(defun parse-complex-parameter (alist name style explode schema)
  (assert (association-list-p alist 'string 'string))
  (check-type schema schema)
  (let ((*coerce-integer-string-to-boolean* (string= style "form")))
    (coerce-data
      (cond
        ((equal style "form")
         (if explode
             (loop for (key . value) in alist
                   if (string= key name)
                   collect value into values
                   finally
                   (return
                     (if (or (typep values '(or array object))
                             (rest values))
                         values
                         (first values))))
             (parse-comma-separated-value (aget alist name)
                                          :as schema)))
        ((equal style "spaceDelimited")
         (parse-space-delimited-value (aget alist name)
                                      :as schema))
        ((equal style "pipeDelimited")
         (parse-pipe-delimited-value (aget alist name)
                                     :as schema))
        ((equal style "deepObject")
         (parse-deep-object-value alist name))
        (t (error "Unexpected style: ~S" style)))
      schema)))

(defun parse-complex-parameters (alist style explode schema)
  (check-type schema object)
  (if explode
      (let ((keys (remove-duplicates (alist-keys alist) :test 'equal :from-end t)))
        (loop for key in keys
              for values = (mapcar #'cdr
                                   (remove-if-not
                                     (lambda (pair)
                                       (string= (car pair) key))
                                     alist))
              for key-property = (find-object-property schema key)
              for key-schema = (or (and key-property
                                        (property-type key-property))
                                   t)
              collect (cons key
                            (typecase key-schema
                              ((or array object) (coerce-data values key-schema))
                              (otherwise
                                (coerce-data (if (rest values)
                                                 values
                                                 (first values))
                                             key-schema))))))
      (if (equal style "deepObject")
          (coerce-data (parse-deep-object-value alist) schema)
          (loop for (key . val) in alist
                for key-property = (find-object-property schema key)
                for key-schema = (or (and key-property
                                          (property-type key-property))
                                     t)
                collect (cons key
                              (coerce-data
                                (let ((values (cond
                                                ((equal style "form")
                                                 (parse-comma-separated-value val
                                                                              :as key-schema))
                                                ((equal style "spaceDelimited")
                                                 (parse-space-delimited-value val
                                                                              :as key-schema))
                                                ((equal style "pipeDelimited")
                                                 (parse-pipe-delimited-value val
                                                                             :as key-schema))
                                                (t (error "Unexpected style: ~S" style)))))
                                  values)
                                key-schema))))))
