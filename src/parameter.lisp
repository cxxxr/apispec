(defpackage #:apispec/parameter
  (:use #:cl
        #:trivial-cltl2
        #:cl-utilities)
  (:shadowing-import-from #:apispec/schema/core
                          #:schema
                          #:array
                          #:object)
  (:import-from #:apispec/schema/coerce
                #:coerce-data)
  (:import-from #:cl-ppcre)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parameter
           #:parse-value))
(in-package #:apispec/parameter)

;; Set safety level 3 for CLOS slot type checking.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *previous-safety*
    (or (assoc 'safety (declaration-information 'optimize))
        '(safety 1)))
  (proclaim '(optimize safety)))

(defun parameter-in-string-p (in)
  (and (member in '("path" "query" "header" "cookie")
               :test #'equal)
       t))

(deftype parameter-in ()
  '(satisfies parameter-in-string-p))

(defun parameter-style-string-p (style)
  (and (member style '("matrix" "label" "form" "simple" "spaceDelimited" "pipeDelimited" "deepObject")
               :test #'equal)
       t))

(deftype parameter-style ()
  '(satisfies parameter-style-string-p))

(defclass parameter ()
  ((name :type string
         :initarg :name)
   (in :type parameter-in
       :initarg :in)
   (required :type boolean
             :initarg :required)
   (schema :type schema
           :initarg :schema)
   (style :type parameter-style
          :initarg :style)
   (explode :type boolean
            :initarg :explode)
   (allow-reserved :type boolean
                   :initarg :allow-reserved)))

(defun parameter-style (parameter)
  (check-type parameter parameter)
  (if (slot-boundp parameter 'style)
      (slot-value parameter 'style)
      (with-slots (in) parameter
        (cond
          ((or (equal in "query")
               (equal in "cookie")) "form")
          ((or (equal in "path")
               (equal in "header")) "simple")))))

(defun parameter-explode (parameter)
  (check-type parameter parameter)
  (if (slot-boundp parameter 'explode)
      (slot-value parameter 'explode)
      (let ((style (parameter-style parameter)))
        (if (equal style "form")
            t
            nil))))

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
       (error "~S can't be type '~A'" value (type-of as))))))

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
        (name (slot-value parameter 'name))
        (empty '#:empty))
    (let ((result
            (cond
              ((equal style "matrix")
               (aget (parse-matrix-value value
                                         :as (slot-value parameter 'schema)
                                         :explode (parameter-explode parameter))
                     name
                     empty))
              ((equal style "label")
               (or (parse-label-value value
                                      :as (slot-value parameter 'schema)
                                      :explode (parameter-explode parameter))
                   empty))
              ((equal style "form")
               (aget (parse-form-value value
                                       :as (slot-value parameter 'schema)
                                       :explode (parameter-explode parameter))
                     name
                     empty))
              ((equal style "simple")
               (or (parse-simple-value value
                                       :as (slot-value parameter 'schema)
                                       :explode (parameter-explode parameter))
                   empty))
              ((equal style "spaceDelimited")
               (or (parse-space-delimited-value value
                                                :as (slot-value parameter 'schema))
                   empty))
              ((equal style "pipeDelimited")
               (or (parse-pipe-delimited-value value
                                               :as (slot-value parameter 'schema))
                   empty))
              ((equal style "deepObject")
               (aget (parse-deep-object-value value)
                     name
                     empty))
              (t
               (error "Unexpected style: ~S" style)))))
      (if (eq result empty)
          (error "Missing parameter: ~S" name)
          (coerce-data result (slot-value parameter 'schema))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim `(optimize ,*previous-safety*)))
