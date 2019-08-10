(defpackage #:apispec/utils
  (:use #:cl)
  (:import-from #:trivial-cltl2
                #:declaration-information)
  (:import-from #:cl-ppcre)
  (:export #:proper-list-p
           #:proper-list
           #:association-list-p
           #:association-list
           #:declaim-safety
           #:undeclaim-safety
           #:parse-media-type
           #:match-content-type))
(in-package #:apispec/utils)

(defpackage #:apispec/utils/lambda-predicate)

(defun proper-list-p (object &optional (element-type t))
  (and (listp object)
       (null (cdr (last object)))
       (or (eq element-type t)
           (every (lambda (x) (typep x element-type))
                  object))))

(defvar *proper-list-type-checker*
  (make-hash-table :test 'equal))

(deftype proper-list (&optional (element-type t))
  (let ((fn (if (eq element-type t)
                'proper-list-p
                (or (gethash element-type *proper-list-type-checker*)
                    (let ((fn (intern (format nil "~A-PROPER-LIST" element-type)
                                      '#:apispec/utils/lambda-predicate)))
                      (setf (fdefinition fn)
                            (lambda (object)
                              (proper-list-p object element-type)))
                      (setf (gethash element-type *proper-list-type-checker*)
                            fn))))))
    `(satisfies ,fn)))

(defun association-list-p (value key-type value-type)
  (and (listp value)
       (every (lambda (pair)
                (and (consp pair)
                     (typep (car pair) key-type)
                     (typep (cdr pair) value-type)))
              value)))

(defun simple-association-list-p (value)
  (association-list-p value '(or symbol string) t))

(defvar *association-list-type-checker*
  (make-hash-table :test 'equal))

(deftype association-list (&optional (key-type '(or symbol string)) (value-type t) )
  (let ((fn (if (and (equal key-type '(or symbol string))
                     (eq value-type t))
                'simple-association-list-p
                (or (gethash (cons key-type value-type) *association-list-type-checker*)
                    (let ((fn (intern (format nil "~S-ASSOCIATION-LIST" (cons key-type value-type))
                                      '#:apispec/utils/lambda-predicate)))
                      (setf (fdefinition fn)
                            (lambda (object)
                              (association-list-p object key-type value-type)))
                      (setf (gethash (cons key-type value-type) *association-list-type-checker*)
                            fn))))))
    `(satisfies ,fn)))

(defmacro declaim-safety ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,(intern (string :*previous-safety*) *package*)
       (or (assoc 'safety (declaration-information 'optimize))
           '(safety 1)))
     (proclaim '(optimize safety))))

(defmacro undeclaim-safety ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (proclaim `(optimize ,,(intern (string :*previous-safety*) *package*)))))

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
