(uiop:define-package #:apispec/utils
  (:use #:cl)
  (:use-reexport #:apispec/utils/media-type)
  (:import-from #:trivial-cltl2
                #:declaration-information)
  (:import-from #:flexi-streams)
  (:import-from #:babel)
  (:import-from #:cl-ppcre)
  (:export #:proper-list-p
           #:proper-list
           #:association-list-p
           #:association-list
           #:declaim-safety
           #:undeclaim-safety
           #:slurp-stream
           #:detect-charset))
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

(defun slurp-stream (stream)
  (if (typep stream 'flex:vector-stream)
      (coerce (flex::vector-stream-vector stream) '(simple-array (unsigned-byte 8) (*)))
      (apply #'concatenate
             '(simple-array (unsigned-byte 8) (*))
             (loop with buffer = (make-array 1024 :element-type '(unsigned-byte 8))
                   for read-bytes = (read-sequence buffer stream)
                   collect (subseq buffer 0 read-bytes)
                   while (= read-bytes 1024)))))

(defun detect-charset (content-type &optional (default babel:*default-character-encoding*))
  (multiple-value-bind (type subtype charset)
      (parse-content-type content-type)
    (declare (ignore type subtype))
    (cond
      ((null charset)
       default)
      ((string-equal charset "utf-8")
       :utf-8)
      ((string-equal charset "euc-jp")
       :eucjp)
      ((or (string-equal charset "shift_jis")
           (string-equal charset "shift-jis")
           (string-equal charset "windows-31j"))
       :cp932)
      (t (or (find charset (babel:list-character-encodings)
                   :test #'string-equal)
             babel:*default-character-encoding*)))))
