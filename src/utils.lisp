(defpackage #:apispec/utils
  (:use #:cl)
  (:export #:proper-list-p
           #:proper-list
           #:association-list-p
           #:association-list))
(in-package #:apispec/utils)

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
                    (let ((fn (gensym (format nil "~A-PROPER-LIST" element-type))))
                      (setf (fdefinition fn)
                            (lambda (object)
                              (proper-list-p object element-type)))
                      (setf (gethash element-type *proper-list-type-checker*)
                            fn))))))
    `(satisfies ,fn)))

(defun association-list-p (value)
  (and (listp value)
       (every (lambda (pair)
                (and (consp pair)
                     (typep (car pair) '(or symbol string))))
              value)))

(deftype association-list ()
  '(satisfies association-list-p))
