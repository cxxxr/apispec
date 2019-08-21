(defpackage #:apispec/classes/schema/composition
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/schema/core
                #:object)
  (:import-from #:apispec/classes/schema/coerce
                #:*force-allow-additional-properties*
                #:coerce-data)
  (:import-from #:apispec/classes/schema/errors
                #:schema-error
                #:schema-coercion-failed)
  (:export #:composition-schema
           #:schema-one-of
           #:schema-any-of
           #:schema-all-of))
(in-package #:apispec/classes/schema/composition)

(defclass composition-schema (object)
  ((one-of :type (proper-list object)
           :initarg :one-of
           :initform nil
           :reader schema-one-of)
   (any-of :type (proper-list object)
           :initarg :any-of
           :initform nil
           :reader schema-any-of)
   (all-of :type (proper-list object)
           :initarg :all-of
           :initform nil
           :reader schema-all-of)))

(defmethod coerce-data (value (schema composition-schema))
  (flet ((map-schemas (schemas)
           (mapcar (lambda (subschema)
                     (handler-case
                         (cons
                           (coerce-data value subschema)
                           t)
                       (schema-error ()
                         (cons nil nil))))
                   schemas)))
    (cond
      ((schema-one-of schema)
       (let ((results (map-schemas (schema-one-of schema))))
         (unless (= 1 (count t results :key #'cdr))
           (error 'schema-coercion-failed
                  :value value
                  :schema schema
                  :message "Multiple schemas are possible for oneOf composition schema"))
         (car (find-if #'cdr results))))
      ((schema-any-of schema)
       (let ((results (let ((*force-allow-additional-properties* t))
                        (map-schemas (schema-any-of schema)))))
         (when (= 0 (count t results :key #'cdr))
           (error 'schema-coercion-failed
                  :value value
                  :schema schema
                  :message "Every schemas aren't possible for anyOf composition schema"))
         (apply #'append (mapcar #'car results))))
      ((schema-all-of schema)
       (mapcan (lambda (subschema)
                 (coerce-data value subschema))
               (schema-all-of schema))))))
