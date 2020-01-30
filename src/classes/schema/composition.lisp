(defpackage #:apispec/classes/schema/composition
  (:use #:cl
        #:apispec/utils)
  (:import-from #:apispec/classes/schema/core
                #:type
                #:schema
                #:object)
  (:import-from #:apispec/classes/schema/coerce
                #:*ignore-additional-properties*
                #:coerce-data)
  (:import-from #:apispec/classes/schema/validate
                #:validate-data)
  (:import-from #:apispec/classes/schema/errors
                #:schema-error
                #:schema-coercion-failed
                #:schema-validation-failed
                #:schema-oneof-error
                #:schema-anyof-error
                #:schema-allof-error)
  (:export #:composition-schema
           #:schema-one-of
           #:schema-any-of
           #:schema-all-of

           #:negative-schema
           #:schema-not))
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

(defclass negative-schema (schema)
  ((not :type schema
        :initarg :not
        :reader schema-not)))

(defgeneric process-one-of (process-type value schema))
(defgeneric process-any-of (process-type value schema))
(defgeneric process-all-of (process-type value schema))

(defun process-composition-schema (process-type value schema)
  (cond
    ((schema-one-of schema)
     (process-one-of process-type value schema))
    ((schema-any-of schema)
     (process-any-of process-type value schema))
    ((schema-all-of schema)
     (process-all-of process-type value schema))))

(defun map-schemas (fn value schemas)
  (let ((*ignore-additional-properties* t))
    (mapcar (lambda (subschema)
              (handler-case
                  (cons (funcall fn value subschema)
                        t)
                (schema-error ()
                  (cons nil nil))))
            schemas)))

(defmethod process-one-of ((process-type (eql 'coerce-data)) value schema)
  (let ((results (map-schemas #'coerce-data value (schema-one-of schema))))
    (unless (= 1 (count t results :key #'cdr))
      (error 'schema-oneof-error
             :value value
             :schema schema
             :subschemas (schema-one-of schema)))
    (car (find-if #'cdr results))))

(defmethod process-any-of ((process-type (eql 'coerce-data)) value schema)
  (let ((results (map-schemas #'coerce-data value (schema-any-of schema))))
    (when (= 0 (count t results :key #'cdr))
      (error 'schema-anyof-error
             :value value
             :schema schema
             :subschemas (schema-any-of schema)))
    (apply #'append (mapcar #'car results))))

(defmethod process-all-of ((process-type (eql 'coerce-data)) value schema)
  (mapcan (lambda (subschema)
            (coerce-data value subschema))
          (schema-all-of schema)))

(defmethod coerce-data (value (schema composition-schema))
  (process-composition-schema 'coerce-data value schema))

(defmethod coerce-data (value (schema negative-schema))
  (handler-case
      (coerce-data value (schema-not schema))
    (schema-error ()
      (return-from coerce-data value)))
  (error 'schema-coercion-failed
         :value value
         :schema schema
         :message "Possible for negative schema"))

(defmethod process-one-of ((process-type (eql 'validate-data)) value schema)
  (let ((results (map-schemas #'validate-data value (schema-one-of schema))))
    (unless (= 1 (count t results :key #'cdr))
      (error 'schema-oneof-error
             :value value
             :schema schema
             :subschemas (schema-one-of schema)))
    t))

(defmethod process-any-of ((process-type (eql 'validate-data)) value schema)
  (let ((results (map-schemas #'validate-data value (schema-any-of schema))))
    (when (= 0 (count t results :key #'cdr))
      (error 'schema-anyof-error
             :value value
             :schema schema
             :subschemas (schema-any-of schema)))
    t))

(defmethod process-all-of ((process-type (eql 'validate-data)) value schema)
  (every (lambda (subschema)
           (handler-case (validate-data value subschema)
             (schema-error ()
               nil)))
         (schema-all-of schema)))

(defmethod validate-data (value (schema composition-schema))
  (process-composition-schema 'validate-data value schema))
