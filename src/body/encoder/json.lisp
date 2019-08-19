(defpackage #:apispec/body/encoder/json
  (:use #:cl
        #:apispec/body/errors)
  (:import-from #:apispec/classes/schema
                #:object
                #:binary
                #:object-properties
                #:schema-nullable-p
                #:property-name
                #:property-type)
  (:shadowing-import-from #:apispec/classes/schema
                          #:schema
                          #:number
                          #:string
                          #:boolean
                          #:array
                          #:array-items)
  (:import-from #:assoc-utils
                #:delete-from-alist)
  (:export #:encode-data-to-json))
(in-package #:apispec/body/encoder/json)

(declaim (ftype (function (t schema)) encode-data-to-json))

(defvar *empty* '#:empty)

(defun encode-object (value schema)
  (write-char #\{)
  (let ((rest-value (copy-seq value))
        missing)
    (loop for (prop . rest) on (object-properties schema)
          for name = (property-name prop)
          for (key . field-value) = (assoc name rest-value :test #'string=)
          if (and (null key)
                  (not (schema-nullable-p (property-type prop))))
            do (push name missing)
          else
            do (prin1 name)
               (write-char #\:)
               (encode-data-to-json field-value (property-type prop))
               (setf rest-value (delete-from-alist rest-value name))
          when rest
            do (write-char #\,))
    (when (or missing rest-value)
      (error 'body-encode-object-error
             :value value
             :schema schema
             :missing (nreverse missing)
             :unpermitted (mapcar #'car rest-value))))
  (write-char #\})
  (values))

(defun encode-array (value schema)
  (let ((items-schema (array-items schema)))
    (write-char #\[)
    (if (listp value)
        (mapl (lambda (items)
                (encode-data-to-json (first items) items-schema)
                (when (rest items)
                  (write-char #\,)))
              value)
        (loop with first = t
              for item across value
              do (unless first
                   (write-char #\,))
                 (setf first nil)
                 (encode-data-to-json item items-schema)))
    (write-char #\])))

(defun encode-boolean (value)
  (princ (case value
           ('t "true")
           ('nil "false")
           (otherwise (error 'body-encode-error
                             :value value
                             :schema (schema boolean)))))
  (values))

(defun encode-data-to-json (value schema)
  (typecase schema
    (object (encode-object value schema))
    (array (encode-array value schema))
    (boolean (encode-boolean value))
    (binary (error "Can't encode binary data to JSON"))
    (otherwise
      (typecase value
        (null
          (if (schema-nullable-p schema)
              (princ "null")
              ;; Not nullable error
              (error 'body-encode-error
                     :value value
                     :schema schema)))
        (otherwise (jojo:with-output (*standard-output*)
                     (jojo:%to-json value))))))
  (values))
