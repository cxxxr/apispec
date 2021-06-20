(defpackage #:apispec/file-loader
  (:use #:cl
        #:openapi-parser/schema/3/interface)
  (:import-from #:apispec/classes/schema
                #:schema
                #:composition-schema)
  (:import-from #:apispec/classes/parameter
                #:parameter)
  (:import-from #:apispec/classes/path
                #:path-item)
  (:import-from #:apispec/classes/operation
                #:operation)
  (:import-from #:apispec/classes/response
                #:response)
  (:import-from #:apispec/classes/request-body
                #:request-body)
  (:import-from #:apispec/classes/media-type
                #:media-type)
  (:import-from #:apispec/classes/encoding
                #:encoding)
  (:import-from #:apispec/classes/header
                #:header)
  (:shadowing-import-from #:apispec/classes/schema
                          #:number
                          #:double
                          #:binary
                          #:date
                          #:date-time
                          #:email
                          #:uuid
                          #:object
                          #:property
                          #:float
                          #:integer
                          #:string
                          #:byte
                          #:boolean
                          #:array)
  (:import-from #:apispec/router
                #:make-router)
  (:import-from #:alexandria
                #:when-let*)
  (:import-from #:cl-yaml
                #:parse)
  (:export #:spec
           #:spec-version
           #:spec-router
           #:spec-schemas
           #:load-from-file))
(in-package #:apispec/file-loader)

(defvar *path-item-parameters*)

(defgeneric make-from (schema))

(defmethod make-from ((schema <parameter>))
  (apply #'make-instance
         'parameter
         :name (->name schema)
         :in (->in schema)
         :required (->required schema)
         :schema (when (->schema schema)
                   (make-from (->schema schema)))
         :allow-reserved (->allow-reserved schema)
         (append (and (->style schema)
                      `(:style ,(->style schema)))
                 (and (->explode schema)
                      `(:explode ,(->explode schema))))))

;;
;; These accessors are deleted in 3.1.x

(defun get-nullable (schema)
  (typecase schema
    (openapi-parser/schema/3.0.1:<schema> (->nullable schema))
    (otherwise nil)))

(defun get-deprecated (schema)
  (typecase schema
    (openapi-parser/schema/3.0.1:<schema> (->deprecated schema))
    (otherwise nil)))

(defmethod make-from ((schema <schema>))
  (let ((type (->type schema))
        (format (->format schema))
        (common-args
          (append (and (->default schema)
                       (list :default (->default schema)))
                  (list :enum (->enum schema)
                        :nullable (get-nullable schema)
                        :deprecated (get-deprecated schema)))))
    (cond
      ((or (string= type "object")
           (and (null type) ; dead code?
                (->properties schema)))
       (apply #'make-instance 'object
              :required (->required schema)
              :properties (and (->properties schema)
                               (loop for key being each hash-key of (->properties schema)
                                     using (hash-value value)
                                     collect (make-instance 'property
                                                            :name key
                                                            :type (make-from value))))
              :max-properties (->max-properties schema)
              :min-properties (->min-properties schema)
              :additional-properties (if (typep (->additional-properties schema) 'cl:boolean)
                                         (->additional-properties schema)
                                         (make-from (->additional-properties schema)))
              common-args))
      ((or (string= type "number")
           (string= type "integer"))
       (apply #'make-instance (cond
                                ((string= format "float") 'float)
                                ((string= format "double") 'double)
                                ((string= type "integer") 'integer)
                                (t 'number))
              :multiple-of (->multiple-of schema)
              :maximum (->maximum schema)
              :exclusive-maximum (->exclusive-maximum schema)
              :minimum (->minimum schema)
              :exclusive-minimum (->exclusive-minimum schema)
              common-args))
      ((string= type "string")
       (apply #'make-instance (cond
                                ((string= format "byte") 'byte)
                                ((string= format "binary") 'binary)
                                ((string= format "date") 'date)
                                ((string= format "date-time") 'date-time)
                                ((string= format "email") 'email)
                                ((string= format "uuid") 'uuid)
                                (t 'string))
              :max-length (->max-length schema)
              :min-length (->min-length schema)
              :pattern (->pattern schema)
              common-args))
      ((string= type "boolean")
       (apply #'make-instance 'boolean common-args))
      ((string= type "array")
       (apply #'make-instance 'array
              :items (and (->items schema)
                          (make-from (->items schema)))
              :max-items (->max-items schema)
              :min-items (->min-items schema)
              :unique-items (->unique-items schema)
              common-args))
      ((or (->one-of schema)
           (->any-of schema)
           (->all-of schema))
       (apply #'make-instance 'composition-schema
              :one-of (mapcar (lambda (subschema)
                                (make-from subschema))
                              (->one-of schema))
              :any-of (mapcar (lambda (subschema)
                                (make-from subschema))
                              (->any-of schema))
              :all-of (mapcar (lambda (subschema)
                                (make-from subschema))
                              (->all-of schema))
              common-args))
      ((->not schema)
       (apply #'make-instance 'negative-schema ; ERROR
              :not (make-from (->not schema))
              common-args))
      (t
       (apply #'make-instance 'schema common-args)))))

(defmethod make-from ((schema <path-item>))
  (let ((*path-item-parameters* (->parameters schema)))
    (make-instance 'path-item
                   :summary (->summary schema)
                   :description (->description schema)
                   :parameters (mapcar #'make-from *path-item-parameters*)
                   :get (when (->get schema) (make-from (->get schema)))
                   :put (when (->put schema) (make-from (->put schema)))
                   :post (when (->post schema) (make-from (->post schema)))
                   :delete (when (->delete schema) (make-from (->delete schema)))
                   :options (when (->options schema) (make-from (->options schema)))
                   :head (when (->head schema) (make-from (->head schema)))
                   :patch (when (->patch schema) (make-from (->patch schema)))
                   :trace (when (->trace schema) (make-from (->trace schema))))))

(defun parameter= (parameter1 parameter2)
  (check-type parameter1 <parameter>)
  (check-type parameter2 <parameter>)
  (and (equal (->name parameter1) (->name parameter2))
       (equal (->in parameter1) (->in parameter2))))

(defun merge-parameters (parameters1 parameters2)
  (append (remove-if (lambda (parameter)
                       (member parameter parameters2
                               :test #'parameter=))
                     parameters1)
          parameters2))

(defmethod make-from ((schema <operation>))
  (make-instance 'operation
                 :%schema schema
                 :tags (->tags schema)
                 :summary (->summary schema)
                 :description (->description schema)
                 :parameters (mapcar #'make-from
                                     (merge-parameters *path-item-parameters*
                                                       (->parameters schema)))
                 :request-body (and (->request-body schema)
                                    (make-from (->request-body schema)))
                 :responses (loop :for (status-code . response) :in (->field* (->responses schema))
                                  :collect (cons status-code (make-from response)))
                 :deprecated (->deprecated schema)))

(defmethod make-from ((schema <response>))
  (make-instance 'response
                 :description (->description schema)
                 :headers (and (->headers schema)
                               (loop for key being each hash-key of (->headers schema)
                                     using (hash-value value)
                                     collect (cons key
                                                   (make-from value))))
                 :content (and (->content schema)
                               (loop for key being each hash-key of (->content schema)
                                     using (hash-value value)
                                     collect (cons key
                                                   (and value
                                                        (make-from value)))))))

(defmethod make-from ((schema <request-body>))
  (make-instance 'request-body
                 :description (->description schema)
                 :content (loop for key being each hash-key of (->content schema)
                                using (hash-value value)
                                collect (cons key
                                              (make-from value)))
                 :required (->required schema)))

(defmethod make-from ((schema <media-type>))
  (make-instance 'media-type
                 :schema (and (->schema schema)
                              (make-from (->schema schema)))
                 :encoding (and (->encoding schema)
                                (loop for key being each hash-key of (->encoding schema)
                                      using (hash-value value)
                                      collect (cons key
                                                    (make-from value))))))

(defmethod make-from ((schema <encoding>))
  (apply #'make-instance 'encoding
         :content-type (->content-type schema)
         :headers (and (->headers schema)
                       (loop for key being each hash-key of (->headers schema)
                             using (hash-value value)
                             collect (cons key
                                           (make-from value))))
         :allow-reserved (->allow-reserved schema)
         (append (and (->style schema)
                      `(:style ,(->style schema)))
                 (and (->explode schema)
                      `(:expldoe ,(->explode schema))))))

(defmethod make-from ((schema <header>))
  (make-instance 'header
                 :required (->required schema)
                 :schema (and (->schema schema)
                              (make-from (->schema schema)))
                 :explode (->explode schema)))

(defun get-paths-object (openapi)
  (loop :for (path . path-item) :in (->field* (->paths openapi))
        :collect (cons path
                       (make-from path-item))))

(defun get-schemas-object (openapi)
  (when-let* ((components (->components openapi))
              (schemas (->schemas components)))
    (loop for schema-name being each hash-key of schemas
          using (hash-value schema-value)
          collect (cons schema-name (make-from schema-value)))))

(defstruct spec
  openapi
  router
  schemas)

(defun spec-version (spec)
  (->openapi (spec-openapi spec)))

(defun load-from-file (file)
  (let ((openapi (openapi-parser:parse-file file))
        (*path-item-parameters* '()))
    (make-spec :openapi openapi
               :router (make-router (get-paths-object openapi))
               :schemas (get-schemas-object openapi))))
