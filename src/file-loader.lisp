(defpackage #:apispec/file-loader
  (:use #:cl)
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
           #:spec-document
           #:spec-version
           #:spec-router
           #:spec-schemas
           #:load-from-file))
(in-package #:apispec/file-loader)

(defvar *current-path-rule* nil)
(defvar *current-method* nil)

(defun gethash* (key hash)
  (let* ((not-found '#:not-found)
         (value (gethash key hash not-found)))
    (when (and (eq value not-found)
               *current-method*
               *current-path-rule*)
      (error "~A ~A: ~A key does not exist"
             *current-method*
             *current-path-rule*
             key))
    value))

(defun open-yaml-file (file)
  (let ((file-path (probe-file file)))
    (unless file-path
      (error "File not exists: ~A" file))
    (let ((doc (yaml:parse file-path)))
      (unless (gethash "openapi" doc)
        (error "Invalid OpenAPI3 YAML format: 'openapi' version is not found"))
      (unless (asdf/driver:version<= 3 (gethash "openapi" doc))
        (error "Unsupported OpenAPI3 version: ~S" (gethash "openapi" doc)))
      doc)))

(defgeneric make-from-hash (class hash)
  (:method ((class symbol) hash)
    (make-from-hash (find-class class) hash)))

(defun existsp (key hash)
  (nth-value 1 (gethash key hash)))

(defmethod make-from-hash ((class (eql (find-class 'parameter))) hash)
  (if (gethash "$ref" hash)
      (make-from-hash class (get-component-hash (gethash "$ref" hash)))
      (apply #'make-instance 'parameter
             :name (gethash "name" hash)
             :in (gethash "in" hash)
             :required (gethash "required" hash)
             :schema (make-from-hash 'schema (gethash "schema" hash))
             :allow-reserved (gethash "allowReserved" hash)
             (append (and (existsp "style" hash)
                          `(:style ,(gethash "style" hash)))
                     (and (existsp "explode" hash)
                          `(:explode ,(gethash "explode" hash)))))))

(defmethod make-from-hash ((class (eql (find-class 'schema))) hash)
  (if (gethash "$ref" hash)
      (make-from-hash class (get-component-hash (gethash "$ref" hash)))
      (let ((type (gethash "type" hash))
            (format (gethash "format" hash))
            (common-args
              (append (and (existsp "default" hash)
                           (list :default (gethash "default" hash)))
                      (list :enum (gethash "enum" hash)
                            :nullable (gethash "nullable" hash)
                            :deprecated (gethash "deprecated" hash)))))
        (cond
          ((or (string= type "object")
               (and (null type)
                    (gethash "properties" hash)))
           (apply #'make-instance 'object
                  :required (gethash "required" hash)
                  :properties (and (existsp "properties" hash)
                                   (loop for key being each hash-key of (gethash "properties" hash)
                                         using (hash-value value)
                                         collect (make-instance 'property
                                                                :name key
                                                                :type (make-from-hash 'schema value))))
                  :max-properties (gethash "maxProperties" hash)
                  :min-properties (gethash "minProperties" hash)
                  :additional-properties (if (typep (gethash "additionalProperties" hash) 'cl:boolean)
                                             (gethash "additionalProperties" hash)
                                             (make-from-hash 'schema (gethash "additionalProperties" hash)))
                  common-args))
          ((or (string= type "number")
               (string= type "integer"))
           (apply #'make-instance (cond
                                    ((string= format "float") 'float)
                                    ((string= format "double") 'double)
                                    ((string= type "integer") 'integer)
                                    (t 'number))
                  :multiple-of (gethash "multipleOf" hash)
                  :maximum (gethash "maximum" hash)
                  :exclusive-maximum (gethash "exclusiveMaximum" hash)
                  :minimum (gethash "minimum" hash)
                  :exclusive-minimum (gethash "exclusiveMinimum" hash)
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
                  :max-length (gethash "maxLength" hash)
                  :min-length (gethash "minLength" hash)
                  :pattern (gethash "pattern" hash)
                  common-args))
          ((string= type "boolean")
           (apply #'make-instance 'boolean common-args))
          ((string= type "array")
           (apply #'make-instance 'array
                  :items (and (gethash "items" hash)
                              (make-from-hash 'schema (gethash "items" hash)))
                  :max-items (gethash "maxItems" hash)
                  :min-items (gethash "minItems" hash)
                  :unique-items (gethash "uniqueItems" hash)
                  common-args))
          ((or (gethash "oneOf" hash)
               (gethash "anyOf" hash)
               (gethash "allOf" hash))
           (apply #'make-instance 'composition-schema
                  :one-of (mapcar (lambda (subschema)
                                    (make-from-hash 'schema subschema))
                                  (gethash "oneOf" hash))
                  :any-of (mapcar (lambda (subschema)
                                    (make-from-hash 'schema subschema))
                                  (gethash "anyOf" hash))
                  :all-of (mapcar (lambda (subschema)
                                    (make-from-hash 'schema subschema))
                                  (gethash "allOf" hash))
                  common-args))
          ((gethash "not" hash)
           (apply #'make-instance 'negative-schema
                  :not (make-from-hash 'schema (gethash "not" hash))
                  common-args))
          (t
           (apply #'make-instance 'schema common-args))))))

(defmethod make-from-hash ((class (eql (find-class 'path-item))) hash)
  (make-instance 'path-item
                 :summary (gethash "summary" hash)
                 :description (gethash "description" hash)
                 :parameters (mapcar (lambda (param) (make-from-hash 'parameter param))
                                     (gethash "parameters" hash))
                 :get (let ((*current-method* :get))
                        (make-from-hash 'operation (gethash "get" hash)))
                 :put (let ((*current-method* :put))
                        (make-from-hash 'operation (gethash "put" hash)))
                 :post (let ((*current-method* :post))
                         (make-from-hash 'operation (gethash "post" hash)))
                 :delete (let ((*current-method* :delete))
                           (make-from-hash 'operation (gethash "delete" hash)))
                 :options (let ((*current-method* :options))
                            (make-from-hash 'operation (gethash "options" hash)))
                 :head (let ((*current-method* :head))
                         (make-from-hash 'operation (gethash "head" hash)))
                 :patch (let ((*current-method* :patch))
                          (make-from-hash 'operation (gethash "patch" hash)))
                 :trace (let ((*current-method* :trace))
                          (make-from-hash 'operation (gethash "trace" hash)))))

(defmethod make-from-hash ((class (eql (find-class 'operation))) hash)
  (when hash
    (make-instance 'operation
                   :tags (gethash "tags" hash)
                   :summary (gethash "summary" hash)
                   :description (gethash "description" hash)
                   :id (gethash "id" hash)
                   :parameters (mapcar (lambda (param) (make-from-hash 'parameter param))
                                       (gethash "parameters" hash))
                   :request-body (and (gethash "requestBody" hash)
                                      (make-from-hash 'request-body (gethash "requestBody" hash)))
                   :responses (loop for key being each hash-key of (gethash* "responses" hash)
                                    using (hash-value value)
                                    collect (cons (princ-to-string key)
                                                  (make-from-hash 'response value)))
                   :deprecated (gethash "deprecated" hash))))

(defmethod make-from-hash ((class (eql (find-class 'response))) hash)
  (if (gethash "$ref" hash)
      (make-from-hash class (get-component-hash (gethash "$ref" hash)))
      (make-instance 'response
                     :description (gethash "description" hash)
                     :headers (and (gethash "headers" hash)
                                   (loop for key being each hash-key of (gethash "headers" hash)
                                         using (hash-value value)
                                         collect (cons key
                                                       (make-from-hash 'header value))))
                     :content (and (gethash "content" hash)
                                   (loop for key being each hash-key of (gethash "content" hash)
                                         using (hash-value value)
                                         collect (cons key
                                                       (and value
                                                            (make-from-hash 'media-type value))))))))

(defmethod make-from-hash ((class (eql (find-class 'request-body))) hash)
  (if (gethash "$ref" hash)
      (make-from-hash class (get-component-hash (gethash "$ref" hash)))
      (make-instance 'request-body
                     :description (gethash "description" hash)
                     :content (loop for key being each hash-key of (gethash "content" hash)
                                    using (hash-value value)
                                    collect (cons key
                                                  (make-from-hash 'media-type value)))
                     :required (gethash "required" hash))))

(defmethod make-from-hash ((class (eql (find-class 'media-type))) hash)
  (make-instance 'media-type
                 :schema (and (gethash "schema" hash)
                              (make-from-hash 'schema (gethash "schema" hash)))
                 :encoding (and (gethash "encoding" hash)
                                (loop for key being each hash-key of (gethash "encoding" hash)
                                      using (hash-value value)
                                      collect (cons key
                                                    (make-from-hash 'encoding value))))))

(defmethod make-from-hash ((class (eql (find-class 'encoding))) hash)
  (apply #'make-instance 'encoding
         :content-type (gethash "contentType" hash)
         :headers (and (gethash "headers" hash)
                       (loop for key being each hash-key of (gethash "headers" hash)
                             using (hash-value value)
                             collect (cons key
                                           (make-from-hash 'header value))))
         :allow-reserved (gethash "allowReserved" hash)
         (append (and (existsp "style" hash)
                      `(:style ,(gethash "style" hash)))
                 (and (existsp "explode" hash)
                      `(:expldoe ,(gethash "explode" hash))))))

(defmethod make-from-hash ((class (eql (find-class 'header))) hash)
  (if (gethash "$ref" hash)
      (make-from-hash class (get-component-hash (gethash "$ref" hash)))
      (make-instance 'header
                     :required (gethash "required" hash)
                     :schema (and (gethash "schema" hash)
                                  (make-from-hash 'schema (gethash "schema" hash)))
                     :explode (gethash "explode" hash))))

(defvar *doc*)

(defun get-component-hash (key &optional (hash *doc*))
  (reduce (lambda (current path)
            (or (gethash path current)
                (error "No component: ~S" key)))
          (rest (ppcre:split "/" key))
          :initial-value hash))

(defun get-paths-object (parsed-hash)
  (let ((*doc* parsed-hash))
    (loop for path-rule being each hash-key of (gethash "paths" parsed-hash)
          using (hash-value path-item)
          collect (cons path-rule
                        (let ((*current-path-rule* path-rule))
                          (make-from-hash 'path-item path-item))))))

(defun get-schemas-object (parsed-hash)
  (let ((*doc* parsed-hash))
    (when-let* ((components (gethash "components" parsed-hash))
                (schemas (gethash "schemas" components)))
      (loop for schema-name being each hash-key of schemas
            using (hash-value schema-value)
            collect (cons schema-name (make-from-hash 'schema schema-value))))))

(defstruct spec
  (document nil :type hash-table)
  router
  schemas)

(defun spec-version (spec)
  (values (gethash "openapi" (spec-document spec))))

(defun load-from-file (file)
  (let ((document (open-yaml-file file)))
    (make-spec :document document
               :router (make-router (get-paths-object document))
               :schemas (get-schemas-object document))))
