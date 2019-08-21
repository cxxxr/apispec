(uiop:define-package #:apispec
  (:nicknames #:apispec/main)
  (:mix #:cl
        #:apispec/classes/schema)
  (:export #:schema
           #:schemap
           #:schema-type
           #:schema-format
           #:schema-enum
           #:schema-default
           #:schema-has-default-p
           #:schema-nullable-p
           #:schema-deprecated-p
           #:make-schema
           #:defschema

           #:number-multiple-of
           #:number-maximum
           #:number-exclusive-maximum-p
           #:number-minimum
           #:number-exclusive-minimum-p

           #:string-max-length
           #:string-min-length
           #:string-pattern

           #:binary
           #:date
           #:date-time
           #:email
           #:uuid

           #:array-items
           #:array-max-items
           #:array-min-items

           #:object
           #:object-required
           #:object-properties
           #:object-max-properties
           #:object-min-properties
           #:object-additional-properties

           #:property
           #:property-name
           #:property-type

           #:coerce-data
           #:validate-data

           #:schema-error
           #:schema-coercion-failed
           #:schema-validation-failed)
  (:mix-reexport #:apispec/classes/header
                 #:apispec/classes/encoding
                 #:apispec/classes/media-type
                 #:apispec/classes/operation
                 #:apispec/classes/parameter
                 #:apispec/classes/request-body
                 #:apispec/classes/response
                 #:apispec/classes/path
                 #:apispec/body
                 #:apispec/router
                 #:apispec/errors))
