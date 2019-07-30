(uiop:define-package #:apispec
    (:nicknames #:apispec/main)
  (:mix #:cl
        #:apispec/schema)
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

           #:coerce-failed
           #:coerce-data

           #:validation-failed
           #:validate-data

           #:encode-data)
  (:mix-reexport #:apispec/header
                 #:apispec/encoding
                 #:apispec/request
                 #:apispec/response
                 #:apispec/operation))
