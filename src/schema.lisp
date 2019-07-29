(uiop:define-package #:apispec/schema
    (:mix-reexport #:apispec/schema/core
                   #:apispec/schema/coerce
                   #:apispec/schema/validate
                   #:apispec/schema/encode
                   #:cl))
