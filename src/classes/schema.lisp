(uiop:define-package #:apispec/classes/schema
  (:mix-reexport #:apispec/classes/schema/core
                 #:apispec/classes/schema/coerce
                 #:apispec/classes/schema/validate
                 #:apispec/classes/schema/errors
                 #:cl))
