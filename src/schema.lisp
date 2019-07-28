(uiop:define-package #:apispec/schema
    (:mix #:cl)
  (:use-reexport #:apispec/schema/core
                 #:apispec/schema/coerce
                 #:apispec/schema/validate
                 #:apispec/schema/encode))
