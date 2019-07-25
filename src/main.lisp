(uiop:define-package #:apispec
    (:nicknames #:apispec/main)
  (:mix #:cl)
  (:mix-reexport #:apispec/schema
                 #:apispec/coerce
                 #:apispec/validate
                 #:apispec/encode))
