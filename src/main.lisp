(uiop:define-package #:apispec
    (:nicknames #:apispec/main)
  (:mix #:cl)
  (:use-reexport #:apispec/schema
                 #:apispec/parameter))
