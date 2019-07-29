(uiop:define-package #:apispec/operation
    (:use #:cl)
  (:use-reexport #:apispec/operation/core
                 #:apispec/operation/header
                 #:apispec/operation/media-type
                 #:apispec/operation/request-body
                 #:apispec/operation/response))
(in-package #:apispec/operation)
