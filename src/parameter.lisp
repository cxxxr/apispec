(uiop:define-package #:apispec/parameter
    (:use #:cl)
  (:use-reexport #:apispec/parameter/core
                 #:apispec/parameter/parse))
(in-package #:apispec/parameter)
