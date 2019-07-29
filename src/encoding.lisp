(uiop:define-package #:apispec/encoding
    (:use #:cl)
  (:use-reexport #:apispec/encoding/core
                 #:apispec/encoding/parse))
(in-package #:apispec/encoding)
