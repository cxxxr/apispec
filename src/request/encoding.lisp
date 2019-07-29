(uiop:define-package #:apispec/request/encoding
    (:use #:cl)
  (:use-reexport #:apispec/request/encoding/core
                 #:apispec/request/encoding/parse))
(in-package #:apispec/request/encoding)
