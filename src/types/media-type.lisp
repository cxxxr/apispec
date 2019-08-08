(uiop:define-package #:apispec/types/media-type
  (:use #:cl)
  (:use-reexport #:apispec/types/media-type/class
                 #:apispec/types/media-type/parse))
(in-package #:apispec/types/media-type)
