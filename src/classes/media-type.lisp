(uiop:define-package #:apispec/classes/media-type
  (:use #:cl)
  (:use-reexport #:apispec/classes/media-type/class
                 #:apispec/classes/media-type/parse))
(in-package #:apispec/classes/media-type)
