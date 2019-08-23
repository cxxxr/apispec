(defpackage #:apispec/body/encoder/custom
  (:use #:cl)
  (:export #:encode-object))
(in-package #:apispec/body/encoder/custom)

(defgeneric encode-object (value)
  (:method (value)
    (typecase value
      ((or standard-object
           structure-object) (call-next-method))
      (otherwise value))))
