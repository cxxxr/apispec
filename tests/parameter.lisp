(defpackage #:apispec/tests/parameter
  (:use #:cl
        #:apispec/parameter
        #:rove)
  (:import-from #:apispec/schema
                #:schema))
(in-package #:apispec/tests/parameter)

(deftest parse-value-tests
  (ok (equal (parse-value "id=10"
                          (make-instance 'parameter
                                         :name "id"
                                         :in "query"
                                         :schema (schema integer)))
             10)))
