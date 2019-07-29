(defpackage #:apispec/tests/request/parameter
  (:use #:cl
        #:apispec/request/parameter
        #:rove)
  (:import-from #:apispec/schema
                #:schema))
(in-package #:apispec/tests/request/parameter)

(deftest parse-with-parameter-tests
  (ok (equal (parse-with-parameter "id=10"
                                   (make-instance 'parameter
                                                  :name "id"
                                                  :in "query"
                                                  :schema (schema integer)))
             10)))
