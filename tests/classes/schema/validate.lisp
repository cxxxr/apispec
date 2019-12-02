(uiop:define-package #:apispec/tests/classes/schema/validate
  (:mix #:apispec/classes/schema/core
        #:cl)
  (:use #:rove
        #:apispec/classes/schema/validate
        #:apispec/classes/schema/errors))
(in-package #:apispec/tests/classes/schema/validate)

(deftest validate-data-tests
  (ok (signals (validate-data #(1 2 3) '(array 10))
               'schema-validation-failed))
  (ok (signals (validate-data '(("hi" . "all"))
                              '(object
                                 (("name" string))
                                 :required ("name")))
               'schema-object-error)))
