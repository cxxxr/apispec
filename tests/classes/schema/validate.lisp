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
               'schema-object-error))
  (ok (validate-data '()
                     '(object
                       (("name" string)))))
  (ok (signals (validate-data "foo"
                              (schema (string :format "email")))
               'schema-validation-failed))
  (ok (validate-data "foo@gmail.com"
                     (schema (string :format "email"))))
  (ok (validate-data "d9d29401-3feb-48b2-ac79-54cee011717d"
                     (schema (string :format "uuid"))))
  (signals (validate-data "foo"
                          (schema (string :format "uuid")))
           'schema-validation-failed)
  (ok (validate-data #(1 2 3)
                     (schema (array :items 'integer))))
  (ok (signals (validate-data #(1 2 #\a)
                              (schema (array :items 'integer)))
               'schema-validation-failed))
  (let ((enum '("foo" "bar")))
    (dolist (string enum)
      (ok (apispec:coerce-data string (schema (string :enum enum)))))
    (ok (signals (apispec:coerce-data "hoge" (schema (string :enum enum)))
                 'schema-validation-failed)))
  (ok (validate-data "2020-01-21T23:03:22.503288Z"
                     (schema (date-time))))
  (ok (signals (validate-data "2020-01-21T23:03:22.503a"
                              (schema (date-time)))
               'schema-validation-failed))
  (ok (validate-data "2020-01-21T23:03:22Z"
                     (schema (date-time))))
  (ok (signals (validate-data "2020-01-21T23:03:22"
                              (schema (date-time)))
               'schema-validation-failed))
  (ok (validate-data "2020-01-21"
                     (schema (date))))
  (ok (signals (validate-data "2020-01-21T23:03:22.503288Z"
                              (schema (date)))
               'schema-validation-failed)))
