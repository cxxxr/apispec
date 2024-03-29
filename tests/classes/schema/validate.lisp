(uiop:define-package #:apispec/tests/classes/schema/validate
  (:mix #:apispec/classes/schema/core
        #:cl)
  (:use #:rove
        #:apispec/classes/schema/validate
        #:apispec/classes/schema/errors
        #:apispec/classes/schema/composition))
(in-package #:apispec/tests/classes/schema/validate)

(deftest validate-array-tests
  (ok (validate-data #(1 2 3) (schema (array :items 'integer))))

  (testing "array size"
    (dolist (schema (list '(array 10)
                          '(array :min-items 4)
                          '(array :max-items 2)))
      (ok (signals (validate-data #(1 2 3) schema)
                   'schema-validation-failed)
          (format nil "array size ~S" schema))))

  (testing "none array data"
    (dolist (value (list ""
                         0
                         t
                         nil
                         '(("a" . "b"))))
      (ok (signals (validate-data value (schema (array :items 'integer)))
                   'schema-validation-failed)
          (format nil "none array data ~S" value))))

  (testing "invalid element"
    (dolist (element (list #\a
                           "a"
                           t
                           nil
                           '(("a" . "b"))))
      (ok (signals (validate-data #(1 2 element)
                                  (schema (array :items 'integer)))
                   'schema-validation-failed)
          (format nil "invalid element ~S" element)))))

(deftest validate-object-tests
  (ok (validate-data '()
                     '(object
                       (("name" string)))))
  (ok (signals (validate-data '(("hi" . "all"))
                              '(object
                                (("name" string))
                                :required ("name")))
               'schema-object-error)))

(deftest validate-email-tests
  (ok (signals (validate-data "foo"
                              (schema (string :format "email")))
               'schema-validation-failed))
  (ok (validate-data "foo@gmail.com"
                     (schema (string :format "email")))))

(deftest validate-uuid-tests
  (ok (validate-data "d9d29401-3feb-48b2-ac79-54cee011717d"
                     (schema (string :format "uuid"))))
  (signals (validate-data "foo"
                          (schema (string :format "uuid")))
           'schema-validation-failed))

(deftest validate-time-tests
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

(deftest validate-json-tests
  (ok (validate-data "{\"key1\": 100}" (schema (string :format "json"))))
  (ok (signals (validate-data "{xx: 100}" (schema (string :format "json")))
               'schema-validation-failed))
  (ok (signals (validate-data "{xx: 100" (schema (string :format "json")))
               'schema-validation-failed)))

(deftest composition-schema-tests
  (testing "oneOf"
    (let ((schema (make-instance 'composition-schema
                                 :one-of
                                 (list (schema (object (("bark" boolean)
                                                        ("breed" (string :enum '("Dingo" "Husky" "Retriever" "Shepherd"))))
                                                       :required '("bark" "breed")))
                                       (schema (object (("hunts" boolean)
                                                        ("age" integer))
                                                       :required '("hunts" "age")))))))
      (ok (validate-data '(("bark" . t)
                           ("breed" . "Dingo"))
                         schema))
      (ok (signals (validate-data '(("bark" . t)
                                    ("hunts" . t))
                                  schema)
                   'schema-oneof-error))
      (ok (signals (validate-data '(("bark" . t)
                                    ("hunts" . t)
                                    ("breed" . "Husky")
                                    ("age" . 3))
                                  schema)
                   'schema-oneof-error))))
  (testing "anyOf"
    (let ((schema (make-instance 'composition-schema
                                 :any-of
                                 (list (schema (object (("age" integer) ("nickname" string))
                                                       :required '("age")))
                                       (schema (object (("pet_type" (string :enum '("Cat" "Dog")))
                                                        ("hunts" boolean))
                                                       :required '("pet_type")))))))
      (ok (validate-data '(("age" . 11)) schema))
      (ok (validate-data '(("pet_type" . "Cat") ("hunts" . t)) schema))
      (ok (signals (validate-data '("foo" . 0) schema)
                   'schema-anyof-error)))))
