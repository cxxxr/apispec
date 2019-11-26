(uiop:define-package #:apispec/tests/classes/schema/coerce
  (:mix #:apispec/classes/schema/core
        #:cl)
  (:use #:apispec/classes/schema/coerce
        #:apispec/classes/schema/composition
        #:apispec/classes/schema/errors
        #:rove)
  (:import-from #:apispec/classes/schema/validate
                #:validation-failed)
  (:import-from #:local-time
                #:timestamp=
                #:universal-to-timestamp)
  (:import-from #:assoc-utils
                #:aget
                #:alist=))
(in-package #:apispec/tests/classes/schema/coerce)

(deftest coerce-number-tests
  (ok (eql (coerce-data 1 'number) 1))
  (ok (eql (coerce-data 1 'integer) 1))
  (ok (eql (coerce-data 1 'float) 1.0))
  (ok (eql (coerce-data "1" 'integer) 1))
  (ok (eql (coerce-data "1.2" 'float) 1.2))
  (ok (eql (coerce-data "1.2" 'double) '1.2d0)))

(deftest coerce-string-tests
  (ok (equal (coerce-data "a" 'string) "a"))
  (ok (signals (coerce-data #\a 'string)
               'schema-coercion-failed))
  (ok (equal (handler-bind ((schema-coercion-failed
                              (lambda (condition)
                                (invoke-restart (find-restart 'use-value condition) "a"))))
               (coerce-data #\a 'string))
             "a"))
  (ok (signals (coerce-data 1 'string)
               'schema-coercion-failed))
  (let ((date (coerce-data "2019-04-15" 'date)))
    (ok (typep date 'local-time:timestamp))
    (ok (= (local-time:timestamp-year date) 2019))
    (ok (= (local-time:timestamp-month date) 4))
    (ok (= (local-time:timestamp-day date) 15))
    (ok (= (local-time:timestamp-hour date) 0))
    (ok (= (local-time:timestamp-minute date) 0))
    (ok (= (local-time:timestamp-second date) 0)))
  (let ((date (coerce-data "2019-04-15T01:02:03+09:00" 'date-time))
        (local-time:*default-timezone* local-time:+gmt-zone+))
    (ok (typep date 'local-time:timestamp))
    (ok (= (local-time:timestamp-year date) 2019))
    (ok (= (local-time:timestamp-month date) 4))
    (ok (= (local-time:timestamp-day date) 14))
    (ok (= (local-time:timestamp-hour date) 16))
    (ok (= (local-time:timestamp-minute date) 2))
    (ok (= (local-time:timestamp-second date) 3)))
  (ok (signals (coerce-data "1" 'boolean)))
  (ok (signals (coerce-data "0" 'boolean)))
  (let ((*coerce-integer-string-to-boolean* t))
    (ok (eq (coerce-data "1" 'boolean) t))
    (ok (eq (coerce-data "0" 'boolean) nil)))
  (ok (eq (coerce-data t 'boolean) t))
  (ok (eq (coerce-data nil 'boolean) nil)))

(deftest coerce-array-tests
  (ok (equalp (coerce-data '(1 2 3) 'array)
              #(1 2 3)))
  (ok (equalp (coerce-data '() 'array)
              #()))
  (ok (signals (coerce-data '(1 2 3) '(array 10))
          'schema-validation-failed))
  (ok (equalp (coerce-data '("1" "-2" "3") '(array :items integer))
              #(1 -2 3))))

(defmacro signals* (form condition &rest slot-value-pairs)
  (let ((c (gensym))
        (condition-type (gensym)))
    `(let ((,condition-type ,condition))
       (typep (block nil
                (handler-bind ((condition
                                 (lambda (,c)
                                   (when (and (typep ,c ,condition-type)
                                              ,@(loop :for (slot-name value) :on slot-value-pairs :by #'cddr
                                                      :collect `(equal (slot-value ,c ,slot-name) ,value)))
                                     (return ,c)))))
                  ,form
                  nil))
              ,condition-type))))

(deftest coerce-object-tests
  (ok (equalp (coerce-data '(("name" . "fukamachi")) 'object)
              '(("name" . "fukamachi"))))
  (ok (equalp (coerce-data '(("name" . "fukamachi")) '(object
                                                       (("name" string))))
              '(("name" . "fukamachi"))))
  (ok (signals* (coerce-data '(("name" . 1))
                             '(object
                               (("name" string))))
                'schema-object-invalid-value
                'apispec/classes/schema/errors::keys '("name")))
  (ok (signals* (coerce-data '(("foo" . 1)
                               ("bar" . "a"))
                             '(object
                               (("foo" string)
                                ("bar" number)
                                ("baz" number))))
                'schema-object-invalid-value
                'apispec/classes/schema/errors::keys '("foo" "bar")))
  (ok (equalp (coerce-data '(("hi" . "all"))
                           '(object
                             (("name" string))))
              '(("hi" . "all"))))
  (ok (signals (coerce-data '(("hi" . "all"))
                            '(object
                              (("name" string))
                              :required ("name")))
               'schema-validation-failed))

  (testing "additionalProperties"
    (ok (equal (coerce-data '(("name" . "fukamachi")
                              ("created-at" . "2019-04-30"))
                            '(object
                              (("name" string))
                              :additional-properties t))
               '(("name" . "fukamachi")
                 ("created-at" . "2019-04-30"))))
    (ok (signals (coerce-data '(("name" . "fukamachi")
                                ("created-at" . "2019-04-30"))
                              '(object
                                (("name" string))
                                :additional-properties nil))
                 'schema-object-unpermitted-key))
    (let ((data (coerce-data '(("name" . "fukamachi")
                               ("created-at" . "2019-04-30"))
                             '(object
                               (("name" string))
                               :additional-properties date))))
      (ok (equal (aget data "name") "fukamachi"))
      (ok (timestamp= (aget data "created-at")
                      (universal-to-timestamp
                       (encode-universal-time 0 0 0 30 4 2019))))
      (ok (= (length data) 2)))
    (ok (equal (coerce-data '(("name" . "fukamachi")
                              ("created-at" . nil))
                            '(object
                              (("name" string))
                              :additional-properties (or date null)))
               '(("name" . "fukamachi")
                 ("created-at" . nil))))))

(deftest coerce-default-tests
  (ok (equal (coerce-data nil '(string :default "none"))
             "none"))
  (ok (equal (coerce-data '()
                          '(object
                            (("name" (string :default "nobody")))))
             '(("name" . "nobody")))))

(deftest composition-schema-tests
  (testing "oneOf"
    (let ((schema (make-instance 'composition-schema
                                 :one-of
                                 (list (schema (object (("bark" boolean)
                                                        ("breed" (string :enum '("Dingo" "Husky" "Retriever" "Shepherd"))))))
                                       (schema (object (("hunts" boolean)
                                                        ("age" integer))))))))
      (ok (equal (coerce-data '(("bark" . t)
                                ("breed" . "Dingo"))
                              schema)
                 '(("bark" . t)
                   ("breed" . "Dingo"))))
      (ok (signals (coerce-data '(("bark" . t)
                                  ("hunts" . t))
                                schema)
                   'schema-coercion-failed))
      (ok (signals (coerce-data '(("bark" . t)
                                  ("hunts" . t)
                                  ("breed" . "Husky")
                                  ("age" . 3))
                                schema)
                   'schema-coercion-failed))))
  (testing "anyOf"
    (let ((schema (make-instance 'composition-schema
                                 :any-of
                                 (list (schema (object (("age" integer) ("nickname" string))
                                                       :required '("age")))
                                       (schema (object (("pet_type" (string :enum '("Cat" "Dog")))
                                                        ("hunts" boolean))
                                                       :required '("pet_type")))))))
      (ok (equal (coerce-data '(("age" . "11")) schema)
                 '(("age" . 11))))
      (ok (equal (coerce-data '(("pet_type" . "Cat") ("hunts" . t)) schema)
                 '(("pet_type" . "Cat") ("hunts" . t))))
      (ok (alist= (coerce-data '(("nickname" . "Fido")
                                 ("pet_type" . "Dog")
                                 ("age" . 4))
                               schema)
                  '(("nickname" . "Fido")
                    ("pet_type" . "Dog")
                    ("age" . 4)))))))

(deftest negative-schema-tests
  (ok (equal (coerce-data "Cat" (make-instance 'negative-schema :not (schema integer)))
             "Cat"))
  (ok (signals (coerce-data 11 (make-instance 'negative-schema :not (schema integer)))
               'schema-coercion-failed)))
