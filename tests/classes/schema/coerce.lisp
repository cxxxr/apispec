(uiop:define-package #:apispec/tests/classes/schema/coerce
  (:mix #:apispec/classes/schema/core
        #:cl)
  (:use #:apispec/classes/schema/coerce
        #:apispec/classes/schema/errors
        #:rove)
  (:import-from #:apispec/classes/schema/validate
                #:validation-failed)
  (:import-from #:local-time
                #:timestamp=
                #:universal-to-timestamp)
  (:import-from #:assoc-utils
                #:aget))
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
  (let ((date (coerce-data "2019-04-15T01:02:03+09:00" 'date-time)))
    (ok (typep date 'local-time:timestamp))
    (ok (= (local-time:timestamp-year date) 2019))
    (ok (= (local-time:timestamp-month date) 4))
    (ok (= (local-time:timestamp-day date) 15))
    (ok (= (local-time:timestamp-hour date) 1))
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

(deftest coerce-object-tests
  (ok (equalp (coerce-data '(("name" . "fukamachi")) 'object)
              '(("name" . "fukamachi"))))
  (ok (equalp (coerce-data '(("name" . "fukamachi")) '(object
                                                       (("name" string))))
              '(("name" . "fukamachi"))))
  (ok (signals (coerce-data '(("name" . 1)) '(object
                                              (("name" string))))
          'schema-coercion-failed))
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
            'schema-validation-failed))
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
