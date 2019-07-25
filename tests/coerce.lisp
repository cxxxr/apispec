(defpackage #:apispec/tests/coerce
  (:use #:cl
        #:apispec/coerce
        #:apispec/schema
        #:rove)
  (:import-from #:local-time))
(in-package #:apispec/tests/coerce)

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
          'coerce-failed))
  (ok (signals (coerce-data 1 'string)
          'coerce-failed))
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
  (ok (eq (coerce-data "true" 'boolean) t))
  (ok (eq (coerce-data "false" 'boolean) nil))
  (ok (eq (coerce-data t 'boolean) t))
  (ok (eq (coerce-data nil 'boolean) nil)))
