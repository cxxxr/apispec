(defpackage #:apispec/tests/classes/header
  (:use #:cl
        #:rove
        #:apispec/classes/header))
(in-package #:apispec/tests/classes/header)

(deftest header-tests
  (ng (header-required-p (make-instance 'header)))
  (ok (eq (header-schema (make-instance 'header)) t))
  (ng (header-explode-p (make-instance 'header))))

(deftest coerce-with-header-tests
  (testing "required"
    (let ((header (make-instance 'header
                                 :required t)))
      (ok (signals (coerce-with-header nil header)
                   'header-missing)))))
