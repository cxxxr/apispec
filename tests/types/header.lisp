(defpackage #:apispec/tests/types/header
  (:use #:cl
        #:rove
        #:apispec/types/header))
(in-package #:apispec/tests/types/header)

(deftest header-tests
  (ng (header-required-p (make-instance 'header)))
  (ok (null (header-schema (make-instance 'header))))
  (ng (header-explode-p (make-instance 'header))))

(deftest coerce-with-header-tests
  (testing "required"
    (let ((header (make-instance 'header
                                 :required t)))
      (ok (signals (coerce-with-header nil header)
                   'header-missing)))))
