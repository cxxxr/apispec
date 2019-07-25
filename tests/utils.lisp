(defpackage #:apispec/tests/utils
  (:use #:cl
        #:apispec/utils
        #:rove)
  (:import-from #:apispec/utils
                #:*proper-list-type-checker*))
(in-package #:apispec/tests/utils)

(defstruct person name)

(deftest proper-list-tests
  (setf *proper-list-type-checker*
        (make-hash-table :test 'equal))

  (ok (typep nil 'proper-list))
  (ok (typep '(1) 'proper-list))
  (ok (typep '(1 2) 'proper-list))
  (ok (typep '(1 (2)) 'proper-list))
  (ng (typep '(1 . 2) 'proper-list))

  (ok (= (hash-table-count *proper-list-type-checker*) 0))

  (ok (typep nil '(proper-list integer)))
  (ok (typep '(1) '(proper-list integer)))
  (ok (typep '(1 2) '(proper-list integer)))
  (ng (typep '(1 (2)) '(proper-list integer)))
  (ng (typep '(1 . 2) '(proper-list integer)))
  (ng (typep '(#\a #\b) '(proper-list integer)))

  (ok (= (hash-table-count *proper-list-type-checker*) 1))

  (ok (typep nil '(proper-list person)))
  (ok (typep (list (make-person :name "Eitaro"))
             '(proper-list person)))
  (ng (typep '(1) '(proper-list person)))

  (ok (= (hash-table-count *proper-list-type-checker*) 2)))
