(defpackage #:apispec/tests/utils
  (:use #:cl
        #:apispec/utils
        #:rove)
  (:import-from #:apispec/utils
                #:*proper-list-type-checker*))
(in-package #:apispec/tests/utils)

(defstruct person name)

(deftest proper-list-tests
  (ok (typep nil 'proper-list))
  (ok (typep '(1) 'proper-list))
  (ok (typep '(1 2) 'proper-list))
  (ok (typep '(1 (2)) 'proper-list))
  (ng (typep '(1 . 2) 'proper-list))

  (ok (typep nil '(proper-list integer)))
  (ok (typep '(1) '(proper-list integer)))
  (ok (typep '(1 2) '(proper-list integer)))
  (ng (typep '(1 (2)) '(proper-list integer)))
  (ng (typep '(1 . 2) '(proper-list integer)))
  (ng (typep '(#\a #\b) '(proper-list integer)))

  (ok (typep nil '(proper-list person)))
  (ok (typep (list (make-person :name "Eitaro"))
             '(proper-list person)))
  (ng (typep '(1) '(proper-list person))))
