(defpackage #:apispec/tests/request
  (:use #:cl
        #:rove
        #:apispec/request)
  (:import-from #:apispec/types/schema
                #:schema)
  (:import-from #:assoc-utils
                #:alist-hash))
(in-package #:apispec/tests/request)

(deftest validate-request-tests
  (testing "header"
    (ok (equalp
          (validate-request
            (alist-hash
              '(("token" . "123,456"))) nil nil nil
            (list (make-instance 'parameter
                                 :name "token"
                                 :in "header"
                                 :description "token to be passed as a header"
                                 :required t
                                 :schema (schema (array :items 'integer))
                                 :style "simple"))
            nil)
          '(("token" . #(123 456)))))
    (ok (signals
          (validate-request
            (make-hash-table) nil nil nil
            (list (make-instance 'parameter
                                 :name "token"
                                 :in "header"
                                 :description "token to be passed as a header"
                                 :required t
                                 :schema (schema (array :items 'integer))
                                 :style "simple"))
            nil)
          'request-validation-failed)))
  (testing "query"
    (ok (equalp
          (validate-request
            nil nil "id=abc&id=opq&id=xyz" nil
            (list (make-instance 'parameter
                                 :name "id"
                                 :in "query"
                                 :description "ID of the object to fetch"
                                 :required nil
                                 :schema (schema (array :items 'string))
                                 :style "form"
                                 :explode t))
            nil)
          '(("id" . #("abc" "opq" "xyz")))))
    (ok (equalp
          (validate-request
            nil nil "id=abc,opq,xyz" nil
            (list (make-instance 'parameter
                                 :name "id"
                                 :in "query"
                                 :description "ID of the object to fetch"
                                 :required nil
                                 :schema (schema (array :items 'string))
                                 :style "form"
                                 :explode nil))
            nil)
          '(("id" . #("abc" "opq" "xyz")))))
    (ok (equalp
          (validate-request
            nil nil "uid=abc,opq,xyz" nil
            (list (make-instance 'parameter
                                 :name "id"
                                 :in "query"
                                 :description "ID of the object to fetch"
                                 :required nil
                                 :schema (schema (array :items 'string))
                                 :style "form"
                                 :explode nil))
            nil)
          '(("id"))))))
