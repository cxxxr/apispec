(defpackage #:apispec/tests/classes/parameter
  (:use #:cl
        #:rove
        #:apispec/classes/parameter)
  (:import-from #:apispec/classes/schema
                #:object
                #:schema)
  (:import-from #:assoc-utils
                #:alist-hash))
(in-package #:apispec/tests/classes/parameter)

(deftest parse-query-string-tests
  (ok (equalp (parse-query-string "id=abc&id=opq&id=xyz"
                                  (list
                                    (make-instance 'parameter
                                                   :name "id"
                                                   :in "query"
                                                   :description "ID of the object to fetch"
                                                   :required nil
                                                   :schema (schema (array :items 'string))
                                                   :style "form"
                                                   :explode t)))
              '(("id" . #("abc" "opq" "xyz")))))
  (ok (equalp (parse-query-string "id=abc,opq,xyz"
                                  (list
                                    (make-instance 'parameter
                                                   :name "id"
                                                   :in "query"
                                                   :description "ID of the object to fetch"
                                                   :required nil
                                                   :schema (schema (array :items 'string))
                                                   :style "form"
                                                   :explode nil)))
              '(("id" . #("abc" "opq" "xyz")))))
  (ok (equalp (parse-query-string nil
                                  (list
                                    (make-instance 'parameter
                                                   :name "name"
                                                   :in "query"
                                                   :description "ID of the object to fetch"
                                                   :required nil
                                                   :schema (schema (array :items 'string))
                                                   :style "form"
                                                   :explode nil)))
              '()))
  (ok (signals (parse-query-string "id=abc,opq,xyz" (list))
               'parameter-validation-failed))

  (ok (signals (parse-query-string nil
                                   (list
                                     (make-instance 'parameter
                                                    :name "name"
                                                    :in "query"
                                                    :description "ID of the object to fetch"
                                                    :required t
                                                    :schema (schema (array :items 'string))
                                                    :style "form"
                                                    :expldoe nil)))
               'parameter-validation-failed))

  (testing "nullable parameter"
    (ok (equal '(("foo" . nil))
               (parse-query-string "foo"
                                   (list (make-instance 'parameter
                                                        :name "foo"
                                                        :in "query"
                                                        :schema (schema (string :nullable t)))))))))

(deftest parse-headers-tests
  (ok (equalp (parse-headers
                (alist-hash
                  '(("token" . "123,456")))
                (list (make-instance 'parameter
                                     :name "token"
                                     :in "header"
                                     :description "token to be passed to a header"
                                     :required t
                                     :schema (schema (array :items 'integer))
                                     :style "simple")))
              '(("token" . #(123 456)))))
  (ok (signals (parse-headers
                 (make-hash-table)
                 (list (make-instance 'parameter
                                      :name "token"
                                      :in "header"
                                      :description "token to be passed to a header"
                                      :required t
                                      :schema (schema (array :items 'integer))
                                      :style "simple")))
               'parameter-validation-failed)))

(deftest parse-cookie-parameter
  (ok (equalp (parse-cookie-string
                "id=5"
                (list (make-instance 'parameter
                                     :name "id"
                                     :in "cookie"
                                     :required t
                                     :schema (schema integer)
                                     :style "form")))
              '(("id" . 5))))
  (ok (equalp (parse-cookie-string
                "id=3,4,5"
                (list (make-instance 'parameter
                                     :name "id"
                                     :in "cookie"
                                     :required t
                                     :schema (schema (array :items 'integer))
                                     :style "form"
                                     :explode nil)))
              '(("id" . #(3 4 5)))))
  (ok (equalp (parse-cookie-string
                "id=role,admin,firstName,Alex"
                (list (make-instance 'parameter
                                     :name "id"
                                     :in "cookie"
                                     :required t
                                     :schema (schema (object (("role" string) ("firstName" string))))
                                     :style "form"
                                     :explode nil)))
              '(("id" . (("role" . "admin")
                         ("firstName" . "Alex")))))))
