(defpackage #:apispec/tests/classes/operation
  (:use #:cl
        #:rove
        #:apispec/classes/operation)
  (:import-from #:apispec/classes/schema
                #:schema)
  (:import-from #:apispec/classes/parameter
                #:parameter)
  (:import-from #:apispec/classes/response
                #:response)
  (:import-from #:assoc-utils
                #:alist-hash))
(in-package #:apispec/tests/classes/operation)

(defun make-operation (parameters)
  (make-instance 'operation
                 :parameters parameters
                 :responses
                 `((200 . ,(make-instance 'response
                                          :description "Success")))))

(deftest validate-request-tests
  (testing "path"
    (let ((operation (make-operation
                       (list
                         (make-instance 'parameter
                                        :name "car_id"
                                        :in "path"
                                        :schema (schema integer))
                         (make-instance 'parameter
                                        :name "driver_id"
                                        :in "path"
                                        :schema (schema string))))))
      (ok (equalp (validate-request operation
                                    ()
                                    :path-parameters '(("car_id" . "1")
                                                       ("driver_id" . "xyz")))
                  '(("car_id" . 1)
                    ("driver_id" . "xyz"))))))
  (testing "query"
    (let ((operation (make-operation
                       (list
                         (make-instance 'parameter
                                        :name "role"
                                        :in "query"
                                        :schema (schema string))))))
      (ok (equalp (validate-request operation
                                    '(:query-string "role=admin"))
                  '(("role" . "admin"))))))
  (testing "header"
    (let ((operation (make-operation
                       (list
                         (make-instance 'parameter
                                        :name "X-App-Version"
                                        :in "header"
                                        :schema (schema integer))))))
      (ok (equalp (validate-request operation
                                    (list
                                      :headers (alist-hash
                                                 `(("x-app-version" . "3")))))
                  '(("X-App-Version" . 3))))))
  (testing "cookie"
    (let ((operation (make-operation
                       (list
                         (make-instance 'parameter
                                        :name "debug"
                                        :in "cookie"
                                        :schema (schema integer))
                         (make-instance 'parameter
                                        :name "csrftoken"
                                        :in "cookie"
                                        :schema (schema string))))))
      (ok (equalp (validate-request operation
                                    (list
                                      :headers (alist-hash
                                                 `(("cookie" . "debug=0; csrftoken=BUSe35dohU3O1MZvDCU")))))
                  '(("debug" . 0)
                    ("csrftoken" . "BUSe35dohU3O1MZvDCU")))))))
