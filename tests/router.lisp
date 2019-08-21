(defpackage #:apispec/tests/router
  (:use #:cl
        #:rove
        #:apispec/router)
  (:import-from #:apispec/classes/operation
                #:operation)
  (:import-from #:apispec/classes/path
                #:path-item))
(in-package #:apispec/tests/router)

(deftest find-route-tests
  (let* ((get-pet-operation (make-instance 'operation
                                           :responses '(("200" . nil))))
         (get-my-pet-operation (make-instance 'operation
                                              :responses '(("200" . nil))))
         (specified-pet (make-instance 'path-item
                                       :get get-pet-operation))
         (my-pet (make-instance 'path-item
                                :get get-my-pet-operation))
         (paths `(("/pets/{petId}" . ,specified-pet)
                  ("/pets/me" . ,my-pet))))
    (ok (eq (find-route paths :get "/pets/me")
            get-my-pet-operation))
    (ok (eq (find-route paths :get "/pets/14")
            get-pet-operation))
    (ok (null (find-route paths :post "/pets/me")))
    (ok (null (find-route paths :get "/pets")))))
