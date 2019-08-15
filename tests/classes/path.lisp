(defpackage #:apispec/tests/classes/path
  (:use #:cl
        #:rove
        #:apispec/classes/path)
  (:import-from #:apispec/classes/operation
                #:operation)
  (:import-from #:apispec/classes/response
                #:response))
(in-package #:apispec/tests/classes/path)

(deftest find-operation-tests
  (let* ((get-operation (make-instance 'operation
                                       :responses `((200 . ,(make-instance 'response
                                                                           :description "Success")))))
         (path (make-instance 'path-item
                              :get get-operation)))
    (ok (eq (find-operation path :get) get-operation))
    (ng (find-operation path :post))
    (ok (signals (find-operation :path :connect)))))

(deftest compile-paths-tests
  (let ((pets (make-instance 'path-item))
        (pet (make-instance 'path-item))
        (pet-xml (make-instance 'path-item))
        (pet-json (make-instance 'path-item))
        (my-pet (make-instance 'path-item)))
    (let ((compiled (compile-paths
                      `(("/pets" . ,pets)
                        ("/pets/{petId}.xml" . ,pet-xml)
                        ("/pets/{petId}" . ,pet)
                        ("/pets/{petId}.json" . ,pet-json)
                        ("/pets/mine" . ,my-pet)))))
      (ok (typep compiled 'function))
      (ok (equalp (multiple-value-list (funcall compiled "/pets"))
                  (list pets '())))
      (ok (equalp (multiple-value-list (funcall compiled "/pets/1"))
                  (list pet '(("petId" . "1")))))
      (ok (equalp (multiple-value-list (funcall compiled "/pets/1.xml"))
                  (list pet-xml '(("petId" . "1")))))
      (ok (equalp (multiple-value-list (funcall compiled "/pets/1.json"))
                  (list pet-json '(("petId" . "1")))))
      (ok (equalp (multiple-value-list (funcall compiled "/pets/mine"))
                  (list my-pet '())))
      (ng (funcall compiled "/report")))))
