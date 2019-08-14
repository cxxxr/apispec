(uiop:define-package #:apispec/tests/classes/schema/core
  (:mix #:apispec/classes/schema/core
        #:cl)
  (:use #:rove))
(in-package #:apispec/tests/classes/schema/core)

(deftest number-tests
  (ok (typep (schema number) 'number))
  (ok (typep (schema cl:number) 'number))
  (ok (typep (schema (cl:number :multiple-of 2)) 'number))
  (ok (typep (schema (cl:number 0)) 'number))
  (ok (typep (schema (cl:number 0 :exclusive-minimum t)) 'number))
  (ok (typep (schema (cl:number 0 100)) 'number))
  (ok (typep (schema (cl:number 0 100 :exclusive-maximum t)) 'number))
  (ok (signals (schema (cl:number :exclusive-minimum t)))
      "Cannot specify :exclusive-minimum without :minimum")
  (testing "nullable"
    (let ((schema (schema (or cl:number null))))
      (ok (typep schema 'number))
      (ok (slot-value schema 'apispec/classes/schema/core::nullable)))
    (let ((schema (schema (or null cl:number))))
      (ok (typep schema 'number))
      (ok (slot-value schema 'apispec/classes/schema/core::nullable)))
    (let ((schema (schema (or (cl:number 0) null))))
      (ok (typep schema 'number))
      (ok (slot-value schema 'apispec/classes/schema/core::nullable)))))

(deftest string-tests
  (ok (typep (schema string) 'string))
  (ok (typep (schema cl:string) 'string))
  (ok (typep (schema (cl:string :pattern "\\d+")) 'string))
  (ok (typep (schema (cl:string 0)) 'string))
  (ok (typep (schema (cl:string 0 :pattern "\\d+")) 'string))
  (ok (typep (schema (cl:string 0 100)) 'string))
  (ok (typep (schema (cl:string 0 100 :pattern "\\d+")) 'string))
  (ok (signals (schema (cl:string -100))))
  (ok (signals (schema (cl:string 1.2)))))

(deftest array-tests
  (ok (typep (schema array) 'array))
  (ok (typep (schema cl:array) 'array))
  (ok (typep (schema (cl:array :unique-items t)) 'array))
  (ok (typep (schema (cl:array 0)) 'array))
  (ok (typep (schema (cl:array 0 :unique-items t)) 'array))
  (ok (typep (schema (cl:array 0 100)) 'array))
  (ok (typep (schema (cl:array 0 100 :unique-items t)) 'array))
  (ok (signals (schema (cl:array -100))))
  (ok (signals (schema (cl:array 1.2)))))

(deftest object-tests
  (ok (typep (schema object) 'object))
  (ok (typep (schema (object)) 'object))
  (ok (typep (schema (object ())) 'object))
  (ok (typep (schema (object (("name" string)))) 'object))
  (ok (typep (schema (object (("name" string)) :required '("name"))) 'object))
  (ok (typep (schema (object
                      (("name" string)
                       ("terminal" (object
                                    (("id" integer)
                                     ("name" string))))))) 'object))
  (testing "nullable"
    (let ((schema (schema (or object null))))
      (ok (typep schema 'object))
      (ok (slot-value schema 'apispec/classes/schema/core::nullable)))
    (let ((schema (schema (or (object) null))))
      (ok (typep schema 'object))
      (ok (slot-value schema 'apispec/classes/schema/core::nullable)))
    (let ((schema (schema (or (object (("name" string))) null))))
      (ok (typep schema 'object))
      (ok (slot-value schema 'apispec/classes/schema/core::nullable)))))
