(defpackage #:apispec/tests/file-loader
  (:use #:cl
        #:rove
        #:apispec/file-loader)
  (:import-from #:apispec/classes/schema
                #:json))
(in-package #:apispec/tests/file-loader)

(defvar *spec*)

(setup
  (setf *spec* (load-from-file (asdf:system-relative-pathname :apispec "./tests/example.yaml"))))

(defun get-operation-properties (method path-info)
  (apispec:object-properties
   (apispec:media-type-schema
    (cdr
     (assoc "application/json"
            (apispec:request-body-content
             (apispec:operation-request-body
              (apispec:find-route (spec-router *spec*)
                                  method path-info)))
            :test #'equal)))))

(deftest structural-test
  (let ((properties
          (get-operation-properties :post "/foo")))
    (ok (= 2 (length properties)))
    (testing "first property"
      (let* ((property (first properties))
             (name (apispec:property-name property))
             (type (apispec:property-type property)))
        (ok (equal "foo" name))
        (ok (equal (apispec:schema-type type) "string"))
        (ok (null (apispec:schema-format type)))
        (ok (null (apispec:schema-enum type)))
        (ok (not (apispec:schema-has-default-p type)))
        (ok (not (apispec:schema-nullable-p type)))
        (ok (not (apispec:schema-deprecated-p type)))
        (ok (null (apispec:string-max-length type)))
        (ok (null (apispec:string-min-length type)))
        (ok (null (apispec:string-pattern type)))))
    (testing "second property"
      (let* ((property (second properties))
             (name (apispec:property-name property))
             (type (apispec:property-type property)))
        (ok (equal "json_string" name))
        (ok (equal (apispec:schema-type type) "string"))
        (ok (equal "json" (apispec:schema-format type)))
        (ok (null (apispec:schema-enum type)))
        (ok (not (apispec:schema-has-default-p type)))
        (ok (not (apispec:schema-nullable-p type)))
        (ok (not (apispec:schema-deprecated-p type)))
        (ok (null (apispec:string-max-length type)))
        (ok (null (apispec:string-min-length type)))
        (ok (null (apispec:string-pattern type)))
        (ok (typep type 'json))))))

(deftest parameters-test
  (let* ((spec *spec*)
         (parameters
           (apispec:operation-parameters
            (apispec:path-item-get
             (cdr (assoc "/foo/{id}" (apispec/router::router-paths (spec-router spec)) :test #'equal))))))
    (ok (= 2 (length parameters)))
    (let ((parameter (first parameters)))
      (ok (null (apispec:parameter-allow-reserved-p parameter)))
      (ok (equal "path" (apispec:parameter-in parameter)))
      (ok (equal "id" (apispec:parameter-name parameter)))
      (ok (eq t (apispec:parameter-required-p parameter)))
      (ok (null (apispec:parameter-schema parameter))))
    (let ((parameter (second parameters)))
      (ok (null (apispec:parameter-allow-reserved-p parameter)))
      (ok (equal "query" (apispec:parameter-in parameter)))
      (ok (equal "a" (apispec:parameter-name parameter)))
      (ok (eq nil (apispec:parameter-required-p parameter)))
      (ok (typep (apispec:parameter-schema parameter) 'apispec/classes/schema:string)))))

(deftest query-test
  (let* ((spec *spec*)
         (parameters
           (apispec:operation-parameters
            (apispec:find-route
             (spec-router spec) :get "/bar"))))
    (ok (= 2 (length parameters)))
    (flet ((test (parameter expected-name expected-default)
             (ok (equal "query" (apispec:parameter-in parameter)))
             (ok (equal expected-name (apispec:parameter-name parameter)))
             (ok (not (apispec:parameter-required-p parameter)))
             (ok (eq t (apispec:parameter-explode-p parameter)))
             (let ((schema (apispec:parameter-schema parameter)))
               (ok (equal "boolean" (apispec:schema-type schema)))
               (ok (eq expected-default (apispec:schema-default schema))))))
      (testing "q1"
        (test (first parameters) "q1" nil))
      (testing "q2"
        (test (second parameters) "q2" t)))))
