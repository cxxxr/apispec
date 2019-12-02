(defpackage #:apispec/tests/file-loader
  (:use #:cl
        #:rove
        #:apispec/file-loader))
(in-package #:apispec/tests/file-loader)

(deftest structural-test
  (let* ((spec (load-from-file (asdf:system-relative-pathname :apispec "./tests/example.yaml")))
         (properties
           (apispec:object-properties
            (apispec:media-type-schema
             (cdr
              (assoc "application/json"
                     (apispec:request-body-content
                      (apispec:operation-request-body
                       (apispec:path-item-post
                        (cdr (assoc "/foo" (apispec/router::router-paths (spec-router spec)) :test #'equal)))))
                     :test #'equal))))))
    (ok (= 1 (length properties)))
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
      (ok (null (apispec:string-pattern type))))))
