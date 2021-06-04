(defsystem "apispec"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :description "API request / response validations"
  :depends-on ("openapi-parser"
               "apispec/main")
  :pathname "src"
  :in-order-to ((test-op (test-op "apispec/tests"))))

(register-system-packages "lack-request" '(#:lack.request))
(register-system-packages "lack-response" '(#:lack.response))
(asdf:register-system-packages "openapi-parser" '(#:openapi-parser #:openapi-parser/schema))

(defsystem "apispec/tests"
  :depends-on ("apispec"
               "cl-interpol"
               "assoc-utils"
               "rove")
  :pathname "tests"
  :components
  ((:module "classes"
    :components
    ((:module "schema"
      :components
      ((:file "core")
       (:file "coerce")
       (:file "validate")))
     (:file "header")
     (:file "encoding")
     (:file "parameter")
     (:file "request-body")
     (:file "media-type")
     (:file "response")
     (:file "path")
     (:file "operation")))
   (:file "complex")
   (:module "body"
    :components
    ((:file "encode")))
   (:file "router")
   (:file "file-loader")
   (:file "utils"))
  :perform (test-op (o c) (symbol-call :rove '#:run c :style :dot)))
