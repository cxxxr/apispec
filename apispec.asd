(defsystem "apispec"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :description "API request / response validations"
  :depends-on ("apispec/main")
  :pathname "src"
  :in-order-to ((test-op (test-op "apispec/tests"))))

(register-system-packages "http-body" '(#:http-body #:http-body.util))

(defsystem "apispec/tests"
  :depends-on ("apispec"
               "assoc-utils"
               "rove")
  :pathname "tests"
  :components
  ((:module "schema"
    :components
    ((:file "core")
     (:file "coerce")
     (:file "encode")))
   (:module "request"
    :components
    ((:file "encoding")))
   (:file "utils"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
