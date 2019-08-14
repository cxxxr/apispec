(defsystem "apispec"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :description "API request / response validations"
  :depends-on ("apispec/main")
  :pathname "src"
  :in-order-to ((test-op (test-op "apispec/tests"))))

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
       (:file "encode")))
     (:file "header")
     (:file "encoding")
     (:file "parameter")
     (:file "media-type")
     (:file "complex")))
   (:file "utils"))
  :perform (test-op (o c) (symbol-call :rove '#:run c :style :dot)))
