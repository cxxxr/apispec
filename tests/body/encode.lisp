(defpackage #:apispec/tests/body/encode
  (:use #:cl
        #:rove
        #:apispec/body/encoder)
  (:import-from #:apispec/classes/schema
                #:schema
                #:object))
(in-package #:apispec/tests/body/encode)

(deftest encode-data-json-tests
  (ok (equal (encode-data 1 (schema integer)
                          "application/json")
             "1"))
  (ok (equal (encode-data "a" (schema string)
                            "application/json")
               "\"a\""))
  (ok (equal (encode-data t (schema boolean)
                            "application/json")
               "true"))
  (ok (equal (encode-data nil (schema boolean)
                            "application/json")
               "false"))
  (ok (equal (encode-data nil (schema (integer :nullable t))
                            "application/json")
               "null"))
  (ok (equal (encode-data '(1 2 3) (schema (array :items 'integer))
                            "application/json")
               "[1,2,3]"))
  (ok (equal
        (encode-data '(("name" . "fukamachi")
                       ("age" . nil)
                       ("is_merchant" . nil)
                       ("terminal" . (("id" . "xxx") ("keys" . ()))))
                     (schema (object
                               (("name" string)
                                ("age" (integer :nullable t))
                                ("is_merchant" boolean)
                                ("terminal"
                                 (object (("id" string) ("name" (string :nullable t)) ("keys" array)))))))
                     "application/json")
       "{\"name\":\"fukamachi\",\"age\":null,\"is_merchant\":false,\"terminal\":{\"id\":\"xxx\",\"name\":null,\"keys\":[]}}")))
