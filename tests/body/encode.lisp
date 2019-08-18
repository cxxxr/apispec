(defpackage #:apispec/tests/body/encode
  (:use #:cl
        #:rove
        #:apispec/body/encoder)
  (:import-from #:apispec/classes/schema
                #:schema
                #:object))
(in-package #:apispec/tests/body/encode)

(deftest encode-data-json-tests
  (ok (outputs (encode-data 1 'json-encoder (schema integer))
               "1"))
  (ok (outputs (encode-data "a" 'json-encoder (schema string))
               "\"a\""))
  (ok (outputs (encode-data t 'json-encoder (schema boolean))
               "true"))
  (ok (outputs (encode-data nil 'json-encoder (schema boolean))
               "false"))
  (ok (outputs (encode-data nil 'json-encoder (schema integer))
               "null"))
  (ok (outputs (encode-data '(1 2 3) 'json-encoder (schema (array :items 'integer)))
               "[1,2,3]"))
  (ok (outputs (encode-data '(("name" . "Eitaro") ("address" . (("pref" . "Tokyo") ("country" . "Japan"))) ("id" . 1))
                            'json-encoder
                            (schema (object
                                      (("id" integer)
                                       ("name" string)
                                       ("address"
                                        (object (("pref" string)
                                                 ("country" string))))
                                       ("information" (string :nullable t))))))
               "{\"id\":1,\"name\":\"Eitaro\",\"address\":{\"pref\":\"Tokyo\",\"country\":\"Japan\"},\"information\":null\}")))
