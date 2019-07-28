(defpackage #:apispec/tests/schema/encode
  (:use #:cl
        #:apispec/schema/encode
        #:apispec/schema/core
        #:rove))
(in-package #:apispec/tests/schema/encode)

(deftest encode-object-tests
  (ok (equal
       (jojo:with-output-to-string*
         (encode-data '(("name" . "fukamachi")
                        ("age" . nil)
                        ("is_merchant" . nil)
                        ("terminal" . (("id" . "xxx") ("keys" . ()))))
                      '(object (("name" string)
                                ("age" (integer :nullable t))
                                ("is_merchant" boolean)
                                ("terminal"
                                 (object (("id" string) ("name" string) ("keys" array))))))))
       "{\"name\":\"fukamachi\",\"age\":null,\"is_merchant\":false,\"terminal\":{\"id\":\"xxx\",\"keys\":[]}}")))
