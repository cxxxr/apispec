(uiop:define-package #:apispec/tests/schema/encode
    (:mix #:apispec/schema/core
          #:cl)
  (:use #:apispec/schema/encode
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
