(uiop:define-package #:apispec/tests/classes/schema/encode
    (:mix #:apispec/classes/schema/core
          #:cl)
  (:use #:apispec/classes/schema/encode
        #:rove))
(in-package #:apispec/tests/classes/schema/encode)

(deftest encode-object-tests
  (ok (equal
        (encode-data '(("name" . "fukamachi")
                       ("age" . nil)
                       ("is_merchant" . nil)
                       ("terminal" . (("id" . "xxx") ("keys" . ()))))
                     '(object (("name" string)
                               ("age" (integer :nullable t))
                               ("is_merchant" boolean)
                               ("terminal"
                                (object (("id" string) ("name" string) ("keys" array)))))))
       "{\"name\":\"fukamachi\",\"age\":null,\"is_merchant\":false,\"terminal\":{\"id\":\"xxx\",\"keys\":[]}}")))
