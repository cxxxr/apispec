(defpackage #:apispec/tests/classes/encoding
  (:use #:cl
        #:rove
        #:apispec/classes/encoding)
  (:import-from #:apispec/classes/encoding/parse
                #:default-content-type)
  (:import-from #:apispec/classes/schema
                #:schema
                #:object
                #:binary)
  (:import-from #:cl-ppcre)
  (:import-from #:assoc-utils
                #:aget))
(in-package #:apispec/tests/classes/encoding)

(deftest encoding-explode-p-tests
  (flet ((make-encoding (style)
           (make-instance 'encoding :style style)))
    (ng (encoding-explode-p (make-encoding "matrix")))
    (ng (encoding-explode-p (make-encoding "label")))
    (ok (encoding-explode-p (make-encoding "form")))
    (ng (encoding-explode-p (make-encoding "simple")))
    (ng (encoding-explode-p (make-encoding "spaceDelimited")))
    (ng (encoding-explode-p (make-encoding "pipeDelimited")))
    (ng (encoding-explode-p (make-encoding "deepObject")))

    (ok (encoding-explode-p (make-instance 'encoding :style "matrix" :explode t)))))

(deftest default-content-type-tests
  (testing "binary"
    (ok (equal (default-content-type (schema binary))
               "application/octet-stream")))
  (testing "primitive types"
    (ok (equal (default-content-type (schema number))
               "text/plain"))
    (ok (equal (default-content-type (schema string))
               "text/plain"))
    (ok (equal (default-content-type (schema boolean))
               "text/plain")))
  (testing "object"
    (ok (equal (default-content-type (schema object))
               "application/json")))
  (testing "array"
    (ok (equal (default-content-type (schema (array :items 'string)))
               "text/plain"))
    (ok (equal (default-content-type (schema (array :items 'object)))
               "application/json"))))

(deftest parse-with-encoding-tests
  (ok (equal (parse-with-encoding "hello=%E3%81%93%E3%82%93%E3%81%AB%E3%81%A1%E3%81%AF"
                                  (make-instance 'encoding
                                                 :content-type "application/x-www-form-urlencoded")
                                  (schema object)
                                  nil)
             '(("hello" . "こんにちは"))))
  (ok (equalp (parse-with-encoding "language=Lisp%20Scheme%20Clojure"
                                  (make-instance 'encoding
                                                 :content-type "application/x-www-form-urlencoded"
                                                 :style "spaceDelimited")
                                  (schema (object (("language" (array :items 'string)))))
                                  nil)
             '(("language" . #("Lisp" "Scheme" "Clojure")))))
  (ok (equal (parse-with-encoding "{\"hello\":\"こんにちは\"}"
                                  (make-instance 'encoding
                                                 :content-type "application/json")
                                  (schema object)
                                  nil)
             '(("hello" . "こんにちは"))))
  (let ((data (ppcre:regex-replace-all "\\n"
                                       "------------0xKhTmLbOuNdArY
Content-Disposition: form-data; name=\"text1\"

Ratione accusamus aspernatur aliquam
------------0xKhTmLbOuNdArY
Content-Disposition: form-data; name=\"text2\"


------------0xKhTmLbOuNdArY
Content-Disposition: form-data; name=\"upload\"; filename=\"hello.lisp\"
Content-Type: application/octet-stream

#!/usr/bin/env sbcl --script

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (1- n)))))


------------0xKhTmLbOuNdArY--" (format nil "~C~C" #\Return #\Newline))))
    (let ((results (parse-with-encoding
                     data
                     (make-instance 'encoding
                                    :content-type "multipart/form-data; boundary=----------0xKhTmLbOuNdArY")
                     (schema
                       (object
                         (("text1" string)
                          ("text2" string)
                          ("upload" binary))))
                     nil)))
      (ok (equal (aget results "text1")
                 "Ratione accusamus aspernatur aliquam"))
      (ok (equal (aget results "text2")
                 ""))
      (ok (typep (aget results "upload") 'stream)))))
