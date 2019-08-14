(defpackage #:apispec/tests/complex
  (:use #:cl
        #:rove)
  (:import-from #:apispec/complex
                #:parse-matrix-value
                #:parse-label-value
                #:parse-form-value
                #:parse-simple-value
                #:parse-space-delimited-value
                #:parse-pipe-delimited-value
                #:parse-deep-object-value
                #:parse-complex-parameter
                #:parse-complex-parameters)
  (:import-from #:apispec/classes/schema
                #:schema
                #:object
                #:coerce-failed)
  (:import-from #:assoc-utils
                #:alist=))
(in-package #:apispec/tests/complex)

(deftest parse-matrix-value-tests
  (testing ":explode nil"
    (ok (equal (parse-matrix-value ";color")
               '(("color"))))
    (ok (equal (parse-matrix-value ";color=blue" :as (schema string))
               '(("color" . "blue"))))
    (ok (equalp (parse-matrix-value ";color=blue,black,brown" :as (schema array))
                '(("color" . #("blue" "black" "brown")))))
    (ok (equal (parse-matrix-value ";color=R,100,G,200,B,150" :as (schema object))
               '(("color" . (("R" . "100") ("G" . "200") ("B" . "150")))))))
  (testing ":explode t"
    (ok (equal (parse-matrix-value ";color" :explode t)
               '(("color"))))
    (ok (equal (parse-matrix-value ";color=blue" :as (schema string) :explode t)
               '(("color" . "blue"))))
    (ok (equalp (parse-matrix-value ";color=blue;color=black;color=brown"
                                    :as (schema array)
                                    :explode t)
                '(("color" . #("blue" "black" "brown")))))
    (ok (equal (parse-matrix-value ";R=100;G=200;B=150"
                                   :as (schema object)
                                   :explode t)
               '(("R" . "100") ("G" . "200") ("B" . "150"))))))

(deftest parse-label-value-tests
  (testing ":explode nil"
    (ok (equal (parse-label-value ".")
               nil))
    (ok (equal (parse-label-value ".blue" :as (schema string))
               "blue"))
    (ok (equalp (parse-label-value ".blue.black.brown" :as (schema array))
                #("blue" "black" "brown")))
    (ok (equal (parse-label-value ".R.100.G.200.B.150" :as (schema object))
               '(("R" . "100") ("G" . "200") ("B" . "150")))))
  (testing ":explode t"
    (ok (equal (parse-label-value "." :explode t)
               nil))
    (ok (equal (parse-label-value ".blue" :as (schema string) :explode t)
               "blue"))
    (ok (equalp (parse-label-value ".blue.black.brown" :as (schema array) :explode t)
                #("blue" "black" "brown")))
    (ok (equal (parse-label-value ".R=100.G=200.B=150" :as (schema object) :explode t)
               '(("R" . "100") ("G" . "200") ("B" . "150"))))))

(deftest parse-form-value-tests
  (testing ":explode nil"
    (ok (equal (parse-form-value '(("color" . "")) "color")
               ""))
    (ok (equal (parse-form-value '(("color" . "blue")) "color" :as (schema string))
               "blue"))
    (ok (equalp (parse-form-value '(("color" . "blue,black,brown")) "color"
                                  :as (schema array))
                #("blue" "black" "brown")))
    (ok (equal (parse-form-value '(("color" . "R,100,G,200,B,150")) "color"
                                 :as (schema object))
               '(("R" . "100") ("G" . "200") ("B" . "150")))))
  (testing ":explode t"
    (ok (equal (parse-form-value '(("color" . "")) "color" :explode t)
               ""))
    (ok (equal (parse-form-value '(("color" . "blue")) "color" :as (schema string) :explode t)
               "blue"))
    (ok (equalp (parse-form-value '(("color" . "blue")
                                    ("color" . "black")
                                    ("color" . "brown"))
                                  "color"
                                  :as (schema array) :explode t)
                #("blue" "black" "brown")))
    (ok (equal (parse-form-value '(("R" . "100")
                                   ("G" . "200")
                                   ("B" . "150")) "color" :as (schema object) :explode t)
               '(("R" . "100") ("G" . "200") ("B" . "150"))))))

(deftest parse-simple-value-tests
  (testing ":explode nil"
    (ok (equal (parse-simple-value "blue" :as (schema string))
               "blue"))
    (ok (equalp (parse-simple-value "blue,black,brown" :as (schema array))
                #("blue" "black" "brown")))
    (ok (equal (parse-simple-value "R,100,G,200,B,150" :as (schema object))
               '(("R" . "100") ("G" . "200") ("B" . "150")))))
  (testing ":explode t"
    (ok (equal (parse-simple-value "blue" :as (schema string) :explode t)
               "blue"))
    (ok (equalp (parse-simple-value "blue,black,brown" :as (schema array) :explode t)
                #("blue" "black" "brown")))
    (ok (equal (parse-simple-value "R=100,G=200,B=150" :as (schema object) :explode t)
               '(("R" . "100") ("G" . "200") ("B" . "150"))))))

(deftest parse-space-delimited-value-tests
  (ok (equalp (parse-space-delimited-value "blue black brown" :as (schema array))
              #("blue" "black" "brown")))
  (ok (equal (parse-space-delimited-value "R 100 G 200 B 150" :as (schema object))
             '(("R" . "100") ("G" . "200") ("B" . "150")))))

(deftest parse-pipe-delimited-value-tests
  (ok (equalp (parse-pipe-delimited-value "blue|black|brown" :as (schema array))
              #("blue" "black" "brown")))
  (ok (equal (parse-pipe-delimited-value "R|100|G|200" :as (schema object))
             '(("R" . "100") ("G" . "200")))))

(deftest parse-deep-object-value-tests
  (ok (equal (parse-deep-object-value '(("color[R]" . "100")
                                        ("color[G]" . "200")
                                        ("color[B]" . "150"))
                                      "color")
             '(("R" . "100")
               ("G" . "200")
               ("B" . "150")))))

(deftest parse-complex-parameter-tests
  (testing "form"
    (ok (equalp (parse-complex-parameter
                  '(("id" . "10")
                    ("color" . "blue")
                    ("color" . "black")
                    ("color" . "brown"))
                  "color"
                  "form"
                  t
                  (schema
                    (array :items 'string)))
                #("blue" "black" "brown")))
    (ok (equal (parse-complex-parameter
                 '(("id" . "10")
                   ("color" . "blue"))
                 "color"
                 "form"
                 t
                 (schema string))
                "blue"))
    (ok (equalp (parse-complex-parameter
                  '(("id" . "10")
                    ("color" . "blue,black,brown"))
                  "color"
                  "form"
                  nil
                  (schema
                    (array :items 'string)))
                #("blue" "black" "brown"))))
  (testing "spaceDelimited"
    (ok (equalp (parse-complex-parameter
                  '(("id" . "10")
                    ("color" . "blue black brown"))
                  "color"
                  "spaceDelimited"
                  nil
                  (schema
                    (array :items 'string)))
                #("blue" "black" "brown")))
    (ok (equalp (parse-complex-parameter
                  '(("id" . "10")
                    ("color" . "blue"))
                  "color"
                  "spaceDelimited"
                  nil
                  (schema
                    (array :items 'string)))
                #("blue")))
    (ok (equal (parse-complex-parameter
                 '(("id" . "10")
                   ("color" . "blue"))
                 "color"
                 "spaceDelimited"
                 nil
                 (schema string))
               "blue")))
  (testing "pipeDelimited"
    (ok (equalp (parse-complex-parameter
                  '(("id" . "10")
                    ("color" . "blue|black|brown"))
                  "color"
                  "pipeDelimited"
                  nil
                  (schema
                    (array :items 'string)))
                #("blue" "black" "brown")))
    (ok (equalp (parse-complex-parameter
                  '(("id" . "10")
                    ("color" . "blue"))
                  "color"
                  "pipeDelimited"
                  nil
                  (schema
                    (array :items 'string)))
                #("blue")))
    (ok (equal (parse-complex-parameter
                 '(("id" . "10")
                   ("color" . "blue"))
                 "color"
                 "pipeDelimited"
                 nil
                 (schema string))
               "blue")))
  (testing "deepObject"
    (ok (equalp (parse-complex-parameter
                  '(("id" . "10")
                    ("color[R]" . "100")
                    ("color[G]" . "200")
                    ("color[B]" . "150"))
                  "color"
                  "deepObject"
                  nil
                  (schema
                    (object
                      ()
                      :additional-properties 'integer)))
                '(("R" . 100)
                  ("G" . 200)
                  ("B" . 150))))))

(deftest parse-complex-parameters-tests
  (ok (equalp (parse-complex-parameters
                '(("id" . "10")
                  ("color" . "blue")
                  ("color" . "black")
                  ("color" . "brown"))
                "form"
                t
                (schema
                  (object
                    (("id" integer)
                     ("color" (array :items 'string))))))
              '(("id" . 10)
                ("color" . #("blue" "black" "brown")))))
  (ok (equalp (parse-complex-parameters
                '(("id" . "10")
                  ("color" . "blue,black,brown"))
                "form"
                nil
                (schema
                  (object
                    (("id" integer)
                     ("color" (array :items 'string))))))
              '(("id" . 10)
                ("color" . #("blue" "black" "brown")))))
  (ok (equalp (parse-complex-parameters
                '(("id" . "10")
                  ("color" . "blue black brown"))
                "spaceDelimited"
                nil
                (schema
                  (object
                    (("id" integer)
                     ("color" (array :items 'string))))))
              '(("id" . 10)
                ("color" . #("blue" "black" "brown")))))
  (ok (equalp (parse-complex-parameters
                '(("id" . "10")
                  ("color" . "blue|black|brown"))
                "pipeDelimited"
                nil
                (schema
                  (object
                    (("id" integer)
                     ("color" (array :items 'string))))))
              '(("id" . 10)
                ("color" . #("blue" "black" "brown")))))
  (ok (alist= (parse-complex-parameters
                '(("id" . "10")
                  ("color[R]" . "100")
                  ("color[G]" . "150")
                  ("color[B]" . "200"))
                "deepObject"
                nil
                (schema
                  (object
                    (("id" integer)
                     ("color" (object
                                ()
                                :additional-properties 'integer))))))
              '(("id" . 10)
                ("color" . (("R" . 100)
                            ("G" . 150)
                            ("B" . 200)))))))
