(defpackage #:apispec/tests/types/encoding
  (:use #:cl
        #:rove)
  (:import-from #:apispec/types/encoding/parse
                #:parse-matrix-value
                #:parse-label-value
                #:parse-form-value
                #:parse-simple-value
                #:parse-space-delimited-value
                #:parse-pipe-delimited-value
                #:parse-deep-object-value)
  (:import-from #:apispec/types/schema
                #:schema
                #:object))
(in-package #:apispec/tests/types/encoding)

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
  (ok (equalp (parse-space-delimited-value "blue%20black%20brown" :as (schema array))
              #("blue" "black" "brown")))
  (ok (equal (parse-space-delimited-value "R%20100%20G%20200%20B%20150" :as (schema object))
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
