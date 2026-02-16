(ns
 reel-book.quickstart-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [fastmath.complex :as c]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l22 (def Z8 (reel/cyclic-group 8)))


(def v4_l24 (reel/order Z8))


(deftest t5_l26 (is (= v4_l24 8)))


(def v6_l29 (reel/elements Z8))


(deftest t7_l31 (is (= v6_l29 (range 8))))


(def v9_l36 (reel/op Z8 3 5))


(deftest t10_l38 (is (= v9_l36 0)))


(def v11_l41 (reel/inv Z8 3))


(deftest t12_l43 (is (= v11_l41 5)))


(def v14_l51 (def ct (reel/character-table Z8)))


(def v16_l55 (mapv c/re (first (:table ct))))


(deftest t17_l57 (is (= v16_l55 [1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0])))


(def
 v19_l62
 (let
  [chi-0
   ((:table ct) 0)
   chi-1
   ((:table ct) 1)
   sizes
   (:class-sizes ct)
   n
   8]
  (c/abs (reel/character-inner-product chi-0 chi-1 sizes n))))


(deftest t20_l68 (is ((fn [v] (< v 1.0E-10)) v19_l62)))


(def
 v22_l75
 (def
  signal
  (mapv
   (fn* [p1__72868#] (c/complex (double p1__72868#)))
   [20 22 25 23 21 19 18 20])))


(def v23_l77 (def f-hat (reel/fourier-transform ct signal)))


(def v25_l81 (c/re (f-hat 0)))


(deftest
 t26_l83
 (is ((fn [v] (< (Math/abs (- v 168.0)) 1.0E-10)) v25_l81)))


(def
 v28_l88
 (def reconstructed (reel/inverse-fourier-transform ct f-hat)))


(def v29_l90 (mapv c/re reconstructed))


(deftest
 t30_l92
 (is
  ((fn
    [vs]
    (every?
     (fn* [p1__72869#] (< (Math/abs (double p1__72869#)) 1.0E-10))
     (map - vs [20 22 25 23 21 19 18 20])))
   v29_l90)))


(def
 v32_l101
 (def
  f
  (mapv
   (fn* [p1__72870#] (c/complex (double p1__72870#)))
   [1 2 0 0 0 0 0 3])))


(def
 v33_l102
 (def
  h
  (mapv
   (fn* [p1__72871#] (c/complex (double p1__72871#)))
   [0 1 1 0 0 0 0 0])))


(def v34_l104 (def convolved (reel/convolve ct f h)))


(def
 v35_l106
 (mapv (fn* [p1__72872#] (Math/round (c/re p1__72872#))) convolved))


(deftest t36_l108 (is (= v35_l106 [3 4 3 2 0 0 0 0])))
