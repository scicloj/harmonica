(ns
 harmonica-book.quickstart-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.complex :as cx]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l22 (def G (hm/cyclic-group 24)))


(def v4_l24 (hm/order G))


(deftest t5_l26 (is (= v4_l24 24)))


(def v6_l29 (hm/elements G))


(deftest t7_l31 (is (= v6_l29 (range 24))))


(def v9_l36 (hm/op G 15 9))


(deftest t10_l38 (is (= v9_l36 0)))


(def v11_l41 (hm/inv G 15))


(deftest t12_l43 (is (= v11_l41 9)))


(def v14_l51 (def ct (hm/character-table G)))


(def
 v16_l55
 (every?
  (fn* [p1__90258#] (< (Math/abs (- (cx/re p1__90258#) 1.0)) 1.0E-10))
  (seq ((:table ct) 0))))


(deftest t17_l57 (is (true? v16_l55)))


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
   24]
  (cx/cabs (hm/character-inner-product chi-0 chi-1 sizes n))))


(deftest t20_l68 (is ((fn [v] (< v 1.0E-10)) v19_l62)))


(def
 v22_l76
 (def
  temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3 3 4 8 13 18 23 26 25 20 14 8 4]))


(def
 v23_l80
 (def
  f-hat
  (hm/fourier-transform ct (cx/complex-tensor-real temperatures))))


(def v25_l84 (cx/re (f-hat 0)))


(deftest
 t26_l86
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v25_l84)))


(def
 v28_l91
 (every?
  (fn* [p1__90259#] (< (Math/abs (double p1__90259#)) 1.0E-10))
  (map
   -
   (vec (cx/re (hm/inverse-fourier-transform ct f-hat)))
   temperatures)))


(deftest t29_l95 (is (true? v28_l91)))


(def
 v31_l103
 (let
  [f
   (cx/complex-tensor-real
    [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])
   h
   (cx/complex-tensor-real
    [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])]
  (mapv
   (fn* [p1__90260#] (Math/round p1__90260#))
   (vec (cx/re (hm/convolve ct f h))))))


(deftest
 t32_l109
 (is (= v31_l103 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))
