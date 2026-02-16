(ns
 reel-book.quickstart-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [fastmath.complex :as c]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l22 (def G (reel/cyclic-group 24)))


(def v4_l24 (reel/order G))


(deftest t5_l26 (is (= v4_l24 24)))


(def v6_l29 (reel/elements G))


(deftest t7_l31 (is (= v6_l29 (range 24))))


(def v9_l36 (reel/op G 15 9))


(deftest t10_l38 (is (= v9_l36 0)))


(def v11_l41 (reel/inv G 15))


(deftest t12_l43 (is (= v11_l41 9)))


(def v14_l51 (def ct (reel/character-table G)))


(def
 v16_l55
 (every?
  (fn* [p1__73329#] (< (Math/abs (- (c/re p1__73329#) 1.0)) 1.0E-10))
  ((:table ct) 0)))


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
  (c/abs (reel/character-inner-product chi-0 chi-1 sizes n))))


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
  (reel/fourier-transform
   ct
   (mapv
    (fn* [p1__73330#] (c/complex (double p1__73330#)))
    temperatures))))


(def v25_l84 (c/re (f-hat 0)))


(deftest
 t26_l86
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v25_l84)))


(def
 v28_l91
 (every?
  (fn* [p1__73331#] (< (Math/abs (double p1__73331#)) 1.0E-10))
  (map
   -
   (mapv c/re (reel/inverse-fourier-transform ct f-hat))
   temperatures)))


(deftest t29_l95 (is (true? v28_l91)))


(def
 v31_l103
 (let
  [f
   (mapv
    (fn* [p1__73332#] (c/complex (double p1__73332#)))
    [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])
   h
   (mapv
    (fn* [p1__73333#] (c/complex (double p1__73333#)))
    [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])]
  (mapv
   (fn* [p1__73334#] (Math/round (c/re p1__73334#)))
   (reel/convolve ct f h))))


(deftest
 t32_l109
 (is (= v31_l103 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))
