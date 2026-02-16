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
  (fn* [p1__73567#] (< (Math/abs (- (c/re p1__73567#) 1.0)) 1.0E-10))
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
  signal
  (mapv
   (fn* [p1__73568#] (c/complex (double p1__73568#)))
   temperatures)))


(def v24_l82 (def f-hat (reel/fourier-transform ct signal)))


(def v26_l86 (c/re (f-hat 0)))


(deftest
 t27_l88
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v26_l86)))


(def
 v29_l93
 (def reconstructed (reel/inverse-fourier-transform ct f-hat)))


(def
 v30_l95
 (every?
  (fn* [p1__73569#] (< (Math/abs (double p1__73569#)) 1.0E-10))
  (map - (mapv c/re reconstructed) temperatures)))


(deftest t31_l98 (is (true? v30_l95)))


(def
 v33_l106
 (def
  f
  (mapv
   (fn* [p1__73570#] (c/complex (double p1__73570#)))
   [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])))


(def
 v34_l108
 (def
  h
  (mapv
   (fn* [p1__73571#] (c/complex (double p1__73571#)))
   [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v35_l111 (def convolved (reel/convolve ct f h)))


(def
 v36_l113
 (mapv (fn* [p1__73572#] (Math/round (c/re p1__73572#))) convolved))


(deftest
 t37_l115
 (is (= v36_l113 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))
