(ns
 harmonica-book.data-representations-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [scicloj.harmonica.protocols :as p]
  [scicloj.harmonica.combinatorics.permutation :as perm]
  [scicloj.harmonica.combinatorics.partition :as part]
  [scicloj.harmonica.combinatorics.young-tableaux :as yt]
  [scicloj.harmonica.combinatorics.murnaghan-nakayama :as mn]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l45 (def Z6 (hm/cyclic-group 6)))


(def v4_l47 (type Z6))


(deftest
 t5_l49
 (is ((fn [t] (= (.getSimpleName t) "CyclicGroup")) v4_l47)))


(def v7_l53 (select-keys Z6 [:n]))


(deftest t8_l55 (is (= v7_l53 {:n 6})))


(def v9_l57 (hm/elements Z6))


(deftest t10_l59 (is (= v9_l57 (range 6))))


(def v12_l63 [(hm/op Z6 2 3) (hm/inv Z6 2) (hm/id Z6)])


(deftest t13_l67 (is (= v12_l63 [5 4 0])))


(def v15_l72 (count (hm/conjugacy-classes Z6)))


(deftest t16_l74 (is (= v15_l72 6)))


(def v18_l85 (def S4 (hm/symmetric-group 4)))


(def v19_l87 (type S4))


(deftest
 t20_l89
 (is ((fn [t] (= (.getSimpleName t) "SymmetricGroup")) v19_l87)))


(def v21_l91 (hm/order S4))


(deftest t22_l93 (is (= v21_l91 24)))


(def v24_l97 (hm/op S4 [1 0 3 2] [2 3 0 1]))


(deftest t25_l99 (is (= v24_l97 [3 2 1 0])))


(def v27_l103 [(hm/id S4) (hm/inv S4 [1 2 3 0])])


(deftest t28_l106 (is (= v27_l103 [[0 1 2 3] [3 0 1 2]])))


(def v30_l112 (mapv :cycle-type (hm/conjugacy-classes S4)))


(deftest t31_l114 (is (= v30_l112 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v33_l127 (def D4 (hm/dihedral-group 4)))


(def v34_l129 (hm/order D4))


(deftest t35_l131 (is (= v34_l129 8)))


(def v37_l135 (hm/op D4 [:r 1] [:s 0]))


(deftest t38_l137 (is (= v37_l135 [:s 1])))


(def v40_l141 (hm/op D4 [:s 2] [:s 0]))


(deftest t41_l143 (is (= v40_l141 [:r 2])))


(def v43_l147 (hm/inv D4 [:s 3]))


(deftest t44_l149 (is (= v43_l147 [:s 3])))


(def
 v46_l155
 (def Z2xZ3 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3))))


(def v47_l157 (hm/order Z2xZ3))


(deftest t48_l159 (is (= v47_l157 6)))


(def v49_l161 (hm/op Z2xZ3 [1 2] [1 1]))


(deftest t50_l163 (is (= v49_l161 [0 0])))


(def
 v52_l170
 (defn
  group-axioms-hold?
  [G]
  (let
   [e (hm/id G) elts (take 20 (hm/elements G))]
   (and
    (every?
     (fn* [p1__95034#] (= (hm/op G e p1__95034#) p1__95034#))
     elts)
    (every?
     (fn*
      [p1__95035#]
      (= (hm/op G p1__95035# (hm/inv G p1__95035#)) e))
     elts)))))


(def v53_l179 (mapv group-axioms-hold? [Z6 S4 D4 Z2xZ3]))


(deftest t54_l181 (is (= v53_l179 [true true true true])))


(def v56_l190 (let [sigma [2 0 1] tau [1 2 0]] (mapv sigma tau)))


(deftest t57_l194 (is (= v56_l190 [0 1 2])))


(def v59_l199 (perm/inverse [2 0 1]))


(deftest t60_l201 (is (= v59_l199 [1 2 0])))


(def v62_l206 (perm/cycles [1 3 0 2]))


(deftest
 t63_l208
 (is ((fn [cs] (= (set (map set cs)) #{#{0 1 3 2}})) v62_l206)))


(def v65_l213 (perm/cycle-type [1 2 0 3 4]))


(deftest t66_l215 (is (= v65_l213 [3 1 1])))


(def
 v68_l220
 [(perm/sign [0 1 2 3]) (perm/sign [1 0 2 3]) (perm/sign [1 2 0 3])])


(deftest t69_l224 (is (= v68_l220 [1 -1 1])))


(def v71_l232 (hm/partitions 5))


(deftest
 t72_l234
 (is
  ((fn
    [ps]
    (and
     (= 7 (count ps))
     (= [5] (first ps))
     (= [1 1 1 1 1] (last ps))))
   v71_l232)))


(def v74_l241 (part/conjugate [4 2 1]))


(deftest t75_l243 (is (= v74_l241 [3 2 1 1])))


(def v77_l250 (hm/standard-young-tableaux [3 2]))


(deftest t78_l252 (is ((fn [syts] (= 5 (count syts))) v77_l250)))


(def v80_l259 (hm/hook-length-dimension [3 2]))


(deftest t81_l261 (is (= v80_l259 5)))


(def
 v83_l265
 (=
  (count (hm/standard-young-tableaux [4 3 1]))
  (hm/hook-length-dimension [4 3 1])))


(deftest t84_l268 (is (true? v83_l265)))


(def v86_l274 (def ct-s4 (hm/character-table S4)))


(def v87_l276 (sort (keys ct-s4)))


(deftest
 t88_l278
 (is (= v87_l276 [:class-sizes :classes :group :irrep-labels :table])))


(def v90_l282 (:irrep-labels ct-s4))


(deftest t91_l284 (is (= v90_l282 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v93_l289 (:classes ct-s4))


(deftest t94_l291 (is ((fn [cs] (= [1 1 1 1] (first cs))) v93_l289)))


(def v96_l297 (t/complex-shape (:table ct-s4)))


(deftest t97_l299 (is (= v96_l297 [5 5])))


(def v98_l301 (:table ct-s4))


(def v100_l322 (def v (t/complex-tensor [1.0 2.0 3.0] [0.5 -0.5 1.0])))


(def v101_l324 v)


(def v103_l328 (t/shape (t/->tensor v)))


(deftest t104_l330 (is (= v103_l328 [3 2])))


(def v106_l335 [(vec (el/re v)) (vec (el/im v))])


(deftest t107_l337 (is (= v106_l335 [[1.0 2.0 3.0] [0.5 -0.5 1.0]])))


(def
 v109_l350
 (let
  [a (t/complex 3.0 4.0) b (t/complex 1.0 2.0)]
  {:cmul-re (el/re (el/* a b)), :cmul-im (el/im (el/* a b))}))


(deftest
 t110_l355
 (is
  ((fn [m] (and (= -5.0 (:cmul-re m)) (= 10.0 (:cmul-im m))))
   v109_l350)))


(def
 v112_l361
 (let
  [ct-row ((:table ct-s4) 0)]
  {:first-value (el/re (((:table ct-s4) 0) 0)),
   :count (count ((:table ct-s4) 0))}))


(deftest
 t113_l365
 (is
  ((fn [m] (and (= 1.0 (:first-value m)) (= 5 (:count m)))) v112_l361)))


(def v115_l374 (def ir-31 (hm/irrep [3 1])))


(def v116_l376 (sort (keys ir-31)))


(deftest
 t117_l378
 (is (= v116_l376 [:dimension :generators :lambda :syts])))


(def v119_l389 (:lambda ir-31))


(deftest t120_l391 (is (= v119_l389 [3 1])))


(def v121_l393 (:dimension ir-31))


(deftest t122_l395 (is (= v121_l393 3)))


(def v124_l399 (:syts ir-31))


(deftest t125_l401 (is ((fn [syts] (= 3 (count syts))) v124_l399)))


(def v127_l407 (count (:generators ir-31)))


(deftest t128_l409 (is (= v127_l407 3)))


(def v130_l415 (hm/rep-matrix ir-31 [1 0 2 3]))


(def
 v132_l421
 (let
  [rep1
   (hm/irrep [3 1])
   rep2
   (hm/irrep [2 2])
   tp
   (hm/tensor-product rep1 rep2)]
  {:has-matrix-fn (some? (:matrix-fn tp)), :dimension (:dimension tp)}))


(deftest
 t133_l427
 (is
  ((fn [m] (and (:has-matrix-fn m) (= 6 (:dimension m)))) v132_l421)))


(def
 v135_l442
 (let
  [G
   (hm/cyclic-group 8)
   ct
   (hm/character-table G)
   signal
   (t/complex-tensor-real [1 0 1 0 1 0 1 0])]
  (hm/fourier-transform ct signal)))
