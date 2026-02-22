(ns
 harmonica-book.data-representations-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [scicloj.harmonica.protocols :as p]
  [scicloj.harmonica.combinatorics.permutation :as perm]
  [scicloj.harmonica.combinatorics.partition :as part]
  [scicloj.harmonica.combinatorics.young-tableaux :as yt]
  [scicloj.harmonica.combinatorics.murnaghan-nakayama :as mn]
  [tech.v3.tensor :as tensor]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l47 (def Z6 (hm/cyclic-group 6)))


(def v4_l49 (type Z6))


(deftest
 t5_l51
 (is ((fn [t] (= (.getSimpleName t) "CyclicGroup")) v4_l49)))


(def v7_l55 (select-keys Z6 [:n]))


(deftest t8_l57 (is (= v7_l55 {:n 6})))


(def v9_l59 (hm/elements Z6))


(deftest t10_l61 (is (= v9_l59 (range 6))))


(def v12_l65 [(hm/op Z6 2 3) (hm/inv Z6 2) (hm/id Z6)])


(deftest t13_l69 (is (= v12_l65 [5 4 0])))


(def v15_l74 (count (hm/conjugacy-classes Z6)))


(deftest t16_l76 (is (= v15_l74 6)))


(def v18_l87 (def S4 (hm/symmetric-group 4)))


(def v19_l89 (type S4))


(deftest
 t20_l91
 (is ((fn [t] (= (.getSimpleName t) "SymmetricGroup")) v19_l89)))


(def v21_l93 (hm/order S4))


(deftest t22_l95 (is (= v21_l93 24)))


(def v24_l99 (hm/op S4 [1 0 3 2] [2 3 0 1]))


(deftest t25_l101 (is (= v24_l99 [3 2 1 0])))


(def v27_l105 [(hm/id S4) (hm/inv S4 [1 2 3 0])])


(deftest t28_l108 (is (= v27_l105 [[0 1 2 3] [3 0 1 2]])))


(def v30_l114 (mapv :cycle-type (hm/conjugacy-classes S4)))


(deftest t31_l116 (is (= v30_l114 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v33_l129 (def D4 (hm/dihedral-group 4)))


(def v34_l131 (hm/order D4))


(deftest t35_l133 (is (= v34_l131 8)))


(def v37_l137 (hm/op D4 [:r 1] [:s 0]))


(deftest t38_l139 (is (= v37_l137 [:s 1])))


(def v40_l143 (hm/op D4 [:s 2] [:s 0]))


(deftest t41_l145 (is (= v40_l143 [:r 2])))


(def v43_l149 (hm/inv D4 [:s 3]))


(deftest t44_l151 (is (= v43_l149 [:s 3])))


(def
 v46_l157
 (def Z2xZ3 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3))))


(def v47_l159 (hm/order Z2xZ3))


(deftest t48_l161 (is (= v47_l159 6)))


(def v49_l163 (hm/op Z2xZ3 [1 2] [1 1]))


(deftest t50_l165 (is (= v49_l163 [0 0])))


(def
 v52_l172
 (defn
  group-axioms-hold?
  [G]
  (let
   [e (hm/id G) elts (take 20 (hm/elements G))]
   (and
    (every?
     (fn* [p1__75829#] (= (hm/op G e p1__75829#) p1__75829#))
     elts)
    (every?
     (fn*
      [p1__75830#]
      (= (hm/op G p1__75830# (hm/inv G p1__75830#)) e))
     elts)))))


(def v53_l181 (mapv group-axioms-hold? [Z6 S4 D4 Z2xZ3]))


(deftest t54_l183 (is (= v53_l181 [true true true true])))


(def v56_l192 (let [sigma [2 0 1] tau [1 2 0]] (mapv sigma tau)))


(deftest t57_l196 (is (= v56_l192 [0 1 2])))


(def v59_l201 (perm/inverse [2 0 1]))


(deftest t60_l203 (is (= v59_l201 [1 2 0])))


(def v62_l208 (perm/cycles [1 3 0 2]))


(deftest
 t63_l210
 (is ((fn [cs] (= (set (map set cs)) #{#{0 1 3 2}})) v62_l208)))


(def v65_l215 (perm/cycle-type [1 2 0 3 4]))


(deftest t66_l217 (is (= v65_l215 [3 1 1])))


(def
 v68_l222
 [(perm/sign [0 1 2 3]) (perm/sign [1 0 2 3]) (perm/sign [1 2 0 3])])


(deftest t69_l226 (is (= v68_l222 [1 -1 1])))


(def v71_l234 (hm/partitions 5))


(deftest
 t72_l236
 (is
  ((fn
    [ps]
    (and
     (= 7 (count ps))
     (= [5] (first ps))
     (= [1 1 1 1 1] (last ps))))
   v71_l234)))


(def v74_l243 (part/conjugate [4 2 1]))


(deftest t75_l245 (is (= v74_l243 [3 2 1 1])))


(def v77_l252 (hm/standard-young-tableaux [3 2]))


(deftest t78_l254 (is ((fn [syts] (= 5 (count syts))) v77_l252)))


(def v80_l261 (hm/hook-length-dimension [3 2]))


(deftest t81_l263 (is (= v80_l261 5)))


(def
 v83_l267
 (=
  (count (hm/standard-young-tableaux [4 3 1]))
  (hm/hook-length-dimension [4 3 1])))


(deftest t84_l270 (is (true? v83_l267)))


(def v86_l276 (def ct-s4 (hm/character-table S4)))


(def v87_l278 (sort (keys ct-s4)))


(deftest
 t88_l280
 (is (= v87_l278 [:class-sizes :classes :group :irrep-labels :table])))


(def v90_l284 (:irrep-labels ct-s4))


(deftest t91_l286 (is (= v90_l284 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v93_l291 (:classes ct-s4))


(deftest t94_l293 (is ((fn [cs] (= [1 1 1 1] (first cs))) v93_l291)))


(def v96_l299 (cx/complex-shape (:table ct-s4)))


(deftest t97_l301 (is (= v96_l299 [5 5])))


(def v98_l303 (:table ct-s4))


(def v100_l324 (def v (cx/complex-tensor [1.0 2.0 3.0] [0.5 -0.5 1.0])))


(def v101_l326 v)


(def v103_l330 (dtype/shape (cx/->tensor v)))


(deftest t104_l332 (is (= v103_l330 [3 2])))


(def v106_l337 [(vec (cx/re v)) (vec (cx/im v))])


(deftest t107_l339 (is (= v106_l337 [[1.0 2.0 3.0] [0.5 -0.5 1.0]])))


(def
 v109_l355
 (let
  [a (cx/complex 3.0 4.0) b (cx/complex 1.0 2.0)]
  {:cmul-re (cx/re (cx/cmul a b)), :cmul-im (cx/im (cx/cmul a b))}))


(deftest
 t110_l360
 (is
  ((fn [m] (and (= -5.0 (:cmul-re m)) (= 10.0 (:cmul-im m))))
   v109_l355)))


(def
 v112_l366
 (let
  [ct-row ((:table ct-s4) 0)]
  {:first-value (cx/re (((:table ct-s4) 0) 0)),
   :count (count ((:table ct-s4) 0))}))


(deftest
 t113_l370
 (is
  ((fn [m] (and (= 1.0 (:first-value m)) (= 5 (:count m)))) v112_l366)))


(def v115_l379 (def ir-31 (hm/irrep [3 1])))


(def v116_l381 (sort (keys ir-31)))


(deftest
 t117_l383
 (is (= v116_l381 [:dimension :generators :lambda :syts])))


(def v119_l394 (:lambda ir-31))


(deftest t120_l396 (is (= v119_l394 [3 1])))


(def v121_l398 (:dimension ir-31))


(deftest t122_l400 (is (= v121_l398 3)))


(def v124_l404 (:syts ir-31))


(deftest t125_l406 (is ((fn [syts] (= 3 (count syts))) v124_l404)))


(def v127_l412 (count (:generators ir-31)))


(deftest t128_l414 (is (= v127_l412 3)))


(def v130_l420 (hm/rep-matrix ir-31 [1 0 2 3]))


(def
 v132_l426
 (let
  [rep1
   (hm/irrep [3 1])
   rep2
   (hm/irrep [2 2])
   tp
   (hm/tensor-product rep1 rep2)]
  {:has-matrix-fn (some? (:matrix-fn tp)), :dimension (:dimension tp)}))


(deftest
 t133_l432
 (is
  ((fn [m] (and (:has-matrix-fn m) (= 6 (:dimension m)))) v132_l426)))


(def
 v135_l451
 (let
  [G
   (hm/cyclic-group 8)
   ct
   (hm/character-table G)
   signal
   (cx/complex-tensor-real [1 0 1 0 1 0 1 0])]
  (hm/fourier-transform ct signal)))
