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
  [tech.v3.tensor :as tensor]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l48 (def Z6 (hm/cyclic-group 6)))


(def v4_l50 (type Z6))


(deftest
 t5_l52
 (is ((fn [t] (= (.getSimpleName t) "CyclicGroup")) v4_l50)))


(def v7_l56 (select-keys Z6 [:n]))


(deftest t8_l58 (is (= v7_l56 {:n 6})))


(def v9_l60 (hm/elements Z6))


(deftest t10_l62 (is (= v9_l60 (range 6))))


(def v12_l66 [(hm/op Z6 2 3) (hm/inv Z6 2) (hm/id Z6)])


(deftest t13_l70 (is (= v12_l66 [5 4 0])))


(def v15_l75 (count (hm/conjugacy-classes Z6)))


(deftest t16_l77 (is (= v15_l75 6)))


(def v18_l88 (def S4 (hm/symmetric-group 4)))


(def v19_l90 (type S4))


(deftest
 t20_l92
 (is ((fn [t] (= (.getSimpleName t) "SymmetricGroup")) v19_l90)))


(def v21_l94 (hm/order S4))


(deftest t22_l96 (is (= v21_l94 24)))


(def v24_l100 (hm/op S4 [1 0 3 2] [2 3 0 1]))


(deftest t25_l102 (is (= v24_l100 [3 2 1 0])))


(def v27_l106 [(hm/id S4) (hm/inv S4 [1 2 3 0])])


(deftest t28_l109 (is (= v27_l106 [[0 1 2 3] [3 0 1 2]])))


(def v30_l115 (mapv :cycle-type (hm/conjugacy-classes S4)))


(deftest t31_l117 (is (= v30_l115 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v33_l130 (def D4 (hm/dihedral-group 4)))


(def v34_l132 (hm/order D4))


(deftest t35_l134 (is (= v34_l132 8)))


(def v37_l138 (hm/op D4 [:r 1] [:s 0]))


(deftest t38_l140 (is (= v37_l138 [:s 1])))


(def v40_l144 (hm/op D4 [:s 2] [:s 0]))


(deftest t41_l146 (is (= v40_l144 [:r 2])))


(def v43_l150 (hm/inv D4 [:s 3]))


(deftest t44_l152 (is (= v43_l150 [:s 3])))


(def
 v46_l158
 (def Z2xZ3 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3))))


(def v47_l160 (hm/order Z2xZ3))


(deftest t48_l162 (is (= v47_l160 6)))


(def v49_l164 (hm/op Z2xZ3 [1 2] [1 1]))


(deftest t50_l166 (is (= v49_l164 [0 0])))


(def
 v52_l173
 (defn
  group-axioms-hold?
  [G]
  (let
   [e (hm/id G) elts (take 20 (hm/elements G))]
   (and
    (every?
     (fn* [p1__68225#] (= (hm/op G e p1__68225#) p1__68225#))
     elts)
    (every?
     (fn*
      [p1__68226#]
      (= (hm/op G p1__68226# (hm/inv G p1__68226#)) e))
     elts)))))


(def v53_l182 (mapv group-axioms-hold? [Z6 S4 D4 Z2xZ3]))


(deftest t54_l184 (is (= v53_l182 [true true true true])))


(def v56_l193 (let [sigma [2 0 1] tau [1 2 0]] (mapv sigma tau)))


(deftest t57_l197 (is (= v56_l193 [0 1 2])))


(def v59_l202 (perm/inverse [2 0 1]))


(deftest t60_l204 (is (= v59_l202 [1 2 0])))


(def v62_l209 (perm/cycles [1 3 0 2]))


(deftest
 t63_l211
 (is ((fn [cs] (= (set (map set cs)) #{#{0 1 3 2}})) v62_l209)))


(def v65_l216 (perm/cycle-type [1 2 0 3 4]))


(deftest t66_l218 (is (= v65_l216 [3 1 1])))


(def
 v68_l223
 [(perm/sign [0 1 2 3]) (perm/sign [1 0 2 3]) (perm/sign [1 2 0 3])])


(deftest t69_l227 (is (= v68_l223 [1 -1 1])))


(def v71_l235 (hm/partitions 5))


(deftest
 t72_l237
 (is
  ((fn
    [ps]
    (and
     (= 7 (count ps))
     (= [5] (first ps))
     (= [1 1 1 1 1] (last ps))))
   v71_l235)))


(def v74_l244 (part/conjugate [4 2 1]))


(deftest t75_l246 (is (= v74_l244 [3 2 1 1])))


(def v77_l253 (hm/standard-young-tableaux [3 2]))


(deftest t78_l255 (is ((fn [syts] (= 5 (count syts))) v77_l253)))


(def v80_l262 (hm/hook-length-dimension [3 2]))


(deftest t81_l264 (is (= v80_l262 5)))


(def
 v83_l268
 (=
  (count (hm/standard-young-tableaux [4 3 1]))
  (hm/hook-length-dimension [4 3 1])))


(deftest t84_l271 (is (true? v83_l268)))


(def v86_l277 (def ct-s4 (hm/character-table S4)))


(def v87_l279 (sort (keys ct-s4)))


(deftest
 t88_l281
 (is (= v87_l279 [:class-sizes :classes :group :irrep-labels :table])))


(def v90_l285 (:irrep-labels ct-s4))


(deftest t91_l287 (is (= v90_l285 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v93_l292 (:classes ct-s4))


(deftest t94_l294 (is ((fn [cs] (= [1 1 1 1] (first cs))) v93_l292)))


(def v96_l300 (t/complex-shape (:table ct-s4)))


(deftest t97_l302 (is (= v96_l300 [5 5])))


(def v98_l304 (:table ct-s4))


(def v100_l325 (def v (t/complex-tensor [1.0 2.0 3.0] [0.5 -0.5 1.0])))


(def v101_l327 v)


(def v103_l331 (dtype/shape (t/->tensor v)))


(deftest t104_l333 (is (= v103_l331 [3 2])))


(def v106_l338 [(vec (el/re v)) (vec (el/im v))])


(deftest t107_l340 (is (= v106_l338 [[1.0 2.0 3.0] [0.5 -0.5 1.0]])))


(def
 v109_l356
 (let
  [a (t/complex 3.0 4.0) b (t/complex 1.0 2.0)]
  {:cmul-re (el/re (el/* a b)), :cmul-im (el/im (el/* a b))}))


(deftest
 t110_l361
 (is
  ((fn [m] (and (= -5.0 (:cmul-re m)) (= 10.0 (:cmul-im m))))
   v109_l356)))


(def
 v112_l367
 (let
  [ct-row ((:table ct-s4) 0)]
  {:first-value (el/re (((:table ct-s4) 0) 0)),
   :count (count ((:table ct-s4) 0))}))


(deftest
 t113_l371
 (is
  ((fn [m] (and (= 1.0 (:first-value m)) (= 5 (:count m)))) v112_l367)))


(def v115_l380 (def ir-31 (hm/irrep [3 1])))


(def v116_l382 (sort (keys ir-31)))


(deftest
 t117_l384
 (is (= v116_l382 [:dimension :generators :lambda :syts])))


(def v119_l395 (:lambda ir-31))


(deftest t120_l397 (is (= v119_l395 [3 1])))


(def v121_l399 (:dimension ir-31))


(deftest t122_l401 (is (= v121_l399 3)))


(def v124_l405 (:syts ir-31))


(deftest t125_l407 (is ((fn [syts] (= 3 (count syts))) v124_l405)))


(def v127_l413 (count (:generators ir-31)))


(deftest t128_l415 (is (= v127_l413 3)))


(def v130_l421 (hm/rep-matrix ir-31 [1 0 2 3]))


(def
 v132_l427
 (let
  [rep1
   (hm/irrep [3 1])
   rep2
   (hm/irrep [2 2])
   tp
   (hm/tensor-product rep1 rep2)]
  {:has-matrix-fn (some? (:matrix-fn tp)), :dimension (:dimension tp)}))


(deftest
 t133_l433
 (is
  ((fn [m] (and (:has-matrix-fn m) (= 6 (:dimension m)))) v132_l427)))


(def
 v135_l452
 (let
  [G
   (hm/cyclic-group 8)
   ct
   (hm/character-table G)
   signal
   (t/complex-tensor-real [1 0 1 0 1 0 1 0])]
  (hm/fourier-transform ct signal)))
