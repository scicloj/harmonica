(ns
 harmonica-book.algorithms-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [scicloj.harmonica.protocols :as p]
  [scicloj.harmonica.combinatorics.permutation :as perm]
  [scicloj.harmonica.combinatorics.partition :as part]
  [scicloj.harmonica.combinatorics.young-tableaux :as yt]
  [scicloj.harmonica.combinatorics.murnaghan-nakayama :as mn]
  [scicloj.harmonica.combinatorics.young-orthogonal :as yo]
  [scicloj.harmonica.combinatorics.riffle :as riffle]
  [scicloj.harmonica.analysis.representations :as rep]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l56 (vec (mn/partition-seq [3 2])))


(deftest t4_l58 (is (= v3_l56 [true true false true false])))


(def v6_l62 (vec (mn/partition-seq [4 2 1])))


(deftest t7_l64 (is (= v6_l62 [true false true false true true false])))


(def v9_l81 (mn/chi [3 2] [2 2 1]))


(deftest t10_l83 (is (= v9_l81 1)))


(def
 v12_l87
 (let
  [ct
   (hm/character-table (hm/symmetric-group 5))
   row-idx
   (.indexOf (:irrep-labels ct) [3 2])
   col-idx
   (.indexOf (:classes ct) [2 2 1])]
  (el/re (((:table ct) row-idx) col-idx))))


(deftest t13_l93 (is (= v12_l87 1.0)))


(def v15_l100 (mn/chi [10 5 3 2] [4 4 4 3 3 2]))


(deftest t16_l102 (is ((fn [v] (integer? v)) v15_l100)))


(def v18_l118 (yt/standard-young-tableaux [3 1]))


(deftest t19_l120 (is ((fn [syts] (= 3 (count syts))) v18_l118)))


(def v21_l134 (yt/axial-distance [[1 2 4] [3]] 2 3))


(deftest t22_l136 (is (= v21_l134 -2)))


(def v24_l154 (let [ir (hm/irrep [3 1])] (first (:generators ir))))


(def v26_l164 (perm/adjacent-transposition-decomposition [2 0 1 3]))


(def
 v28_l169
 (let
  [decomp (perm/adjacent-transposition-decomposition [2 0 1 3])]
  (reduce
   (fn [acc i] (perm/compose acc (perm/from-cycles 4 [[i (inc i)]])))
   (perm/identity-perm 4)
   decomp)))


(deftest t29_l175 (is (= v28_l169 [2 0 1 3])))


(def
 v31_l180
 (let
  [ir (hm/irrep [3 1]) sigma [2 0 1 3] M (hm/rep-matrix ir sigma)]
  {:trace (fm/trace M),
   :is-orthogonal
   (<
    (fm/trace (fm/sub (fm/mulm M (fm/transpose M)) (fm/eye 3 true)))
    1.0E-10)}))


(deftest
 t32_l189
 (is ((fn [m] (and (number? (:trace m)) (:is-orthogonal m))) v31_l180)))


(def
 v34_l219
 (let
  [cls (first (hm/conjugacy-classes (hm/symmetric-group 4)))]
  {:cycle-type (:cycle-type cls),
   :size (:size cls),
   :has-elements (some? (:elements cls))}))


(deftest
 t35_l224
 (is
  ((fn
    [m]
    (and (= [4] (:cycle-type m)) (= 6 (:size m)) (:has-elements m)))
   v34_l219)))


(def
 v37_l230
 (let
  [cls (first (hm/conjugacy-classes (hm/symmetric-group 12)))]
  {:cycle-type (:cycle-type cls),
   :size (:size cls),
   :has-elements (some? (:elements cls))}))


(deftest
 t38_l235
 (is
  ((fn [m] (and (= [12] (:cycle-type m)) (not (:has-elements m))))
   v37_l230)))


(def v40_l240 (part/partition-class-size 6 [3 2 1]))


(deftest t41_l242 (is (= v40_l240 120)))


(def
 v43_l265
 (let
  [G (hm/cyclic-group 6) act (fn [g x] (mod (+ g x) 6))]
  (hm/cycle-index G act (range 6))))


(deftest t44_l269 (is ((fn [ci] (= 4 (count ci))) v43_l265)))


(def
 v46_l274
 (let
  [G
   (hm/cyclic-group 6)
   act
   (fn [g x] (mod (+ g x) 6))
   ci
   (hm/cycle-index G act (range 6))]
  (hm/polya-count ci 3)))


(deftest t47_l279 (is (= v46_l274 130)))


(def
 v49_l286
 (let
  [G
   (hm/cyclic-group 6)
   act
   (fn [g x] (mod (+ g x) 6))
   {:keys [act domain]}
   (hm/coloring-action act 6 3)]
  (hm/burnside-count G act domain)))


(deftest t50_l291 (is (= v49_l286 130)))


(def
 v52_l313
 (let
  [sigma [1 3 0 2]]
  {:inverse (perm/inverse sigma),
   :descents (riffle/descents (perm/inverse sigma)),
   :rising-seqs (riffle/rising-sequences sigma)}))


(deftest t53_l318 (is ((fn [m] (= 3 (:rising-seqs m))) v52_l313)))


(def v55_l326 (riffle/gsr-probability [0 1 2 3] 1))


(deftest
 t56_l328
 (is ((fn [p] (< (Math/abs (- p (/ 5.0 16))) 1.0E-10)) v55_l326)))


(def
 v58_l342
 (let
  [G
   (hm/symmetric-group 4)
   elts
   (vec (hm/elements G))
   uniform
   (/ 1.0 (hm/order G))]
  (mapv
   (fn
    [k]
    (let
     [probs
      (mapv
       (fn* [p1__76825#] (riffle/gsr-probability p1__76825# k))
       elts)]
     (double (* 0.5 (el/sum (el/abs (el/- probs uniform)))))))
   (range 1 8))))


(deftest
 t59_l350
 (is
  ((fn [tvs] (and (> (first tvs) 0.4) (< (last tvs) 0.01))) v58_l342)))


(def
 v61_l363
 (let
  [ir
   (hm/irrep [3 1])
   restricted
   (hm/restrict-rep ir 4 3)
   M
   (hm/rep-matrix restricted [1 2 0])]
  (fm/trace M)))


(deftest t62_l369 (is ((fn [t] (number? t)) v61_l363)))


(def
 v64_l384
 (let
  [ir-triv
   {:dimension 1, :matrix-fn (fn [_] (fm/eye 1 true))}
   induced
   (hm/induce-rep ir-triv 2 4)]
  {:dimension (:dimension induced), :expected (/ 24 2)}))


(deftest
 t65_l390
 (is ((fn [m] (= (:dimension m) (:expected m))) v64_l384)))


(def v67_l402 (hm/branching-rule [3 1]))


(deftest t68_l404 (is ((fn [m] (= m {[3] 1, [2 1] 1})) v67_l402)))
