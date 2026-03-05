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
  [tech.v3.datatype.functional :as dfn]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l57 (vec (mn/partition-seq [3 2])))


(deftest t4_l59 (is (= v3_l57 [true true false true false])))


(def v6_l63 (vec (mn/partition-seq [4 2 1])))


(deftest t7_l65 (is (= v6_l63 [true false true false true true false])))


(def v9_l82 (mn/chi [3 2] [2 2 1]))


(deftest t10_l84 (is (= v9_l82 1)))


(def
 v12_l88
 (let
  [ct
   (hm/character-table (hm/symmetric-group 5))
   row-idx
   (.indexOf (:irrep-labels ct) [3 2])
   col-idx
   (.indexOf (:classes ct) [2 2 1])]
  (el/re (((:table ct) row-idx) col-idx))))


(deftest t13_l94 (is (= v12_l88 1.0)))


(def v15_l101 (mn/chi [10 5 3 2] [4 4 4 3 3 2]))


(deftest t16_l103 (is ((fn [v] (integer? v)) v15_l101)))


(def v18_l119 (yt/standard-young-tableaux [3 1]))


(deftest t19_l121 (is ((fn [syts] (= 3 (count syts))) v18_l119)))


(def v21_l135 (yt/axial-distance [[1 2 4] [3]] 2 3))


(deftest t22_l137 (is (= v21_l135 -2)))


(def v24_l155 (let [ir (hm/irrep [3 1])] (first (:generators ir))))


(def v26_l165 (perm/adjacent-transposition-decomposition [2 0 1 3]))


(def
 v28_l170
 (let
  [decomp (perm/adjacent-transposition-decomposition [2 0 1 3])]
  (reduce
   (fn [acc i] (perm/compose acc (perm/from-cycles 4 [[i (inc i)]])))
   (perm/identity-perm 4)
   decomp)))


(deftest t29_l176 (is (= v28_l170 [2 0 1 3])))


(def
 v31_l181
 (let
  [ir (hm/irrep [3 1]) sigma [2 0 1 3] M (hm/rep-matrix ir sigma)]
  {:trace (fm/trace M),
   :is-orthogonal
   (<
    (fm/trace (fm/sub (fm/mulm M (fm/transpose M)) (fm/eye 3 true)))
    1.0E-10)}))


(deftest
 t32_l190
 (is ((fn [m] (and (number? (:trace m)) (:is-orthogonal m))) v31_l181)))


(def
 v34_l220
 (let
  [cls (first (hm/conjugacy-classes (hm/symmetric-group 4)))]
  {:cycle-type (:cycle-type cls),
   :size (:size cls),
   :has-elements (some? (:elements cls))}))


(deftest
 t35_l225
 (is
  ((fn
    [m]
    (and (= [4] (:cycle-type m)) (= 6 (:size m)) (:has-elements m)))
   v34_l220)))


(def
 v37_l231
 (let
  [cls (first (hm/conjugacy-classes (hm/symmetric-group 12)))]
  {:cycle-type (:cycle-type cls),
   :size (:size cls),
   :has-elements (some? (:elements cls))}))


(deftest
 t38_l236
 (is
  ((fn [m] (and (= [12] (:cycle-type m)) (not (:has-elements m))))
   v37_l231)))


(def v40_l241 (part/partition-class-size 6 [3 2 1]))


(deftest t41_l243 (is (= v40_l241 120)))


(def
 v43_l266
 (let
  [G (hm/cyclic-group 6) act (fn [g x] (mod (+ g x) 6))]
  (hm/cycle-index G act (range 6))))


(deftest t44_l270 (is ((fn [ci] (= 4 (count ci))) v43_l266)))


(def
 v46_l275
 (let
  [G
   (hm/cyclic-group 6)
   act
   (fn [g x] (mod (+ g x) 6))
   ci
   (hm/cycle-index G act (range 6))]
  (hm/polya-count ci 3)))


(deftest t47_l280 (is (= v46_l275 130)))


(def
 v49_l287
 (let
  [G
   (hm/cyclic-group 6)
   act
   (fn [g x] (mod (+ g x) 6))
   {:keys [act domain]}
   (hm/coloring-action act 6 3)]
  (hm/burnside-count G act domain)))


(deftest t50_l292 (is (= v49_l287 130)))


(def
 v52_l314
 (let
  [sigma [1 3 0 2]]
  {:inverse (perm/inverse sigma),
   :descents (riffle/descents (perm/inverse sigma)),
   :rising-seqs (riffle/rising-sequences sigma)}))


(deftest t53_l319 (is ((fn [m] (= 3 (:rising-seqs m))) v52_l314)))


(def v55_l327 (riffle/gsr-probability [0 1 2 3] 1))


(deftest
 t56_l329
 (is ((fn [p] (< (Math/abs (- p (/ 5.0 16))) 1.0E-10)) v55_l327)))


(def
 v58_l343
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
       (fn* [p1__68080#] (riffle/gsr-probability p1__68080# k))
       elts)]
     (double (* 0.5 (dfn/sum (dfn/abs (dfn/- probs uniform)))))))
   (range 1 8))))


(deftest
 t59_l351
 (is
  ((fn [tvs] (and (> (first tvs) 0.4) (< (last tvs) 0.01))) v58_l343)))


(def
 v61_l364
 (let
  [ir
   (hm/irrep [3 1])
   restricted
   (hm/restrict-rep ir 4 3)
   M
   (hm/rep-matrix restricted [1 2 0])]
  (fm/trace M)))


(deftest t62_l370 (is ((fn [t] (number? t)) v61_l364)))


(def
 v64_l385
 (let
  [ir-triv
   {:dimension 1, :matrix-fn (fn [_] (fm/eye 1 true))}
   induced
   (hm/induce-rep ir-triv 2 4)]
  {:dimension (:dimension induced), :expected (/ 24 2)}))


(deftest
 t65_l391
 (is ((fn [m] (= (:dimension m) (:expected m))) v64_l385)))


(def v67_l403 (hm/branching-rule [3 1]))


(deftest t68_l405 (is ((fn [m] (= m {[3] 1, [2 1] 1})) v67_l403)))
