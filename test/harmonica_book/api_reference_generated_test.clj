(ns
 harmonica-book.api-reference-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [scicloj.harmonica.protocols :as p]
  [scicloj.harmonica.analysis.representations :as rep]
  [fastmath.matrix :as fm]
  [tech.v3.tensor :as tensor]
  [tech.v3.datatype :as dtype]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l21 (kind/doc #'hm/cyclic-group))


(def v4_l23 (hm/cyclic-group 5))


(deftest t5_l25 (is ((fn [v] (= (hm/order v) 5)) v4_l23)))


(def v6_l27 (kind/doc #'hm/symmetric-group))


(def v7_l29 (hm/symmetric-group 3))


(deftest t8_l31 (is ((fn [v] (= (hm/order v) 6)) v7_l29)))


(def v9_l33 (kind/doc #'hm/dihedral-group))


(def v10_l35 (hm/dihedral-group 4))


(deftest t11_l37 (is ((fn [v] (= (hm/order v) 8)) v10_l35)))


(def v12_l39 (kind/doc #'hm/product-group))


(def v13_l41 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3)))


(deftest t14_l43 (is ((fn [v] (= (hm/order v) 6)) v13_l41)))


(def v16_l47 (kind/doc #'hm/op))


(def v17_l49 (hm/op (hm/cyclic-group 7) 3 5))


(deftest t18_l51 (is (= v17_l49 1)))


(def v19_l53 (hm/op (hm/symmetric-group 3) [1 2 0] [0 2 1]))


(deftest t20_l55 (is (= v19_l53 [1 0 2])))


(def v21_l57 (kind/doc #'hm/inv))


(def v22_l59 (hm/inv (hm/cyclic-group 7) 3))


(deftest t23_l61 (is (= v22_l59 4)))


(def v24_l63 (hm/inv (hm/symmetric-group 3) [1 2 0]))


(deftest t25_l65 (is (= v24_l63 [2 0 1])))


(def v26_l67 (kind/doc #'hm/id))


(def v27_l69 (hm/id (hm/cyclic-group 5)))


(deftest t28_l71 (is (= v27_l69 0)))


(def v29_l73 (hm/id (hm/symmetric-group 3)))


(deftest t30_l75 (is (= v29_l73 [0 1 2])))


(def v31_l77 (hm/id (hm/dihedral-group 4)))


(deftest t32_l79 (is (= v31_l77 [:r 0])))


(def v33_l81 (kind/doc #'hm/elements))


(def v34_l83 (vec (hm/elements (hm/cyclic-group 4))))


(deftest t35_l85 (is (= v34_l83 [0 1 2 3])))


(def v36_l87 (kind/doc #'hm/order))


(def v37_l89 (hm/order (hm/symmetric-group 4)))


(deftest t38_l91 (is (= v37_l89 24)))


(def v39_l93 (kind/doc #'hm/conjugacy-classes))


(def
 v40_l95
 (let
  [classes (hm/conjugacy-classes (hm/symmetric-group 3))]
  (mapv :size classes)))


(deftest t41_l98 (is (= v40_l95 [2 3 1])))


(def v43_l102 (kind/doc #'hm/cycles))


(def v44_l104 (hm/cycles [1 2 3 0]))


(deftest t45_l106 (is (= v44_l104 [[0 1 2 3]])))


(def v46_l108 (hm/cycles [1 0 3 2]))


(deftest t47_l110 (is (= v46_l108 [[0 1] [2 3]])))


(def v48_l112 (kind/doc #'hm/cycle-type))


(def v49_l114 (hm/cycle-type [1 0 3 2]))


(deftest t50_l116 (is (= v49_l114 [2 2])))


(def v51_l118 (kind/doc #'hm/sign))


(def v52_l120 (hm/sign [1 0 2 3]))


(deftest t53_l122 (is (= v52_l120 -1)))


(def v54_l124 (hm/sign [0 1 2 3]))


(deftest t55_l126 (is (= v54_l124 1)))


(def v56_l128 (kind/doc #'hm/identity-perm))


(def v57_l130 (hm/identity-perm 4))


(deftest t58_l132 (is (= v57_l130 [0 1 2 3])))


(def v59_l134 (kind/doc #'hm/transposition))


(def v60_l136 (hm/transposition 5 1 3))


(deftest t61_l138 (is (= v60_l136 [0 3 2 1 4])))


(def v62_l140 (kind/doc #'hm/adjacent-transposition-decomposition))


(def v63_l142 (hm/adjacent-transposition-decomposition [2 0 1]))


(deftest t64_l144 (is (vector? v63_l142)))


(def v66_l148 (kind/doc #'hm/partitions))


(def v67_l150 (hm/partitions 4))


(deftest t68_l152 (is (= v67_l150 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v69_l154 (kind/doc #'hm/partition-conjugate))


(def v70_l156 (hm/partition-conjugate [4 2 1]))


(deftest t71_l158 (is (= v70_l156 [3 2 1 1])))


(def v73_l162 (kind/doc #'hm/standard-young-tableaux))


(def v74_l164 (hm/standard-young-tableaux [2 1]))


(deftest t75_l166 (is (= v74_l164 [[[1 2] [3]] [[1 3] [2]]])))


(def v76_l168 (kind/doc #'hm/hook-length-dimension))


(def v77_l170 (hm/hook-length-dimension [3 2]))


(deftest t78_l172 (is (= v77_l170 5)))


(def v79_l174 (hm/hook-length-dimension [2 2 1]))


(deftest t80_l176 (is (= v79_l174 5)))


(def v82_l180 (kind/doc #'hm/character-table))


(def
 v83_l182
 (let
  [ct (hm/character-table (hm/cyclic-group 3))]
  (count (:table ct))))


(deftest t84_l185 (is (= v83_l182 3)))


(def
 v85_l187
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv (fn* [p1__124015#] (Math/round (cx/re p1__124015#))) row))
    (:table ct))]
  re-table))


(deftest t86_l192 (is (= v85_l187 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def v87_l194 (kind/doc #'hm/character-inner-product))


(def
 v88_l196
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   {:keys [table class-sizes]}
   ct
   order
   (hm/order (:group ct))]
  (cx/re
   (hm/character-inner-product
    (nth table 0)
    (nth table 1)
    class-sizes
    order))))


(deftest t89_l201 (is ((fn [v] (< (Math/abs v) 1.0E-10)) v88_l196)))


(def v90_l203 (kind/doc #'hm/format-cx))


(def v91_l205 (hm/format-cx (cx/complex 0 1)))


(deftest t92_l207 (is (= v91_l205 "i")))


(def v93_l209 (kind/doc #'hm/show-character-table))


(def
 v94_l211
 (hm/show-character-table (hm/character-table (hm/symmetric-group 3))))


(def v96_l215 (kind/doc #'hm/irrep))


(def v97_l217 (let [ir (hm/irrep [2 1])] (hm/rep-dimension ir)))


(deftest t98_l220 (is (= v97_l217 2)))


(def v99_l222 (kind/doc #'hm/rep-matrix))


(def
 v100_l224
 (let [ir (hm/irrep [2 1])] (fm/nrow (hm/rep-matrix ir [1 0 2]))))


(deftest t101_l227 (is (= v100_l224 2)))


(def v102_l229 (kind/doc #'hm/rep-dimension))


(def v103_l231 (hm/rep-dimension (hm/irrep [3 1])))


(deftest t104_l233 (is (= v103_l231 3)))


(def v105_l235 (kind/doc #'hm/rep-character))


(def
 v106_l237
 (let [ir (hm/irrep [2 1])] (hm/rep-character ir [0 1 2])))


(deftest
 t107_l240
 (is ((fn [v] (< (Math/abs (- v 2.0)) 1.0E-10)) v106_l237)))


(def v108_l242 (kind/doc #'hm/rep-generators))


(def
 v109_l244
 (let [ir (hm/irrep [2 1])] (count (hm/rep-generators ir))))


(deftest t110_l247 (is (= v109_l244 2)))


(def v111_l249 (kind/doc #'hm/tensor-product))


(def
 v112_l251
 (let
  [ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [2 1])
   tp
   (hm/tensor-product ir1 ir2)]
  (hm/rep-dimension tp)))


(deftest t113_l256 (is (= v112_l251 4)))


(def v114_l258 (kind/doc #'hm/direct-sum))


(def
 v115_l260
 (let
  [ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [1 1 1])
   ds
   (hm/direct-sum ir1 ir2)]
  (hm/rep-dimension ds)))


(deftest t116_l265 (is (= v115_l260 3)))


(def v117_l267 (kind/doc #'hm/frobenius-norm-sq))


(def
 v118_l269
 (let
  [M (fm/rows->mat [[1.0 0.0] [0.0 1.0]])]
  (hm/frobenius-norm-sq M)))


(deftest
 t119_l272
 (is ((fn [v] (< (Math/abs (- v 2.0)) 1.0E-10)) v118_l269)))


(def v120_l274 (kind/doc #'hm/frobenius-norm))


(def
 v121_l276
 (let [M (fm/rows->mat [[3.0 0.0] [0.0 4.0]])] (hm/frobenius-norm M)))


(deftest
 t122_l279
 (is ((fn [v] (< (Math/abs (- v 5.0)) 1.0E-10)) v121_l276)))


(def v123_l281 (kind/doc #'hm/matrix-fourier-transform))


(def
 v124_l283
 (let
  [G
   (hm/symmetric-group 3)
   ir
   (hm/irrep [2 1])
   f
   (zipmap (hm/elements G) (repeat 1.0))
   fhat
   (hm/matrix-fourier-transform ir G f)]
  (fm/nrow fhat)))


(deftest t125_l289 (is (= v124_l283 2)))


(def v127_l293 (kind/doc #'hm/rising-sequences))


(def v128_l295 (hm/rising-sequences [0 1 2 3]))


(deftest t129_l297 (is (= v128_l295 1)))


(def v130_l299 (hm/rising-sequences [3 2 1 0]))


(deftest t131_l301 (is (= v130_l299 4)))


(def v132_l303 (kind/doc #'hm/gsr-probability))


(def v133_l305 (let [p (hm/gsr-probability [0 1 2 3] 1)] (> p 0.0)))


(deftest t134_l308 (is (true? v133_l305)))


(def v136_l312 (kind/doc #'hm/orbit))


(def
 v137_l314
 (let
  [G (hm/cyclic-group 4) act (fn [g x] (mod (+ g x) 4))]
  (hm/orbit G act 0)))


(deftest t138_l318 (is (= v137_l314 #{0 1 3 2})))


(def v139_l320 (kind/doc #'hm/orbits))


(def
 v140_l322
 (let
  [G (hm/cyclic-group 3) act (fn [g x] (mod (+ g x) 3))]
  (count (hm/orbits G act (range 3)))))
