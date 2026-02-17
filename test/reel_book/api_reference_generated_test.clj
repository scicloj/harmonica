(ns
 reel-book.api-reference-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.reel.representations :as rep]
  [fastmath.complex :as c]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l18 (kind/doc #'reel/cyclic-group))


(def v4_l20 (reel/cyclic-group 5))


(deftest t5_l22 (is ((fn [v] (= (reel/order v) 5)) v4_l20)))


(def v6_l24 (kind/doc #'reel/symmetric-group))


(def v7_l26 (reel/symmetric-group 3))


(deftest t8_l28 (is ((fn [v] (= (reel/order v) 6)) v7_l26)))


(def v9_l30 (kind/doc #'reel/dihedral-group))


(def v10_l32 (reel/dihedral-group 4))


(deftest t11_l34 (is ((fn [v] (= (reel/order v) 8)) v10_l32)))


(def v12_l36 (kind/doc #'reel/product-group))


(def
 v13_l38
 (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 3)))


(deftest t14_l40 (is ((fn [v] (= (reel/order v) 6)) v13_l38)))


(def v16_l44 (kind/doc #'reel/op))


(def v17_l46 (reel/op (reel/cyclic-group 7) 3 5))


(deftest t18_l48 (is (= v17_l46 1)))


(def v19_l50 (reel/op (reel/symmetric-group 3) [1 2 0] [0 2 1]))


(deftest t20_l52 (is (= v19_l50 [1 0 2])))


(def v21_l54 (kind/doc #'reel/inv))


(def v22_l56 (reel/inv (reel/cyclic-group 7) 3))


(deftest t23_l58 (is (= v22_l56 4)))


(def v24_l60 (reel/inv (reel/symmetric-group 3) [1 2 0]))


(deftest t25_l62 (is (= v24_l60 [2 0 1])))


(def v26_l64 (kind/doc #'reel/id))


(def v27_l66 (reel/id (reel/cyclic-group 5)))


(deftest t28_l68 (is (= v27_l66 0)))


(def v29_l70 (reel/id (reel/symmetric-group 3)))


(deftest t30_l72 (is (= v29_l70 [0 1 2])))


(def v31_l74 (reel/id (reel/dihedral-group 4)))


(deftest t32_l76 (is (= v31_l74 [:r 0])))


(def v33_l78 (kind/doc #'reel/elements))


(def v34_l80 (vec (reel/elements (reel/cyclic-group 4))))


(deftest t35_l82 (is (= v34_l80 [0 1 2 3])))


(def v36_l84 (kind/doc #'reel/order))


(def v37_l86 (reel/order (reel/symmetric-group 4)))


(deftest t38_l88 (is (= v37_l86 24)))


(def v39_l90 (kind/doc #'reel/conjugacy-classes))


(def
 v40_l92
 (let
  [classes (reel/conjugacy-classes (reel/symmetric-group 3))]
  (mapv :size classes)))


(deftest t41_l95 (is (= v40_l92 [2 3 1])))


(def v43_l99 (kind/doc #'reel/cycles))


(def v44_l101 (reel/cycles [1 2 3 0]))


(deftest t45_l103 (is (= v44_l101 [[0 1 2 3]])))


(def v46_l105 (reel/cycles [1 0 3 2]))


(deftest t47_l107 (is (= v46_l105 [[0 1] [2 3]])))


(def v48_l109 (kind/doc #'reel/cycle-type))


(def v49_l111 (reel/cycle-type [1 0 3 2]))


(deftest t50_l113 (is (= v49_l111 [2 2])))


(def v51_l115 (kind/doc #'reel/sign))


(def v52_l117 (reel/sign [1 0 2 3]))


(deftest t53_l119 (is (= v52_l117 -1)))


(def v54_l121 (reel/sign [0 1 2 3]))


(deftest t55_l123 (is (= v54_l121 1)))


(def v56_l125 (kind/doc #'reel/identity-perm))


(def v57_l127 (reel/identity-perm 4))


(deftest t58_l129 (is (= v57_l127 [0 1 2 3])))


(def v59_l131 (kind/doc #'reel/transposition))


(def v60_l133 (reel/transposition 5 1 3))


(deftest t61_l135 (is (= v60_l133 [0 3 2 1 4])))


(def v62_l137 (kind/doc #'reel/adjacent-transposition-decomposition))


(def v63_l139 (reel/adjacent-transposition-decomposition [2 0 1]))


(deftest t64_l141 (is (vector? v63_l139)))


(def v66_l145 (kind/doc #'reel/partitions))


(def v67_l147 (reel/partitions 4))


(deftest t68_l149 (is (= v67_l147 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v69_l151 (kind/doc #'reel/partition-conjugate))


(def v70_l153 (reel/partition-conjugate [4 2 1]))


(deftest t71_l155 (is (= v70_l153 [3 2 1 1])))


(def v73_l159 (kind/doc #'reel/standard-young-tableaux))


(def v74_l161 (reel/standard-young-tableaux [2 1]))


(deftest t75_l163 (is (= v74_l161 [[[1 2] [3]] [[1 3] [2]]])))


(def v76_l165 (kind/doc #'reel/hook-length-dimension))


(def v77_l167 (reel/hook-length-dimension [3 2]))


(deftest t78_l169 (is (= v77_l167 5)))


(def v79_l171 (reel/hook-length-dimension [2 2 1]))


(deftest t80_l173 (is (= v79_l171 5)))


(def v82_l177 (kind/doc #'reel/character-table))


(def
 v83_l179
 (let
  [ct (reel/character-table (reel/cyclic-group 3))]
  (count (:table ct))))


(deftest t84_l182 (is (= v83_l179 3)))


(def
 v85_l184
 (let
  [ct
   (reel/character-table (reel/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__77161#] (long (Math/round (c/re p1__77161#))))
      row))
    (:table ct))]
  re-table))


(deftest t86_l189 (is (= v85_l184 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def v87_l191 (kind/doc #'reel/character-inner-product))


(def
 v88_l193
 (let
  [ct
   (reel/character-table (reel/symmetric-group 3))
   {:keys [table class-sizes]}
   ct
   order
   (reel/order (:group ct))]
  (c/re
   (reel/character-inner-product
    (nth table 0)
    (nth table 1)
    class-sizes
    order))))


(deftest t89_l198 (is ((fn [v] (< (Math/abs v) 1.0E-10)) v88_l193)))


(def v91_l202 (kind/doc #'reel/irrep))


(def v92_l204 (let [ir (reel/irrep [2 1])] (reel/rep-dimension ir)))


(deftest t93_l207 (is (= v92_l204 2)))


(def v94_l209 (kind/doc #'reel/rep-matrix))


(def
 v95_l211
 (let [ir (reel/irrep [2 1])] (fm/nrow (reel/rep-matrix ir [1 0 2]))))


(deftest t96_l214 (is (= v95_l211 2)))


(def v97_l216 (kind/doc #'reel/rep-dimension))


(def v98_l218 (reel/rep-dimension (reel/irrep [3 1])))


(deftest t99_l220 (is (= v98_l218 3)))


(def v100_l222 (kind/doc #'reel/rep-character))


(def
 v101_l224
 (let [ir (reel/irrep [2 1])] (reel/rep-character ir [0 1 2])))


(deftest
 t102_l227
 (is ((fn [v] (< (Math/abs (- v 2.0)) 1.0E-10)) v101_l224)))


(def v103_l229 (kind/doc #'reel/rep-generators))


(def
 v104_l231
 (let [ir (reel/irrep [2 1])] (count (reel/rep-generators ir))))


(deftest t105_l234 (is (= v104_l231 2)))


(def v106_l236 (kind/doc #'reel/tensor-product))


(def
 v107_l238
 (let
  [ir1
   (reel/irrep [2 1])
   ir2
   (reel/irrep [2 1])
   tp
   (reel/tensor-product ir1 ir2)]
  (reel/rep-dimension tp)))


(deftest t108_l243 (is (= v107_l238 4)))


(def v109_l245 (kind/doc #'reel/direct-sum))


(def
 v110_l247
 (let
  [ir1
   (reel/irrep [2 1])
   ir2
   (reel/irrep [1 1 1])
   ds
   (reel/direct-sum ir1 ir2)]
  (reel/rep-dimension ds)))


(deftest t111_l252 (is (= v110_l247 3)))


(def v112_l254 (kind/doc #'reel/frobenius-norm-sq))


(def
 v113_l256
 (let
  [M (fm/rows->mat [[1.0 0.0] [0.0 1.0]])]
  (reel/frobenius-norm-sq M)))


(deftest
 t114_l259
 (is ((fn [v] (< (Math/abs (- v 2.0)) 1.0E-10)) v113_l256)))


(def v115_l261 (kind/doc #'reel/frobenius-norm))


(def
 v116_l263
 (let [M (fm/rows->mat [[3.0 0.0] [0.0 4.0]])] (reel/frobenius-norm M)))


(deftest
 t117_l266
 (is ((fn [v] (< (Math/abs (- v 5.0)) 1.0E-10)) v116_l263)))


(def v118_l268 (kind/doc #'reel/matrix-fourier-transform))


(def
 v119_l270
 (let
  [G
   (reel/symmetric-group 3)
   ir
   (reel/irrep [2 1])
   f
   (zipmap (reel/elements G) (repeat 1.0))
   fhat
   (reel/matrix-fourier-transform ir G f)]
  (fm/nrow fhat)))


(deftest t120_l276 (is (= v119_l270 2)))


(def v122_l280 (kind/doc #'reel/rising-sequences))


(def v123_l282 (reel/rising-sequences [0 1 2 3]))


(deftest t124_l284 (is (= v123_l282 1)))


(def v125_l286 (reel/rising-sequences [3 2 1 0]))


(deftest t126_l288 (is (= v125_l286 4)))


(def v127_l290 (kind/doc #'reel/gsr-probability))


(def v128_l292 (let [p (reel/gsr-probability [0 1 2 3] 1)] (> p 0.0)))


(deftest t129_l295 (is (true? v128_l292)))


(def v131_l299 (kind/doc #'reel/orbit))


(def
 v132_l301
 (let
  [G
   (reel/cyclic-group 4)
   act
   (fn [g x] (mod (+ (long g) (long x)) 4))]
  (reel/orbit G act 0)))


(deftest t133_l305 (is (= v132_l301 #{0 1 3 2})))


(def v134_l307 (kind/doc #'reel/orbits))


(def
 v135_l309
 (let
  [G
   (reel/cyclic-group 3)
   act
   (fn [g x] (mod (+ (long g) (long x)) 3))]
  (count (reel/orbits G act (range 3)))))


(deftest t136_l313 (is (= v135_l309 1)))


(def v137_l315 (kind/doc #'reel/fixed-points))


(def
 v138_l317
 (let
  [act (fn [g x] (mod (+ (long g) (long x)) 5))]
  (reel/fixed-points act 0 (range 5))))


(deftest t139_l320 (is (= v138_l317 #{0 1 4 3 2})))


(def v140_l322 (kind/doc #'reel/stabilizer))


(def
 v141_l324
 (let
  [G
   (reel/cyclic-group 4)
   act
   (fn [g x] (mod (+ (long g) (long x)) 4))]
  (reel/stabilizer G act 0)))


(deftest t142_l328 (is (= v141_l324 #{0})))


(def v143_l330 (kind/doc #'reel/burnside-count))


(def
 v144_l332
 (let
  [G
   (reel/cyclic-group 4)
   act
   (fn
    [g coloring]
    (mapv
     (fn* [p1__77162#] (coloring (mod (+ p1__77162# (long g)) 4)))
     (range 4)))
   domain
   [[0 0 0 0]
    [0 0 0 1]
    [0 0 1 0]
    [0 0 1 1]
    [0 1 0 0]
    [0 1 0 1]
    [0 1 1 0]
    [0 1 1 1]
    [1 0 0 0]
    [1 0 0 1]
    [1 0 1 0]
    [1 0 1 1]
    [1 1 0 0]
    [1 1 0 1]
    [1 1 1 0]
    [1 1 1 1]]]
  (reel/burnside-count G act domain)))


(deftest t145_l340 (is (= v144_l332 6)))


(def v146_l342 (kind/doc #'reel/cycle-index))


(def
 v147_l344
 (let
  [G
   (reel/cyclic-group 3)
   act
   (fn [g x] (mod (+ (long g) (long x)) 3))
   ci
   (reel/cycle-index G act (range 3))]
  (= 1 (reduce + (vals ci)))))


(deftest t148_l349 (is (true? v147_l344)))


(def v149_l351 (kind/doc #'reel/polya-count))


(def
 v150_l353
 (let
  [G
   (reel/cyclic-group 4)
   act
   (fn [g x] (mod (+ (long g) (long x)) 4))
   ci
   (reel/cycle-index G act (range 4))]
  (reel/polya-count ci 2)))


(deftest t151_l358 (is (= v150_l353 6)))


(def v152_l360 (kind/doc #'reel/subset-action))


(def
 v153_l362
 (let
  [perm-act
   (fn [sigma x] (sigma x))
   {:keys [domain]}
   (reel/subset-action perm-act (range 4) 2)]
  (count domain)))


(deftest t154_l366 (is (= v153_l362 6)))


(def v156_l370 (kind/doc #'reel/fourier-transform))


(def
 v157_l372
 (let
  [ct
   (reel/character-table (reel/cyclic-group 4))
   f
   (mapv
    (fn* [p1__77163#] (c/complex (double p1__77163#) 0.0))
    [1 0 0 0])
   fhat
   (reel/fourier-transform ct f)]
  (count fhat)))


(deftest t158_l377 (is (= v157_l372 4)))


(def v159_l379 (kind/doc #'reel/inverse-fourier-transform))


(def
 v160_l381
 (let
  [ct
   (reel/character-table (reel/cyclic-group 4))
   f
   (mapv
    (fn* [p1__77164#] (c/complex (double p1__77164#) 0.0))
    [1 2 3 4])
   fhat
   (reel/fourier-transform ct f)
   f-back
   (reel/inverse-fourier-transform ct fhat)
   max-err
   (apply
    max
    (map
     (fn*
      [p1__77165# p2__77166#]
      (c/abs (c/sub p1__77165# p2__77166#)))
     f
     f-back))]
  (< max-err 1.0E-10)))


(deftest t161_l388 (is (true? v160_l381)))


(def v162_l390 (kind/doc #'reel/convolve))


(def
 v163_l392
 (let
  [ct
   (reel/character-table (reel/cyclic-group 4))
   f
   (mapv
    (fn* [p1__77167#] (c/complex (double p1__77167#) 0.0))
    [1 0 0 0])
   g
   (mapv
    (fn* [p1__77168#] (c/complex (double p1__77168#) 0.0))
    [0 1 0 0])
   conv
   (reel/convolve ct f g)]
  (long (Math/round (c/re (nth conv 1))))))


(deftest t164_l398 (is (= v163_l392 1)))


(def v165_l400 (kind/doc #'reel/total-variation-distance))


(def
 v166_l402
 (reel/total-variation-distance
  [0.5 0.5 0.0 0.0]
  [0.25 0.25 0.25 0.25]))


(deftest
 t167_l404
 (is ((fn [v] (< (Math/abs (- v 0.5)) 1.0E-10)) v166_l402)))
