(ns
 harmonica-book.api-reference-generated-test
 (:require
  [scicloj.harmonica.core :as hm]
  [scicloj.harmonica.representations :as rep]
  [fastmath.complex :as c]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l18 (kind/doc #'hm/cyclic-group))


(def v4_l20 (hm/cyclic-group 5))


(deftest t5_l22 (is ((fn [v] (= (hm/order v) 5)) v4_l20)))


(def v6_l24 (kind/doc #'hm/symmetric-group))


(def v7_l26 (hm/symmetric-group 3))


(deftest t8_l28 (is ((fn [v] (= (hm/order v) 6)) v7_l26)))


(def v9_l30 (kind/doc #'hm/dihedral-group))


(def v10_l32 (hm/dihedral-group 4))


(deftest t11_l34 (is ((fn [v] (= (hm/order v) 8)) v10_l32)))


(def v12_l36 (kind/doc #'hm/product-group))


(def v13_l38 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3)))


(deftest t14_l40 (is ((fn [v] (= (hm/order v) 6)) v13_l38)))


(def v16_l44 (kind/doc #'hm/op))


(def v17_l46 (hm/op (hm/cyclic-group 7) 3 5))


(deftest t18_l48 (is (= v17_l46 1)))


(def v19_l50 (hm/op (hm/symmetric-group 3) [1 2 0] [0 2 1]))


(deftest t20_l52 (is (= v19_l50 [1 0 2])))


(def v21_l54 (kind/doc #'hm/inv))


(def v22_l56 (hm/inv (hm/cyclic-group 7) 3))


(deftest t23_l58 (is (= v22_l56 4)))


(def v24_l60 (hm/inv (hm/symmetric-group 3) [1 2 0]))


(deftest t25_l62 (is (= v24_l60 [2 0 1])))


(def v26_l64 (kind/doc #'hm/id))


(def v27_l66 (hm/id (hm/cyclic-group 5)))


(deftest t28_l68 (is (= v27_l66 0)))


(def v29_l70 (hm/id (hm/symmetric-group 3)))


(deftest t30_l72 (is (= v29_l70 [0 1 2])))


(def v31_l74 (hm/id (hm/dihedral-group 4)))


(deftest t32_l76 (is (= v31_l74 [:r 0])))


(def v33_l78 (kind/doc #'hm/elements))


(def v34_l80 (vec (hm/elements (hm/cyclic-group 4))))


(deftest t35_l82 (is (= v34_l80 [0 1 2 3])))


(def v36_l84 (kind/doc #'hm/order))


(def v37_l86 (hm/order (hm/symmetric-group 4)))


(deftest t38_l88 (is (= v37_l86 24)))


(def v39_l90 (kind/doc #'hm/conjugacy-classes))


(def
 v40_l92
 (let
  [classes (hm/conjugacy-classes (hm/symmetric-group 3))]
  (mapv :size classes)))


(deftest t41_l95 (is (= v40_l92 [2 3 1])))


(def v43_l99 (kind/doc #'hm/cycles))


(def v44_l101 (hm/cycles [1 2 3 0]))


(deftest t45_l103 (is (= v44_l101 [[0 1 2 3]])))


(def v46_l105 (hm/cycles [1 0 3 2]))


(deftest t47_l107 (is (= v46_l105 [[0 1] [2 3]])))


(def v48_l109 (kind/doc #'hm/cycle-type))


(def v49_l111 (hm/cycle-type [1 0 3 2]))


(deftest t50_l113 (is (= v49_l111 [2 2])))


(def v51_l115 (kind/doc #'hm/sign))


(def v52_l117 (hm/sign [1 0 2 3]))


(deftest t53_l119 (is (= v52_l117 -1)))


(def v54_l121 (hm/sign [0 1 2 3]))


(deftest t55_l123 (is (= v54_l121 1)))


(def v56_l125 (kind/doc #'hm/identity-perm))


(def v57_l127 (hm/identity-perm 4))


(deftest t58_l129 (is (= v57_l127 [0 1 2 3])))


(def v59_l131 (kind/doc #'hm/transposition))


(def v60_l133 (hm/transposition 5 1 3))


(deftest t61_l135 (is (= v60_l133 [0 3 2 1 4])))


(def v62_l137 (kind/doc #'hm/adjacent-transposition-decomposition))


(def v63_l139 (hm/adjacent-transposition-decomposition [2 0 1]))


(deftest t64_l141 (is (vector? v63_l139)))


(def v66_l145 (kind/doc #'hm/partitions))


(def v67_l147 (hm/partitions 4))


(deftest t68_l149 (is (= v67_l147 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v69_l151 (kind/doc #'hm/partition-conjugate))


(def v70_l153 (hm/partition-conjugate [4 2 1]))


(deftest t71_l155 (is (= v70_l153 [3 2 1 1])))


(def v73_l159 (kind/doc #'hm/standard-young-tableaux))


(def v74_l161 (hm/standard-young-tableaux [2 1]))


(deftest t75_l163 (is (= v74_l161 [[[1 2] [3]] [[1 3] [2]]])))


(def v76_l165 (kind/doc #'hm/hook-length-dimension))


(def v77_l167 (hm/hook-length-dimension [3 2]))


(deftest t78_l169 (is (= v77_l167 5)))


(def v79_l171 (hm/hook-length-dimension [2 2 1]))


(deftest t80_l173 (is (= v79_l171 5)))


(def v82_l177 (kind/doc #'hm/character-table))


(def
 v83_l179
 (let
  [ct (hm/character-table (hm/cyclic-group 3))]
  (count (:table ct))))


(deftest t84_l182 (is (= v83_l179 3)))


(def
 v85_l184
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__105941#] (long (Math/round (c/re p1__105941#))))
      row))
    (:table ct))]
  re-table))


(deftest t86_l189 (is (= v85_l184 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def v87_l191 (kind/doc #'hm/character-inner-product))


(def
 v88_l193
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   {:keys [table class-sizes]}
   ct
   order
   (hm/order (:group ct))]
  (c/re
   (hm/character-inner-product
    (nth table 0)
    (nth table 1)
    class-sizes
    order))))


(deftest t89_l198 (is ((fn [v] (< (Math/abs v) 1.0E-10)) v88_l193)))


(def v91_l202 (kind/doc #'hm/irrep))


(def v92_l204 (let [ir (hm/irrep [2 1])] (hm/rep-dimension ir)))


(deftest t93_l207 (is (= v92_l204 2)))


(def v94_l209 (kind/doc #'hm/rep-matrix))


(def
 v95_l211
 (let [ir (hm/irrep [2 1])] (fm/nrow (hm/rep-matrix ir [1 0 2]))))


(deftest t96_l214 (is (= v95_l211 2)))


(def v97_l216 (kind/doc #'hm/rep-dimension))


(def v98_l218 (hm/rep-dimension (hm/irrep [3 1])))


(deftest t99_l220 (is (= v98_l218 3)))


(def v100_l222 (kind/doc #'hm/rep-character))


(def
 v101_l224
 (let [ir (hm/irrep [2 1])] (hm/rep-character ir [0 1 2])))


(deftest
 t102_l227
 (is ((fn [v] (< (Math/abs (- v 2.0)) 1.0E-10)) v101_l224)))


(def v103_l229 (kind/doc #'hm/rep-generators))


(def
 v104_l231
 (let [ir (hm/irrep [2 1])] (count (hm/rep-generators ir))))


(deftest t105_l234 (is (= v104_l231 2)))


(def v106_l236 (kind/doc #'hm/tensor-product))


(def
 v107_l238
 (let
  [ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [2 1])
   tp
   (hm/tensor-product ir1 ir2)]
  (hm/rep-dimension tp)))


(deftest t108_l243 (is (= v107_l238 4)))


(def v109_l245 (kind/doc #'hm/direct-sum))


(def
 v110_l247
 (let
  [ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [1 1 1])
   ds
   (hm/direct-sum ir1 ir2)]
  (hm/rep-dimension ds)))


(deftest t111_l252 (is (= v110_l247 3)))


(def v112_l254 (kind/doc #'hm/frobenius-norm-sq))


(def
 v113_l256
 (let
  [M (fm/rows->mat [[1.0 0.0] [0.0 1.0]])]
  (hm/frobenius-norm-sq M)))


(deftest
 t114_l259
 (is ((fn [v] (< (Math/abs (- v 2.0)) 1.0E-10)) v113_l256)))


(def v115_l261 (kind/doc #'hm/frobenius-norm))


(def
 v116_l263
 (let [M (fm/rows->mat [[3.0 0.0] [0.0 4.0]])] (hm/frobenius-norm M)))


(deftest
 t117_l266
 (is ((fn [v] (< (Math/abs (- v 5.0)) 1.0E-10)) v116_l263)))


(def v118_l268 (kind/doc #'hm/matrix-fourier-transform))


(def
 v119_l270
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


(deftest t120_l276 (is (= v119_l270 2)))


(def v122_l280 (kind/doc #'hm/rising-sequences))


(def v123_l282 (hm/rising-sequences [0 1 2 3]))


(deftest t124_l284 (is (= v123_l282 1)))


(def v125_l286 (hm/rising-sequences [3 2 1 0]))


(deftest t126_l288 (is (= v125_l286 4)))


(def v127_l290 (kind/doc #'hm/gsr-probability))


(def v128_l292 (let [p (hm/gsr-probability [0 1 2 3] 1)] (> p 0.0)))


(deftest t129_l295 (is (true? v128_l292)))


(def v131_l299 (kind/doc #'hm/orbit))


(def
 v132_l301
 (let
  [G (hm/cyclic-group 4) act (fn [g x] (mod (+ (long g) (long x)) 4))]
  (hm/orbit G act 0)))


(deftest t133_l305 (is (= v132_l301 #{0 1 3 2})))


(def v134_l307 (kind/doc #'hm/orbits))


(def
 v135_l309
 (let
  [G (hm/cyclic-group 3) act (fn [g x] (mod (+ (long g) (long x)) 3))]
  (count (hm/orbits G act (range 3)))))


(deftest t136_l313 (is (= v135_l309 1)))


(def v137_l315 (kind/doc #'hm/fixed-points))


(def
 v138_l317
 (let
  [act (fn [g x] (mod (+ (long g) (long x)) 5))]
  (hm/fixed-points act 0 (range 5))))


(deftest t139_l320 (is (= v138_l317 #{0 1 4 3 2})))


(def v140_l322 (kind/doc #'hm/stabilizer))


(def
 v141_l324
 (let
  [G (hm/cyclic-group 4) act (fn [g x] (mod (+ (long g) (long x)) 4))]
  (hm/stabilizer G act 0)))


(deftest t142_l328 (is (= v141_l324 #{0})))


(def v143_l330 (kind/doc #'hm/burnside-count))


(def
 v144_l332
 (let
  [G
   (hm/cyclic-group 4)
   act
   (fn
    [g coloring]
    (mapv
     (fn* [p1__105942#] (coloring (mod (+ p1__105942# (long g)) 4)))
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
  (hm/burnside-count G act domain)))


(deftest t145_l340 (is (= v144_l332 6)))


(def v146_l342 (kind/doc #'hm/cycle-index))


(def
 v147_l344
 (let
  [G
   (hm/cyclic-group 3)
   act
   (fn [g x] (mod (+ (long g) (long x)) 3))
   ci
   (hm/cycle-index G act (range 3))]
  (= 1 (reduce + (vals ci)))))


(deftest t148_l349 (is (true? v147_l344)))


(def v149_l351 (kind/doc #'hm/polya-count))


(def
 v150_l353
 (let
  [G
   (hm/cyclic-group 4)
   act
   (fn [g x] (mod (+ (long g) (long x)) 4))
   ci
   (hm/cycle-index G act (range 4))]
  (hm/polya-count ci 2)))


(deftest t151_l358 (is (= v150_l353 6)))


(def v152_l360 (kind/doc #'hm/subset-action))


(def
 v153_l362
 (let
  [perm-act
   (fn [sigma x] (sigma x))
   {:keys [domain]}
   (hm/subset-action perm-act (range 4) 2)]
  (count domain)))


(deftest t154_l366 (is (= v153_l362 6)))


(def v156_l370 (kind/doc #'hm/fourier-transform))


(def
 v157_l372
 (let
  [ct
   (hm/character-table (hm/cyclic-group 4))
   f
   (mapv
    (fn* [p1__105943#] (c/complex (double p1__105943#) 0.0))
    [1 0 0 0])
   fhat
   (hm/fourier-transform ct f)]
  (count fhat)))


(deftest t158_l377 (is (= v157_l372 4)))


(def v159_l379 (kind/doc #'hm/inverse-fourier-transform))


(def
 v160_l381
 (let
  [ct
   (hm/character-table (hm/cyclic-group 4))
   f
   (mapv
    (fn* [p1__105944#] (c/complex (double p1__105944#) 0.0))
    [1 2 3 4])
   fhat
   (hm/fourier-transform ct f)
   f-back
   (hm/inverse-fourier-transform ct fhat)
   max-err
   (apply
    max
    (map
     (fn*
      [p1__105945# p2__105946#]
      (c/abs (c/sub p1__105945# p2__105946#)))
     f
     f-back))]
  (< max-err 1.0E-10)))


(deftest t161_l388 (is (true? v160_l381)))


(def v162_l390 (kind/doc #'hm/convolve))


(def
 v163_l392
 (let
  [ct
   (hm/character-table (hm/cyclic-group 4))
   f
   (mapv
    (fn* [p1__105947#] (c/complex (double p1__105947#) 0.0))
    [1 0 0 0])
   g
   (mapv
    (fn* [p1__105948#] (c/complex (double p1__105948#) 0.0))
    [0 1 0 0])
   conv
   (hm/convolve ct f g)]
  (long (Math/round (c/re (nth conv 1))))))


(deftest t164_l398 (is (= v163_l392 1)))


(def v165_l400 (kind/doc #'hm/total-variation-distance))


(def
 v166_l402
 (hm/total-variation-distance [0.5 0.5 0.0 0.0] [0.25 0.25 0.25 0.25]))


(deftest
 t167_l404
 (is ((fn [v] (< (Math/abs (- v 0.5)) 1.0E-10)) v166_l402)))


(def v169_l409 (kind/doc #'hm/young-diagram-svg))


(def v170_l411 (kind/hiccup (hm/young-diagram-svg [4 2 1])))


(def v171_l413 (kind/doc #'hm/young-hooks-svg))


(def v172_l415 (kind/hiccup (hm/young-hooks-svg [4 2 1])))


(def v173_l417 (kind/doc #'hm/syt-svg))


(def v174_l419 (kind/hiccup (hm/syt-svg [[1 2 3 4] [5 6] [7]])))


(def v175_l421 (kind/doc #'hm/cycle-diagram-svg))


(def v176_l423 (kind/hiccup (hm/cycle-diagram-svg [1 2 3 0])))


(def v177_l425 (kind/doc #'hm/cayley-table-svg))


(def v178_l427 (kind/hiccup (hm/cayley-table-svg (hm/cyclic-group 4))))
