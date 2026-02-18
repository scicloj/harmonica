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
     (mapv
      (fn* [p1__90470#] (long (Math/round (cx/re p1__90470#))))
      row))
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


(def v91_l205 (kind/doc #'hm/irrep))


(def v92_l207 (let [ir (hm/irrep [2 1])] (hm/rep-dimension ir)))


(deftest t93_l210 (is (= v92_l207 2)))


(def v94_l212 (kind/doc #'hm/rep-matrix))


(def
 v95_l214
 (let [ir (hm/irrep [2 1])] (fm/nrow (hm/rep-matrix ir [1 0 2]))))


(deftest t96_l217 (is (= v95_l214 2)))


(def v97_l219 (kind/doc #'hm/rep-dimension))


(def v98_l221 (hm/rep-dimension (hm/irrep [3 1])))


(deftest t99_l223 (is (= v98_l221 3)))


(def v100_l225 (kind/doc #'hm/rep-character))


(def
 v101_l227
 (let [ir (hm/irrep [2 1])] (hm/rep-character ir [0 1 2])))


(deftest
 t102_l230
 (is ((fn [v] (< (Math/abs (- v 2.0)) 1.0E-10)) v101_l227)))


(def v103_l232 (kind/doc #'hm/rep-generators))


(def
 v104_l234
 (let [ir (hm/irrep [2 1])] (count (hm/rep-generators ir))))


(deftest t105_l237 (is (= v104_l234 2)))


(def v106_l239 (kind/doc #'hm/tensor-product))


(def
 v107_l241
 (let
  [ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [2 1])
   tp
   (hm/tensor-product ir1 ir2)]
  (hm/rep-dimension tp)))


(deftest t108_l246 (is (= v107_l241 4)))


(def v109_l248 (kind/doc #'hm/direct-sum))


(def
 v110_l250
 (let
  [ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [1 1 1])
   ds
   (hm/direct-sum ir1 ir2)]
  (hm/rep-dimension ds)))


(deftest t111_l255 (is (= v110_l250 3)))


(def v112_l257 (kind/doc #'hm/frobenius-norm-sq))


(def
 v113_l259
 (let
  [M (fm/rows->mat [[1.0 0.0] [0.0 1.0]])]
  (hm/frobenius-norm-sq M)))


(deftest
 t114_l262
 (is ((fn [v] (< (Math/abs (- v 2.0)) 1.0E-10)) v113_l259)))


(def v115_l264 (kind/doc #'hm/frobenius-norm))


(def
 v116_l266
 (let [M (fm/rows->mat [[3.0 0.0] [0.0 4.0]])] (hm/frobenius-norm M)))


(deftest
 t117_l269
 (is ((fn [v] (< (Math/abs (- v 5.0)) 1.0E-10)) v116_l266)))


(def v118_l271 (kind/doc #'hm/matrix-fourier-transform))


(def
 v119_l273
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


(deftest t120_l279 (is (= v119_l273 2)))


(def v122_l283 (kind/doc #'hm/rising-sequences))


(def v123_l285 (hm/rising-sequences [0 1 2 3]))


(deftest t124_l287 (is (= v123_l285 1)))


(def v125_l289 (hm/rising-sequences [3 2 1 0]))


(deftest t126_l291 (is (= v125_l289 4)))


(def v127_l293 (kind/doc #'hm/gsr-probability))


(def v128_l295 (let [p (hm/gsr-probability [0 1 2 3] 1)] (> p 0.0)))


(deftest t129_l298 (is (true? v128_l295)))


(def v131_l302 (kind/doc #'hm/orbit))


(def
 v132_l304
 (let
  [G (hm/cyclic-group 4) act (fn [g x] (mod (+ (long g) (long x)) 4))]
  (hm/orbit G act 0)))


(deftest t133_l308 (is (= v132_l304 #{0 1 3 2})))


(def v134_l310 (kind/doc #'hm/orbits))


(def
 v135_l312
 (let
  [G (hm/cyclic-group 3) act (fn [g x] (mod (+ (long g) (long x)) 3))]
  (count (hm/orbits G act (range 3)))))


(deftest t136_l316 (is (= v135_l312 1)))


(def v137_l318 (kind/doc #'hm/fixed-points))


(def
 v138_l320
 (let
  [act (fn [g x] (mod (+ (long g) (long x)) 5))]
  (hm/fixed-points act 0 (range 5))))


(deftest t139_l323 (is (= v138_l320 #{0 1 4 3 2})))


(def v140_l325 (kind/doc #'hm/stabilizer))


(def
 v141_l327
 (let
  [G (hm/cyclic-group 4) act (fn [g x] (mod (+ (long g) (long x)) 4))]
  (hm/stabilizer G act 0)))


(deftest t142_l331 (is (= v141_l327 #{0})))


(def v143_l333 (kind/doc #'hm/burnside-count))


(def
 v144_l335
 (let
  [G
   (hm/cyclic-group 4)
   act
   (fn
    [g coloring]
    (mapv
     (fn* [p1__90471#] (coloring (mod (+ p1__90471# (long g)) 4)))
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


(deftest t145_l343 (is (= v144_l335 6)))


(def v146_l345 (kind/doc #'hm/cycle-index))


(def
 v147_l347
 (let
  [G
   (hm/cyclic-group 3)
   act
   (fn [g x] (mod (+ (long g) (long x)) 3))
   ci
   (hm/cycle-index G act (range 3))]
  (= 1 (reduce + (vals ci)))))


(deftest t148_l352 (is (true? v147_l347)))


(def v149_l354 (kind/doc #'hm/polya-count))


(def
 v150_l356
 (let
  [G
   (hm/cyclic-group 4)
   act
   (fn [g x] (mod (+ (long g) (long x)) 4))
   ci
   (hm/cycle-index G act (range 4))]
  (hm/polya-count ci 2)))


(deftest t151_l361 (is (= v150_l356 6)))


(def v152_l363 (kind/doc #'hm/subset-action))


(def
 v153_l365
 (let
  [perm-act
   (fn [sigma x] (sigma x))
   {:keys [domain]}
   (hm/subset-action perm-act (range 4) 2)]
  (count domain)))


(deftest t154_l369 (is (= v153_l365 6)))


(def v156_l373 (kind/doc #'hm/fourier-transform))


(def
 v157_l375
 (let
  [ct
   (hm/character-table (hm/cyclic-group 4))
   f
   (cx/complex-tensor-real [1 0 0 0])
   fhat
   (hm/fourier-transform ct f)]
  (count fhat)))


(deftest t158_l380 (is (= v157_l375 4)))


(def v159_l382 (kind/doc #'hm/inverse-fourier-transform))


(def
 v160_l383
 (let
  [ct
   (hm/character-table (hm/cyclic-group 4))
   f
   (cx/complex-tensor-real [1 2 3 4])
   fhat
   (hm/fourier-transform ct f)
   f-back
   (hm/inverse-fourier-transform ct fhat)
   max-err
   (apply max (vec (cx/cabs (cx/csub f-back f))))]
  (< max-err 1.0E-10)
  (< max-err 1.0E-10)))


(deftest t161_l391 (is (true? v160_l383)))


(def v162_l393 (kind/doc #'hm/convolve))


(def
 v163_l395
 (let
  [ct
   (hm/character-table (hm/cyclic-group 4))
   f
   (cx/complex-tensor-real [1 0 0 0])
   g
   (cx/complex-tensor-real [0 1 0 0])
   conv
   (hm/convolve ct f g)]
  (long (Math/round (cx/re (conv 1))))))


(deftest t164_l400 (is (= v163_l395 1)))


(def v165_l402 (kind/doc #'hm/total-variation-distance))


(def
 v166_l404
 (hm/total-variation-distance [0.5 0.5 0.0 0.0] [0.25 0.25 0.25 0.25]))


(deftest
 t167_l406
 (is ((fn [v] (< (Math/abs (- v 0.5)) 1.0E-10)) v166_l404)))


(def v169_l410 (kind/doc #'hm/young-diagram-svg))


(def v170_l412 (kind/hiccup (hm/young-diagram-svg [4 2 1])))


(def v171_l414 (kind/doc #'hm/young-hooks-svg))


(def v172_l416 (kind/hiccup (hm/young-hooks-svg [4 2 1])))


(def v173_l418 (kind/doc #'hm/syt-svg))


(def v174_l420 (kind/hiccup (hm/syt-svg [[1 2 3 4] [5 6] [7]])))


(def v175_l422 (kind/doc #'hm/cycle-diagram-svg))


(def v176_l424 (kind/hiccup (hm/cycle-diagram-svg [1 2 3 0])))


(def v177_l426 (kind/doc #'hm/cayley-table-svg))


(def v178_l428 (kind/hiccup (hm/cayley-table-svg (hm/cyclic-group 4))))


(def v179_l430 (kind/doc #'hm/cayley-graph-svg))


(def
 v180_l432
 (kind/hiccup
  (hm/cayley-graph-svg
   (hm/symmetric-group 3)
   [[1 0 2] [0 2 1]]
   :radius
   100)))


(def v182_l442 (kind/doc #'cx/complex-tensor))


(def v183_l444 (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0]))


(deftest
 t184_l446
 (is ((fn [v] (= [3] (cx/complex-shape v))) v183_l444)))


(def
 v185_l448
 (cx/complex-tensor (tensor/->tensor [[1.0 2.0] [3.0 4.0]])))


(deftest
 t186_l450
 (is ((fn [v] (= [2] (cx/complex-shape v))) v185_l448)))


(def v187_l452 (kind/doc #'cx/complex-tensor-real))


(def v188_l454 (cx/complex-tensor-real [5.0 6.0 7.0]))


(deftest
 t189_l456
 (is ((fn [v] (= [0.0 0.0 0.0] (vec (cx/im v)))) v188_l454)))


(def v191_l460 (kind/doc #'cx/re))


(def v192_l462 (vec (cx/re (cx/complex-tensor [1.0 2.0] [3.0 4.0]))))


(deftest t193_l464 (is (= v192_l462 [1.0 2.0])))


(def v194_l466 (kind/doc #'cx/im))


(def v195_l468 (vec (cx/im (cx/complex-tensor [1.0 2.0] [3.0 4.0]))))


(deftest t196_l470 (is (= v195_l468 [3.0 4.0])))


(def v198_l474 (kind/doc #'cx/complex-shape))


(def
 v199_l476
 (cx/complex-shape
  (cx/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])))


(deftest t200_l479 (is (= v199_l476 [2 2])))


(def v201_l481 (kind/doc #'cx/scalar?))


(def
 v202_l483
 (cx/scalar? (cx/complex-tensor (tensor/->tensor [3.0 4.0]))))


(deftest t203_l485 (is (true? v202_l483)))


(def v204_l487 (cx/scalar? (cx/complex-tensor [1.0 2.0] [3.0 4.0])))


(deftest t205_l489 (is ((fn [v] (not v)) v204_l487)))


(def v206_l491 (kind/doc #'cx/->tensor))


(def
 v207_l493
 (vec
  (dtype/shape (cx/->tensor (cx/complex-tensor [1.0 2.0] [3.0 4.0])))))


(deftest t208_l495 (is (= v207_l493 [2 2])))


(def v209_l497 (kind/doc #'cx/->double-array))


(def
 v210_l499
 (let
  [ct (cx/complex-tensor [1.0 2.0] [3.0 4.0])]
  (identical? (cx/->double-array ct) (cx/->double-array ct))))


(deftest t211_l502 (is (true? v210_l499)))


(def v213_l506 (kind/doc #'cx/cmul))


(def
 v215_l510
 (let
  [a
   (cx/complex-tensor [1.0] [3.0])
   b
   (cx/complex-tensor [5.0] [7.0])
   c
   (cx/cmul a b)]
  [(cx/re (c 0)) (cx/im (c 0))]))


(deftest t216_l515 (is (= v215_l510 [-16.0 22.0])))


(def v217_l517 (kind/doc #'cx/cconj))


(def
 v218_l519
 (let
  [ct (cx/cconj (cx/complex-tensor [1.0 2.0] [3.0 -4.0]))]
  (vec (cx/im ct))))


(deftest t219_l522 (is (= v218_l519 [-3.0 4.0])))


(def v220_l524 (kind/doc #'cx/cscale))


(def
 v221_l526
 (let
  [ct (cx/cscale (cx/complex-tensor [1.0 2.0] [3.0 4.0]) 2.0)]
  [(vec (cx/re ct)) (vec (cx/im ct))]))


(deftest t222_l529 (is (= v221_l526 [[2.0 4.0] [6.0 8.0]])))


(def v223_l531 (kind/doc #'cx/cabs))


(def
 v225_l535
 (let
  [m (cx/cabs (cx/complex-tensor [3.0] [4.0]))]
  (< (Math/abs (- (double (m 0)) 5.0)) 1.0E-10)))


(deftest t226_l538 (is (true? v225_l535)))


(def v228_l542 (kind/doc #'cx/cdot))


(def
 v230_l546
 (let
  [a
   (cx/complex-tensor [1.0 0.0] [0.0 1.0])
   b
   (cx/complex-tensor [0.0 1.0] [1.0 0.0])
   [re im]
   (cx/cdot a b)]
  (and (< (Math/abs re) 1.0E-10) (< (Math/abs (- im 2.0)) 1.0E-10))))


(deftest t231_l552 (is (true? v230_l546)))


(def v232_l554 (kind/doc #'cx/cdot-conj))


(def
 v234_l558
 (let
  [a
   (cx/complex-tensor [3.0 1.0] [4.0 2.0])
   [re im]
   (cx/cdot-conj a a)]
  (and (< (Math/abs (- re 30.0)) 1.0E-10) (< (Math/abs im) 1.0E-10))))


(deftest t235_l563 (is (true? v234_l558)))


(def
 v237_l567
 (let
  [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  [(count ct) (cx/scalar? (ct 0)) (cx/re (ct 1))]))


(deftest t238_l570 (is (= v237_l567 [3 true 2.0])))
