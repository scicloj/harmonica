(ns
 harmonica-book.api-reference-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [scicloj.lalinea.linalg :as la]
  [scicloj.harmonica.protocols :as p]
  [scicloj.harmonica.analysis.representations :as rep]
  [fastmath.matrix :as fm]
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
     (mapv (fn* [p1__90103#] (Math/round (el/re p1__90103#))) row))
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
  (el/re
   (hm/character-inner-product
    (nth table 0)
    (nth table 1)
    class-sizes
    order))))


(deftest t89_l201 (is ((fn [v] (< (Math/abs v) 1.0E-10)) v88_l196)))


(def v90_l203 (kind/doc #'hm/format-cx))


(def v91_l205 (hm/format-cx (t/complex 0 1)))


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


(deftest t141_l326 (is (= v140_l322 1)))


(def v142_l328 (kind/doc #'hm/fixed-points))


(def
 v143_l330
 (let
  [act (fn [g x] (mod (+ g x) 5))]
  (hm/fixed-points act 0 (range 5))))


(deftest t144_l333 (is (= v143_l330 #{0 1 4 3 2})))


(def v145_l335 (kind/doc #'hm/stabilizer))


(def
 v146_l337
 (let
  [G (hm/cyclic-group 4) act (fn [g x] (mod (+ g x) 4))]
  (hm/stabilizer G act 0)))


(deftest t147_l341 (is (= v146_l337 #{0})))


(def v148_l343 (kind/doc #'hm/burnside-count))


(def
 v149_l345
 (let
  [G
   (hm/cyclic-group 4)
   act
   (fn
    [g coloring]
    (mapv
     (fn* [p1__90104#] (coloring (mod (+ p1__90104# g) 4)))
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


(deftest t150_l353 (is (= v149_l345 6)))


(def v151_l355 (kind/doc #'hm/cycle-index))


(def
 v152_l357
 (let
  [G
   (hm/cyclic-group 3)
   act
   (fn [g x] (mod (+ g x) 3))
   ci
   (hm/cycle-index G act (range 3))]
  (= 1 (reduce + (vals ci)))))


(deftest t153_l362 (is (true? v152_l357)))


(def v154_l364 (kind/doc #'hm/polya-count))


(def
 v155_l366
 (let
  [G
   (hm/cyclic-group 4)
   act
   (fn [g x] (mod (+ g x) 4))
   ci
   (hm/cycle-index G act (range 4))]
  (hm/polya-count ci 2)))


(deftest t156_l371 (is (= v155_l366 6)))


(def v157_l373 (kind/doc #'hm/subset-action))


(def
 v158_l375
 (let
  [perm-act
   (fn [sigma x] (sigma x))
   {:keys [domain]}
   (hm/subset-action perm-act (range 4) 2)]
  (count domain)))


(deftest t159_l379 (is (= v158_l375 6)))


(def v161_l383 (kind/doc #'hm/fourier-transform))


(def
 v162_l385
 (let
  [ct
   (hm/character-table (hm/cyclic-group 4))
   f
   (t/complex-tensor-real [1 0 0 0])
   fhat
   (hm/fourier-transform ct f)]
  (count fhat)))


(deftest t163_l390 (is (= v162_l385 4)))


(def v164_l392 (kind/doc #'hm/inverse-fourier-transform))


(def
 v165_l393
 (let
  [ct
   (hm/character-table (hm/cyclic-group 4))
   f
   (t/complex-tensor-real [1 2 3 4])
   fhat
   (hm/fourier-transform ct f)
   f-back
   (hm/inverse-fourier-transform ct fhat)
   max-err
   (apply max (vec (el/abs (el/- f-back f))))]
  (< max-err 1.0E-10)
  (< max-err 1.0E-10)))


(deftest t166_l401 (is (true? v165_l393)))


(def v167_l403 (kind/doc #'hm/convolve))


(def
 v168_l405
 (let
  [ct
   (hm/character-table (hm/cyclic-group 4))
   f
   (t/complex-tensor-real [1 0 0 0])
   g
   (t/complex-tensor-real [0 1 0 0])
   conv
   (hm/convolve ct f g)]
  (Math/round (el/re (conv 1)))))


(deftest t169_l410 (is (= v168_l405 1)))


(def v170_l412 (kind/doc #'hm/total-variation-distance))


(def
 v171_l414
 (hm/total-variation-distance [0.5 0.5 0.0 0.0] [0.25 0.25 0.25 0.25]))


(deftest
 t172_l416
 (is ((fn [v] (< (Math/abs (- v 0.5)) 1.0E-10)) v171_l414)))


(def v174_l420 (kind/doc #'hm/young-diagram-svg))


(def v175_l422 (kind/hiccup (hm/young-diagram-svg [4 2 1])))


(def v176_l424 (kind/doc #'hm/young-hooks-svg))


(def v177_l426 (kind/hiccup (hm/young-hooks-svg [4 2 1])))


(def v178_l428 (kind/doc #'hm/syt-svg))


(def v179_l430 (kind/hiccup (hm/syt-svg [[1 2 3 4] [5 6] [7]])))


(def v180_l432 (kind/doc #'hm/cycle-diagram-svg))


(def v181_l434 (kind/hiccup (hm/cycle-diagram-svg [1 2 3 0])))


(def v182_l436 (kind/doc #'hm/cayley-table-svg))


(def v183_l438 (kind/hiccup (hm/cayley-table-svg (hm/cyclic-group 4))))


(def v184_l440 (kind/doc #'hm/cayley-graph-svg))


(def
 v185_l442
 (kind/hiccup
  (hm/cayley-graph-svg
   (hm/symmetric-group 3)
   [[1 0 2] [0 2 1]]
   :radius
   100)))


(def v187_l452 (kind/doc #'t/complex-tensor))


(def v188_l454 (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0]))


(deftest
 t189_l456
 (is ((fn [v] (= [3] (t/complex-shape v))) v188_l454)))


(def v190_l458 (t/complex-tensor [[1.0 2.0] [3.0 4.0]]))


(deftest
 t191_l460
 (is ((fn [v] (= [2] (t/complex-shape v))) v190_l458)))


(def v192_l462 (kind/doc #'t/complex-tensor-real))


(def v193_l464 (t/complex-tensor-real [5.0 6.0 7.0]))


(deftest
 t194_l466
 (is ((fn [v] (= [0.0 0.0 0.0] (vec (el/im v)))) v193_l464)))


(def v196_l470 (kind/doc #'el/re))


(def v197_l472 (vec (el/re (t/complex-tensor [1.0 2.0] [3.0 4.0]))))


(deftest t198_l474 (is (= v197_l472 [1.0 2.0])))


(def v199_l476 (kind/doc #'el/im))


(def v200_l478 (vec (el/im (t/complex-tensor [1.0 2.0] [3.0 4.0]))))


(deftest t201_l480 (is (= v200_l478 [3.0 4.0])))


(def v203_l484 (kind/doc #'t/complex-shape))


(def
 v204_l486
 (t/complex-shape
  (t/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])))


(deftest t205_l489 (is (= v204_l486 [2 2])))


(def v206_l491 (kind/doc #'t/scalar?))


(def v207_l493 (t/scalar? (t/complex-tensor [3.0 4.0])))


(deftest t208_l495 (is (true? v207_l493)))


(def v209_l497 (t/scalar? (t/complex-tensor [1.0 2.0] [3.0 4.0])))


(deftest t210_l499 (is ((fn [v] (not v)) v209_l497)))


(def v211_l501 (kind/doc #'t/->tensor))


(def
 v212_l503
 (t/shape (t/->tensor (t/complex-tensor [1.0 2.0] [3.0 4.0]))))


(deftest t213_l505 (is (= v212_l503 [2 2])))


(def v214_l507 (kind/doc #'t/->double-array))


(def
 v215_l509
 (let
  [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
  (vec (t/->double-array ct))))


(deftest t216_l512 (is (= v215_l509 [1.0 3.0 2.0 4.0])))


(def v218_l516 (kind/doc #'el/*))


(def
 v220_l520
 (let
  [a
   (t/complex-tensor [1.0] [3.0])
   b
   (t/complex-tensor [5.0] [7.0])
   c
   (el/* a b)]
  [(el/re (c 0)) (el/im (c 0))]))


(deftest t221_l525 (is (= v220_l520 [-16.0 22.0])))


(def v222_l527 (kind/doc #'el/conj))


(def
 v223_l529
 (let
  [ct (el/conj (t/complex-tensor [1.0 2.0] [3.0 -4.0]))]
  (vec (el/im ct))))


(deftest t224_l532 (is (= v223_l529 [-3.0 4.0])))


(def v225_l534 (kind/doc #'el/scale))


(def
 v226_l536
 (let
  [ct (el/scale (t/complex-tensor [1.0 2.0] [3.0 4.0]) 2.0)]
  [(vec (el/re ct)) (vec (el/im ct))]))


(deftest t227_l539 (is (= v226_l536 [[2.0 4.0] [6.0 8.0]])))


(def v228_l541 (kind/doc #'el/abs))


(def
 v230_l545
 (let
  [m (el/abs (t/complex-tensor [3.0] [4.0]))]
  (< (Math/abs (- (double (m 0)) 5.0)) 1.0E-10)))


(deftest t231_l548 (is (true? v230_l545)))


(def v233_l552 (kind/doc #'la/dot))


(def
 v235_l556
 (let
  [a
   (t/complex-tensor [3.0 1.0] [4.0 2.0])
   d
   (la/dot a a)
   re
   (el/re d)
   im
   (el/im d)]
  (and (< (Math/abs (- re 30.0)) 1.0E-10) (< (Math/abs im) 1.0E-10))))


(deftest t236_l561 (is (true? v235_l556)))


(def v237_l563 (kind/doc #'la/dot-conj))


(def
 v239_l567
 (let
  [a
   (t/complex-tensor [3.0 1.0] [4.0 2.0])
   d
   (la/dot-conj a a)
   re
   (el/re d)
   im
   (el/im d)]
  (and (< (Math/abs (- re 30.0)) 1.0E-10) (< (Math/abs im) 1.0E-10))))


(deftest t240_l572 (is (true? v239_l567)))


(def
 v242_l576
 (let
  [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  [(count ct) (t/scalar? (ct 0)) (el/re (ct 1))]))


(deftest t243_l579 (is (= v242_l576 [3 true 2.0])))
