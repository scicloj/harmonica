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
  [tech.v3.tensor :as tensor]
  [tech.v3.datatype :as dtype]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l23 (kind/doc #'hm/cyclic-group))


(def v4_l25 (hm/cyclic-group 5))


(deftest t5_l27 (is ((fn [v] (= (hm/order v) 5)) v4_l25)))


(def v6_l29 (kind/doc #'hm/symmetric-group))


(def v7_l31 (hm/symmetric-group 3))


(deftest t8_l33 (is ((fn [v] (= (hm/order v) 6)) v7_l31)))


(def v9_l35 (kind/doc #'hm/dihedral-group))


(def v10_l37 (hm/dihedral-group 4))


(deftest t11_l39 (is ((fn [v] (= (hm/order v) 8)) v10_l37)))


(def v12_l41 (kind/doc #'hm/product-group))


(def v13_l43 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3)))


(deftest t14_l45 (is ((fn [v] (= (hm/order v) 6)) v13_l43)))


(def v16_l49 (kind/doc #'hm/op))


(def v17_l51 (hm/op (hm/cyclic-group 7) 3 5))


(deftest t18_l53 (is (= v17_l51 1)))


(def v19_l55 (hm/op (hm/symmetric-group 3) [1 2 0] [0 2 1]))


(deftest t20_l57 (is (= v19_l55 [1 0 2])))


(def v21_l59 (kind/doc #'hm/inv))


(def v22_l61 (hm/inv (hm/cyclic-group 7) 3))


(deftest t23_l63 (is (= v22_l61 4)))


(def v24_l65 (hm/inv (hm/symmetric-group 3) [1 2 0]))


(deftest t25_l67 (is (= v24_l65 [2 0 1])))


(def v26_l69 (kind/doc #'hm/id))


(def v27_l71 (hm/id (hm/cyclic-group 5)))


(deftest t28_l73 (is (= v27_l71 0)))


(def v29_l75 (hm/id (hm/symmetric-group 3)))


(deftest t30_l77 (is (= v29_l75 [0 1 2])))


(def v31_l79 (hm/id (hm/dihedral-group 4)))


(deftest t32_l81 (is (= v31_l79 [:r 0])))


(def v33_l83 (kind/doc #'hm/elements))


(def v34_l85 (vec (hm/elements (hm/cyclic-group 4))))


(deftest t35_l87 (is (= v34_l85 [0 1 2 3])))


(def v36_l89 (kind/doc #'hm/order))


(def v37_l91 (hm/order (hm/symmetric-group 4)))


(deftest t38_l93 (is (= v37_l91 24)))


(def v39_l95 (kind/doc #'hm/conjugacy-classes))


(def
 v40_l97
 (let
  [classes (hm/conjugacy-classes (hm/symmetric-group 3))]
  (mapv :size classes)))


(deftest t41_l100 (is (= v40_l97 [2 3 1])))


(def v43_l104 (kind/doc #'hm/cycles))


(def v44_l106 (hm/cycles [1 2 3 0]))


(deftest t45_l108 (is (= v44_l106 [[0 1 2 3]])))


(def v46_l110 (hm/cycles [1 0 3 2]))


(deftest t47_l112 (is (= v46_l110 [[0 1] [2 3]])))


(def v48_l114 (kind/doc #'hm/cycle-type))


(def v49_l116 (hm/cycle-type [1 0 3 2]))


(deftest t50_l118 (is (= v49_l116 [2 2])))


(def v51_l120 (kind/doc #'hm/sign))


(def v52_l122 (hm/sign [1 0 2 3]))


(deftest t53_l124 (is (= v52_l122 -1)))


(def v54_l126 (hm/sign [0 1 2 3]))


(deftest t55_l128 (is (= v54_l126 1)))


(def v56_l130 (kind/doc #'hm/identity-perm))


(def v57_l132 (hm/identity-perm 4))


(deftest t58_l134 (is (= v57_l132 [0 1 2 3])))


(def v59_l136 (kind/doc #'hm/transposition))


(def v60_l138 (hm/transposition 5 1 3))


(deftest t61_l140 (is (= v60_l138 [0 3 2 1 4])))


(def v62_l142 (kind/doc #'hm/adjacent-transposition-decomposition))


(def v63_l144 (hm/adjacent-transposition-decomposition [2 0 1]))


(deftest t64_l146 (is (vector? v63_l144)))


(def v66_l150 (kind/doc #'hm/partitions))


(def v67_l152 (hm/partitions 4))


(deftest t68_l154 (is (= v67_l152 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v69_l156 (kind/doc #'hm/partition-conjugate))


(def v70_l158 (hm/partition-conjugate [4 2 1]))


(deftest t71_l160 (is (= v70_l158 [3 2 1 1])))


(def v73_l164 (kind/doc #'hm/standard-young-tableaux))


(def v74_l166 (hm/standard-young-tableaux [2 1]))


(deftest t75_l168 (is (= v74_l166 [[[1 2] [3]] [[1 3] [2]]])))


(def v76_l170 (kind/doc #'hm/hook-length-dimension))


(def v77_l172 (hm/hook-length-dimension [3 2]))


(deftest t78_l174 (is (= v77_l172 5)))


(def v79_l176 (hm/hook-length-dimension [2 2 1]))


(deftest t80_l178 (is (= v79_l176 5)))


(def v82_l182 (kind/doc #'hm/character-table))


(def
 v83_l184
 (let
  [ct (hm/character-table (hm/cyclic-group 3))]
  (count (:table ct))))


(deftest t84_l187 (is (= v83_l184 3)))


(def
 v85_l189
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv (fn* [p1__85286#] (Math/round (el/re p1__85286#))) row))
    (:table ct))]
  re-table))


(deftest t86_l194 (is (= v85_l189 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def v87_l196 (kind/doc #'hm/character-inner-product))


(def
 v88_l198
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


(deftest t89_l203 (is ((fn [v] (< (Math/abs v) 1.0E-10)) v88_l198)))


(def v90_l205 (kind/doc #'hm/format-cx))


(def v91_l207 (hm/format-cx (t/complex 0 1)))


(deftest t92_l209 (is (= v91_l207 "i")))


(def v93_l211 (kind/doc #'hm/show-character-table))


(def
 v94_l213
 (hm/show-character-table (hm/character-table (hm/symmetric-group 3))))


(def v96_l217 (kind/doc #'hm/irrep))


(def v97_l219 (let [ir (hm/irrep [2 1])] (hm/rep-dimension ir)))


(deftest t98_l222 (is (= v97_l219 2)))


(def v99_l224 (kind/doc #'hm/rep-matrix))


(def
 v100_l226
 (let [ir (hm/irrep [2 1])] (fm/nrow (hm/rep-matrix ir [1 0 2]))))


(deftest t101_l229 (is (= v100_l226 2)))


(def v102_l231 (kind/doc #'hm/rep-dimension))


(def v103_l233 (hm/rep-dimension (hm/irrep [3 1])))


(deftest t104_l235 (is (= v103_l233 3)))


(def v105_l237 (kind/doc #'hm/rep-character))


(def
 v106_l239
 (let [ir (hm/irrep [2 1])] (hm/rep-character ir [0 1 2])))


(deftest
 t107_l242
 (is ((fn [v] (< (Math/abs (- v 2.0)) 1.0E-10)) v106_l239)))


(def v108_l244 (kind/doc #'hm/rep-generators))


(def
 v109_l246
 (let [ir (hm/irrep [2 1])] (count (hm/rep-generators ir))))


(deftest t110_l249 (is (= v109_l246 2)))


(def v111_l251 (kind/doc #'hm/tensor-product))


(def
 v112_l253
 (let
  [ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [2 1])
   tp
   (hm/tensor-product ir1 ir2)]
  (hm/rep-dimension tp)))


(deftest t113_l258 (is (= v112_l253 4)))


(def v114_l260 (kind/doc #'hm/direct-sum))


(def
 v115_l262
 (let
  [ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [1 1 1])
   ds
   (hm/direct-sum ir1 ir2)]
  (hm/rep-dimension ds)))


(deftest t116_l267 (is (= v115_l262 3)))


(def v117_l269 (kind/doc #'hm/frobenius-norm-sq))


(def
 v118_l271
 (let
  [M (fm/rows->mat [[1.0 0.0] [0.0 1.0]])]
  (hm/frobenius-norm-sq M)))


(deftest
 t119_l274
 (is ((fn [v] (< (Math/abs (- v 2.0)) 1.0E-10)) v118_l271)))


(def v120_l276 (kind/doc #'hm/frobenius-norm))


(def
 v121_l278
 (let [M (fm/rows->mat [[3.0 0.0] [0.0 4.0]])] (hm/frobenius-norm M)))


(deftest
 t122_l281
 (is ((fn [v] (< (Math/abs (- v 5.0)) 1.0E-10)) v121_l278)))


(def v123_l283 (kind/doc #'hm/matrix-fourier-transform))


(def
 v124_l285
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


(deftest t125_l291 (is (= v124_l285 2)))


(def v127_l295 (kind/doc #'hm/rising-sequences))


(def v128_l297 (hm/rising-sequences [0 1 2 3]))


(deftest t129_l299 (is (= v128_l297 1)))


(def v130_l301 (hm/rising-sequences [3 2 1 0]))


(deftest t131_l303 (is (= v130_l301 4)))


(def v132_l305 (kind/doc #'hm/gsr-probability))


(def v133_l307 (let [p (hm/gsr-probability [0 1 2 3] 1)] (> p 0.0)))


(deftest t134_l310 (is (true? v133_l307)))


(def v136_l314 (kind/doc #'hm/orbit))


(def
 v137_l316
 (let
  [G (hm/cyclic-group 4) act (fn [g x] (mod (+ g x) 4))]
  (hm/orbit G act 0)))


(deftest t138_l320 (is (= v137_l316 #{0 1 3 2})))


(def v139_l322 (kind/doc #'hm/orbits))


(def
 v140_l324
 (let
  [G (hm/cyclic-group 3) act (fn [g x] (mod (+ g x) 3))]
  (count (hm/orbits G act (range 3)))))


(deftest t141_l328 (is (= v140_l324 1)))


(def v142_l330 (kind/doc #'hm/fixed-points))


(def
 v143_l332
 (let
  [act (fn [g x] (mod (+ g x) 5))]
  (hm/fixed-points act 0 (range 5))))


(deftest t144_l335 (is (= v143_l332 #{0 1 4 3 2})))


(def v145_l337 (kind/doc #'hm/stabilizer))


(def
 v146_l339
 (let
  [G (hm/cyclic-group 4) act (fn [g x] (mod (+ g x) 4))]
  (hm/stabilizer G act 0)))


(deftest t147_l343 (is (= v146_l339 #{0})))


(def v148_l345 (kind/doc #'hm/burnside-count))


(def
 v149_l347
 (let
  [G
   (hm/cyclic-group 4)
   act
   (fn
    [g coloring]
    (mapv
     (fn* [p1__85287#] (coloring (mod (+ p1__85287# g) 4)))
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


(deftest t150_l355 (is (= v149_l347 6)))


(def v151_l357 (kind/doc #'hm/cycle-index))


(def
 v152_l359
 (let
  [G
   (hm/cyclic-group 3)
   act
   (fn [g x] (mod (+ g x) 3))
   ci
   (hm/cycle-index G act (range 3))]
  (= 1 (reduce + (vals ci)))))


(deftest t153_l364 (is (true? v152_l359)))


(def v154_l366 (kind/doc #'hm/polya-count))


(def
 v155_l368
 (let
  [G
   (hm/cyclic-group 4)
   act
   (fn [g x] (mod (+ g x) 4))
   ci
   (hm/cycle-index G act (range 4))]
  (hm/polya-count ci 2)))


(deftest t156_l373 (is (= v155_l368 6)))


(def v157_l375 (kind/doc #'hm/subset-action))


(def
 v158_l377
 (let
  [perm-act
   (fn [sigma x] (sigma x))
   {:keys [domain]}
   (hm/subset-action perm-act (range 4) 2)]
  (count domain)))


(deftest t159_l381 (is (= v158_l377 6)))


(def v161_l385 (kind/doc #'hm/fourier-transform))


(def
 v162_l387
 (let
  [ct
   (hm/character-table (hm/cyclic-group 4))
   f
   (t/complex-tensor-real [1 0 0 0])
   fhat
   (hm/fourier-transform ct f)]
  (count fhat)))


(deftest t163_l392 (is (= v162_l387 4)))


(def v164_l394 (kind/doc #'hm/inverse-fourier-transform))


(def
 v165_l395
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


(deftest t166_l403 (is (true? v165_l395)))


(def v167_l405 (kind/doc #'hm/convolve))


(def
 v168_l407
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


(deftest t169_l412 (is (= v168_l407 1)))


(def v170_l414 (kind/doc #'hm/total-variation-distance))


(def
 v171_l416
 (hm/total-variation-distance [0.5 0.5 0.0 0.0] [0.25 0.25 0.25 0.25]))


(deftest
 t172_l418
 (is ((fn [v] (< (Math/abs (- v 0.5)) 1.0E-10)) v171_l416)))


(def v174_l422 (kind/doc #'hm/young-diagram-svg))


(def v175_l424 (kind/hiccup (hm/young-diagram-svg [4 2 1])))


(def v176_l426 (kind/doc #'hm/young-hooks-svg))


(def v177_l428 (kind/hiccup (hm/young-hooks-svg [4 2 1])))


(def v178_l430 (kind/doc #'hm/syt-svg))


(def v179_l432 (kind/hiccup (hm/syt-svg [[1 2 3 4] [5 6] [7]])))


(def v180_l434 (kind/doc #'hm/cycle-diagram-svg))


(def v181_l436 (kind/hiccup (hm/cycle-diagram-svg [1 2 3 0])))


(def v182_l438 (kind/doc #'hm/cayley-table-svg))


(def v183_l440 (kind/hiccup (hm/cayley-table-svg (hm/cyclic-group 4))))


(def v184_l442 (kind/doc #'hm/cayley-graph-svg))


(def
 v185_l444
 (kind/hiccup
  (hm/cayley-graph-svg
   (hm/symmetric-group 3)
   [[1 0 2] [0 2 1]]
   :radius
   100)))


(def v187_l454 (kind/doc #'t/complex-tensor))


(def v188_l456 (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0]))


(deftest
 t189_l458
 (is ((fn [v] (= [3] (t/complex-shape v))) v188_l456)))


(def
 v190_l460
 (t/complex-tensor (tensor/->tensor [[1.0 2.0] [3.0 4.0]])))


(deftest
 t191_l462
 (is ((fn [v] (= [2] (t/complex-shape v))) v190_l460)))


(def v192_l464 (kind/doc #'t/complex-tensor-real))


(def v193_l466 (t/complex-tensor-real [5.0 6.0 7.0]))


(deftest
 t194_l468
 (is ((fn [v] (= [0.0 0.0 0.0] (vec (el/im v)))) v193_l466)))


(def v196_l472 (kind/doc #'el/re))


(def v197_l474 (vec (el/re (t/complex-tensor [1.0 2.0] [3.0 4.0]))))


(deftest t198_l476 (is (= v197_l474 [1.0 2.0])))


(def v199_l478 (kind/doc #'el/im))


(def v200_l480 (vec (el/im (t/complex-tensor [1.0 2.0] [3.0 4.0]))))


(deftest t201_l482 (is (= v200_l480 [3.0 4.0])))


(def v203_l486 (kind/doc #'t/complex-shape))


(def
 v204_l488
 (t/complex-shape
  (t/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])))


(deftest t205_l491 (is (= v204_l488 [2 2])))


(def v206_l493 (kind/doc #'t/scalar?))


(def
 v207_l495
 (t/scalar? (t/complex-tensor (tensor/->tensor [3.0 4.0]))))


(deftest t208_l497 (is (true? v207_l495)))


(def v209_l499 (t/scalar? (t/complex-tensor [1.0 2.0] [3.0 4.0])))


(deftest t210_l501 (is ((fn [v] (not v)) v209_l499)))


(def v211_l503 (kind/doc #'t/->tensor))


(def
 v212_l505
 (vec
  (dtype/shape (t/->tensor (t/complex-tensor [1.0 2.0] [3.0 4.0])))))


(deftest t213_l507 (is (= v212_l505 [2 2])))


(def v214_l509 (kind/doc #'t/->double-array))


(def
 v215_l511
 (let
  [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
  (vec (t/->double-array ct))))


(deftest t216_l514 (is (= v215_l511 [1.0 3.0 2.0 4.0])))


(def v218_l518 (kind/doc #'el/*))


(def
 v220_l522
 (let
  [a
   (t/complex-tensor [1.0] [3.0])
   b
   (t/complex-tensor [5.0] [7.0])
   c
   (el/* a b)]
  [(el/re (c 0)) (el/im (c 0))]))


(deftest t221_l527 (is (= v220_l522 [-16.0 22.0])))


(def v222_l529 (kind/doc #'el/conj))


(def
 v223_l531
 (let
  [ct (el/conj (t/complex-tensor [1.0 2.0] [3.0 -4.0]))]
  (vec (el/im ct))))


(deftest t224_l534 (is (= v223_l531 [-3.0 4.0])))


(def v225_l536 (kind/doc #'el/scale))


(def
 v226_l538
 (let
  [ct (el/scale (t/complex-tensor [1.0 2.0] [3.0 4.0]) 2.0)]
  [(vec (el/re ct)) (vec (el/im ct))]))


(deftest t227_l541 (is (= v226_l538 [[2.0 4.0] [6.0 8.0]])))


(def v228_l543 (kind/doc #'el/abs))


(def
 v230_l547
 (let
  [m (el/abs (t/complex-tensor [3.0] [4.0]))]
  (< (Math/abs (- (double (m 0)) 5.0)) 1.0E-10)))


(deftest t231_l550 (is (true? v230_l547)))


(def v233_l554 (kind/doc #'la/dot))


(def
 v235_l558
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


(deftest t236_l563 (is (true? v235_l558)))


(def v237_l565 (kind/doc #'la/dot-conj))


(def
 v239_l569
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


(deftest t240_l574 (is (true? v239_l569)))


(def
 v242_l578
 (let
  [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  [(count ct) (t/scalar? (ct 0)) (el/re (ct 1))]))


(deftest t243_l581 (is (= v242_l578 [3 true 2.0])))
