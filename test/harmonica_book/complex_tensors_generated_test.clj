(ns
 harmonica-book.complex-tensors-generated-test
 (:require
  [scicloj.harmonica.linalg.complex :as cx]
  [tech.v3.tensor :as tensor]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l35 (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0]))


(deftest t4_l37 (is ((fn [v] (= [3] (cx/complex-shape v))) v3_l35)))


(def
 v6_l41
 (let
  [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  {:re (vec (cx/re ct)), :im (vec (cx/im ct))}))


(deftest
 t7_l45
 (is
  ((fn [v] (and (= (:re v) [1.0 2.0 3.0]) (= (:im v) [4.0 5.0 6.0])))
   v6_l41)))


(def v9_l53 (cx/complex-tensor (tensor/->tensor [[1.0 2.0] [3.0 4.0]])))


(deftest
 t10_l55
 (is
  ((fn
    [v]
    (and
     (= [2] (cx/complex-shape v))
     (= [1.0 3.0] (vec (cx/re v)))
     (= [2.0 4.0] (vec (cx/im v)))))
   v9_l53)))


(def v12_l63 (cx/complex-tensor-real [5.0 6.0 7.0]))


(deftest
 t13_l65
 (is
  ((fn
    [v]
    (and
     (= [5.0 6.0 7.0] (vec (cx/re v)))
     (= [0.0 0.0 0.0] (vec (cx/im v)))))
   v12_l63)))


(def v15_l72 (def z (cx/complex-tensor (tensor/->tensor [3.0 4.0]))))


(def v16_l74 (cx/scalar? z))


(deftest t17_l76 (is (true? v16_l74)))


(def v19_l80 [(cx/re z) (cx/im z)])


(deftest t20_l82 (is (= v19_l80 [3.0 4.0])))


(def v22_l86 [(count z) (seq z)])


(deftest t23_l88 (is (= v22_l86 [0 nil])))


(def
 v25_l94
 (def
  M
  (cx/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])))


(def v26_l97 (cx/complex-shape M))


(deftest t27_l99 (is (= v26_l97 [2 2])))


(def
 v29_l108
 (let
  [ct (cx/complex-tensor [10.0 20.0 30.0] [0.1 0.2 0.3])]
  [(vec (cx/re ct)) (vec (cx/im ct))]))


(deftest t30_l111 (is (= v29_l108 [[10.0 20.0 30.0] [0.1 0.2 0.3]])))


(def
 v32_l115
 (let
  [ct (cx/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])]
  (vec (dtype/shape (cx/re ct)))))


(deftest t33_l119 (is (= v32_l115 [2 2])))


(def
 v35_l123
 (cx/complex-shape (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])))


(deftest t36_l125 (is (= v35_l123 [3])))


(def
 v38_l135
 (let
  [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  [(cx/re (ct 0)) (cx/im (ct 0))]))


(deftest t39_l138 (is (= v38_l135 [1.0 4.0])))


(def
 v40_l140
 (let
  [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  (cx/scalar? (ct 1))))


(deftest t41_l143 (is (true? v40_l140)))


(def v43_l147 (nth (cx/complex-tensor [1.0] [2.0]) 99 :missing))


(deftest t44_l149 (is (= v43_l147 :missing)))


(def
 v46_l155
 (let
  [ct
   (cx/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])
   row0
   (ct 0)]
  {:shape (cx/complex-shape row0),
   :re (vec (cx/re row0)),
   :im (vec (cx/im row0))}))


(deftest
 t47_l162
 (is
  ((fn
    [v]
    (and
     (= (:shape v) [2])
     (= (:re v) [1.0 2.0])
     (= (:im v) [5.0 6.0])))
   v46_l155)))


(def
 v49_l168
 (let
  [ct (cx/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])]
  [(cx/re ((ct 1) 1)) (cx/im ((ct 1) 1))]))


(deftest t50_l172 (is (= v49_l168 [4.0 8.0])))


(def
 v52_l178
 (let
  [ct (cx/complex-tensor [1.0 2.0] [3.0 4.0])]
  (mapv cx/re (seq ct))))


(deftest t53_l181 (is (= v52_l178 [1.0 2.0])))


(def
 v54_l183
 (let
  [ct (cx/complex-tensor [1.0 2.0] [3.0 4.0])]
  (every? cx/scalar? (seq ct))))


(deftest t55_l186 (is (true? v54_l183)))


(def
 v57_l197
 (let
  [a
   (cx/complex-tensor [1.0 2.0] [3.0 4.0])
   b
   (cx/complex-tensor [5.0 6.0] [7.0 8.0])
   c
   (cx/cmul a b)]
  {:re (vec (cx/re c)), :im (vec (cx/im c))}))


(deftest
 t59_l207
 (is
  ((fn [v] (and (= (:re v) [-16.0 -20.0]) (= (:im v) [22.0 40.0])))
   v57_l197)))


(def
 v61_l214
 (let
  [ct (cx/cconj (cx/complex-tensor [1.0 2.0] [3.0 -4.0]))]
  {:re (vec (cx/re ct)), :im (vec (cx/im ct))}))


(deftest
 t62_l218
 (is
  ((fn [v] (and (= (:re v) [1.0 2.0]) (= (:im v) [-3.0 4.0])))
   v61_l214)))


(def
 v64_l225
 (let
  [ct (cx/cscale (cx/complex-tensor [1.0 2.0] [3.0 4.0]) 2.0)]
  {:re (vec (cx/re ct)), :im (vec (cx/im ct))}))


(deftest
 t65_l229
 (is
  ((fn [v] (and (= (:re v) [2.0 4.0]) (= (:im v) [6.0 8.0])))
   v64_l225)))


(def
 v67_l236
 (let
  [m (cx/cabs (cx/complex-tensor [3.0 0.0] [4.0 1.0]))]
  [(double (m 0)) (double (m 1))]))


(deftest
 t69_l241
 (is
  ((fn
    [v]
    (and
     (< (Math/abs (- (first v) 5.0)) 1.0E-10)
     (< (Math/abs (- (second v) 1.0)) 1.0E-10)))
   v67_l236)))


(def
 v71_l252
 (let
  [a
   (cx/complex-tensor [1.0 0.0] [0.0 1.0])
   b
   (cx/complex-tensor [0.0 1.0] [1.0 0.0])]
  (cx/cdot a b)))


(deftest
 t73_l258
 (is
  ((fn
    [[re im]]
    (and (< (Math/abs re) 1.0E-10) (< (Math/abs (- im 2.0)) 1.0E-10)))
   v71_l252)))


(def
 v75_l269
 (let
  [a
   (cx/complex-tensor [1.0 0.0] [0.0 1.0])
   b
   (cx/complex-tensor [0.0 1.0] [1.0 0.0])]
  (cx/cdot-conj a b)))


(deftest
 t77_l275
 (is
  ((fn
    [[re im]]
    (and (< (Math/abs re) 1.0E-10) (< (Math/abs im) 1.0E-10)))
   v75_l269)))


(def
 v79_l281
 (let
  [a
   (cx/complex-tensor [3.0 1.0] [4.0 2.0])
   [re im]
   (cx/cdot-conj a a)]
  {:norm-sq re, :im-part im}))


(deftest
 t81_l287
 (is
  ((fn
    [v]
    (and
     (< (Math/abs (- (:norm-sq v) 30.0)) 1.0E-10)
     (< (Math/abs (:im-part v)) 1.0E-10)))
   v79_l281)))


(def v83_l295 (def a (cx/complex-tensor [1.0 -2.0 3.0] [4.0 5.0 -6.0])))


(def v84_l296 (def b (cx/complex-tensor [-3.0 0.5 2.0] [1.0 -1.5 7.0])))


(def v85_l297 (def c (cx/complex-tensor [0.0 4.0 -1.0] [2.0 -3.0 0.5])))


(def
 v86_l299
 (defn
  approx=
  "Check that two ComplexTensors are approximately equal."
  [x y tol]
  (let
   [re-diff
    (dfn/- (cx/re x) (cx/re y))
    im-diff
    (dfn/- (cx/im x) (cx/im y))
    max-re
    (dfn/reduce-max (dfn/abs re-diff))
    max-im
    (dfn/reduce-max (dfn/abs im-diff))]
   (and (< max-re tol) (< max-im tol)))))


(def v88_l312 (approx= (cx/cmul a b) (cx/cmul b a) 1.0E-10))


(deftest t89_l314 (is (true? v88_l312)))


(def
 v91_l320
 (approx= (cx/cmul (cx/cmul a b) c) (cx/cmul a (cx/cmul b c)) 1.0E-10))


(deftest t92_l324 (is (true? v91_l320)))


(def
 v94_l330
 (let
  [one (cx/complex-tensor-real [1.0 1.0 1.0])]
  (approx= (cx/cmul a one) a 1.0E-10)))


(deftest t95_l333 (is (true? v94_l330)))


(def v97_l339 (approx= (cx/cconj (cx/cconj a)) a 1.0E-10))


(deftest t98_l341 (is (true? v97_l339)))


(def
 v100_l347
 (approx=
  (cx/cconj (cx/cmul a b))
  (cx/cmul (cx/cconj a) (cx/cconj b))
  1.0E-10))


(deftest t101_l351 (is (true? v100_l347)))


(def
 v103_l357
 (let
  [prod
   (cx/cmul a (cx/cconj a))
   mag-sq
   (dfn/+ (dfn/* (cx/re a) (cx/re a)) (dfn/* (cx/im a) (cx/im a)))]
  (and
   (< (dfn/reduce-max (dfn/abs (dfn/- (cx/re prod) mag-sq))) 1.0E-10)
   (< (dfn/reduce-max (dfn/abs (cx/im prod))) 1.0E-10))))


(deftest t104_l363 (is (true? v103_l357)))


(def
 v106_l369
 (let
  [lhs (cx/cabs (cx/cmul a b)) rhs (dfn/* (cx/cabs a) (cx/cabs b))]
  (< (dfn/reduce-max (dfn/abs (dfn/- lhs rhs))) 1.0E-10)))


(deftest t107_l373 (is (true? v106_l369)))


(def
 v109_l379
 (let
  [alpha 3.7]
  (approx=
   (cx/cscale (cx/cmul a b) alpha)
   (cx/cmul (cx/cscale a alpha) b)
   1.0E-10)))


(deftest t110_l384 (is (true? v109_l379)))


(def
 v112_l390
 (let
  [alpha -2.5]
  (approx=
   (cx/cconj (cx/cscale a alpha))
   (cx/cscale (cx/cconj a) alpha)
   1.0E-10)))


(deftest t113_l395 (is (true? v112_l390)))


(def
 v115_l401
 (let
  [[re-ab im-ab] (cx/cdot-conj a b) [re-ba im-ba] (cx/cdot-conj b a)]
  (and
   (< (Math/abs (- re-ab re-ba)) 1.0E-10)
   (< (Math/abs (+ im-ab im-ba)) 1.0E-10))))


(deftest t116_l406 (is (true? v115_l401)))


(def
 v118_l412
 (let
  [[re-aa im-aa] (cx/cdot-conj a a)]
  (and (>= re-aa 0.0) (< (Math/abs im-aa) 1.0E-10))))


(deftest t119_l416 (is (true? v118_l412)))


(def
 v120_l418
 (let
  [zero
   (cx/complex-tensor-real [0.0 0.0 0.0])
   [re-00 _]
   (cx/cdot-conj zero zero)]
  (< (Math/abs re-00) 1.0E-10)))


(deftest t121_l422 (is (true? v120_l418)))


(def
 v123_l428
 (let
  [[re-aa _]
   (cx/cdot-conj a a)
   norm-sq
   (dfn/sum
    (dfn/+ (dfn/* (cx/re a) (cx/re a)) (dfn/* (cx/im a) (cx/im a))))]
  (< (Math/abs (- re-aa norm-sq)) 1.0E-10)))


(deftest t124_l433 (is (true? v123_l428)))


(def
 v126_l439
 (let
  [[re-ab im-ab] (cx/cdot a b) [re-ba im-ba] (cx/cdot b a)]
  (and
   (< (Math/abs (- re-ab re-ba)) 1.0E-10)
   (< (Math/abs (- im-ab im-ba)) 1.0E-10))))


(deftest t127_l444 (is (true? v126_l439)))


(def
 v129_l450
 (let
  [[re-dot im-dot]
   (cx/cdot a b)
   [re-conj im-conj]
   (cx/cdot-conj a (cx/cconj b))]
  (and
   (< (Math/abs (- re-dot re-conj)) 1.0E-10)
   (< (Math/abs (- im-dot im-conj)) 1.0E-10))))


(deftest t130_l455 (is (true? v129_l450)))


(def
 v132_l461
 (let
  [[re-ab im-ab]
   (cx/cdot-conj a b)
   [re-aa _]
   (cx/cdot-conj a a)
   [re-bb _]
   (cx/cdot-conj b b)
   lhs
   (+ (* re-ab re-ab) (* im-ab im-ab))
   rhs
   (* re-aa re-bb)]
  (<= (- lhs 1.0E-10) rhs)))


(deftest t133_l468 (is (true? v132_l461)))


(def
 v135_l474
 (let
  [alpha
   3.7
   [re1 im1]
   (cx/cdot-conj (cx/cscale a alpha) b)
   [re2 im2]
   (cx/cdot-conj a b)]
  (and
   (< (Math/abs (- re1 (* alpha re2))) 1.0E-10)
   (< (Math/abs (- im1 (* alpha im2))) 1.0E-10))))


(deftest t136_l480 (is (true? v135_l474)))


(def
 v138_l490
 (let
  [ct (cx/complex-tensor [1.0 2.0] [3.0 4.0])]
  (vec (dtype/shape (cx/->tensor ct)))))


(deftest t139_l493 (is (= v138_l490 [2 2])))


(def
 v141_l497
 (let
  [ct
   (cx/complex-tensor [1.0 2.0] [3.0 4.0])
   arr
   (cx/->double-array ct)]
  {:identical? (identical? arr (cx/->double-array ct)),
   :values (vec arr)}))


(deftest
 t143_l504
 (is
  ((fn [v] (and (:identical? v) (= (:values v) [1.0 3.0 2.0 4.0])))
   v141_l497)))


(def
 v145_l515
 (def
  mat
  (cx/complex-tensor
   [[1.0 2.0 3.0] [4.0 5.0 6.0]]
   [[0.1 0.2 0.3] [0.4 0.5 0.6]])))


(def v146_l520 (cx/complex-shape mat))


(deftest t147_l522 (is (= v146_l520 [2 3])))


(def v148_l524 (count mat))


(deftest t149_l526 (is (= v148_l524 2)))


(def
 v151_l530
 (let
  [row (mat 0)]
  {:shape (cx/complex-shape row), :re (vec (cx/re row))}))


(deftest
 t152_l534
 (is
  ((fn [v] (and (= (:shape v) [3]) (= (:re v) [1.0 2.0 3.0])))
   v151_l530)))


(def
 v154_l539
 (let
  [re-mat (cx/re mat) shape (vec (dtype/shape re-mat))]
  {:shape shape,
   :row0 (vec (tensor/select re-mat 0 :all)),
   :row1 (vec (tensor/select re-mat 1 :all))}))


(deftest
 t155_l545
 (is
  ((fn
    [v]
    (and
     (= (:shape v) [2 3])
     (= (:row0 v) [1.0 2.0 3.0])
     (= (:row1 v) [4.0 5.0 6.0])))
   v154_l539)))


(def v157_l559 (cx/complex-tensor (tensor/->tensor [3.0 4.0])))


(deftest
 t158_l561
 (is
  ((fn [v] (clojure.string/includes? (str v) "3.0+4.0i")) v157_l559)))


(def v160_l565 (cx/complex-tensor (tensor/->tensor [3.0 -4.0])))


(deftest
 t161_l567
 (is
  ((fn [v] (clojure.string/includes? (str v) "3.0-4.0i")) v160_l565)))


(def v163_l571 (cx/complex-tensor (tensor/->tensor [5.0 0.0])))


(deftest
 t164_l573
 (is ((fn [v] (clojure.string/includes? (str v) "5.0")) v163_l571)))


(def v166_l577 (cx/complex-tensor (tensor/->tensor [0.0 0.0])))


(deftest
 t167_l579
 (is ((fn [v] (clojure.string/includes? (str v) "0.0")) v166_l577)))


(def v169_l583 (cx/complex-tensor (tensor/->tensor [0.0 3.0])))


(deftest
 t170_l585
 (is ((fn [v] (clojure.string/includes? (str v) "3.0i")) v169_l583)))


(def v172_l589 (cx/complex-tensor (tensor/->tensor [0.0 1.0])))


(deftest
 t173_l591
 (is ((fn [v] (clojure.string/ends-with? (str v) "\ni")) v172_l589)))


(def v174_l593 (cx/complex-tensor (tensor/->tensor [0.0 -1.0])))


(deftest
 t175_l595
 (is ((fn [v] (clojure.string/ends-with? (str v) "\n-i")) v174_l593)))


(def v177_l599 (cx/complex-tensor (tensor/->tensor [-2.0 3.0])))


(deftest
 t178_l601
 (is
  ((fn [v] (clojure.string/includes? (str v) "-2.0+3.0i")) v177_l599)))


(def v180_l608 (cx/complex-tensor [1.0 3.0] [2.0 4.0]))


(deftest
 t181_l610
 (is
  ((fn [v] (clojure.string/includes? (str v) "[1.0+2.0i, 3.0+4.0i]"))
   v180_l608)))


(def v182_l612 (cx/complex-tensor-real [1.0 2.0]))


(deftest
 t183_l614
 (is
  ((fn [v] (clojure.string/includes? (str v) "[1.0, 2.0]")) v182_l612)))


(def v185_l618 (cx/complex-tensor [1.0 0.0 -1.0] [2.0 3.0 0.0]))


(deftest
 t186_l620
 (is
  ((fn [v] (clojure.string/includes? (str v) "[1.0+2.0i, 3.0i, -1.0]"))
   v185_l618)))


(def v188_l624 (cx/complex-tensor-real (vec (range 25.0))))


(deftest
 t189_l626
 (is
  ((fn [v] (clojure.string/includes? (str v) "... (25 total)"))
   v188_l624)))


(def
 v191_l632
 (cx/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]]))


(deftest
 t192_l635
 (is
  ((fn
    [v]
    (and
     (clojure.string/includes? (str v) "#ComplexTensor [2 2]")
     (clojure.string/includes? (str v) "[1.0+5.0i, 2.0+6.0i]")
     (clojure.string/includes? (str v) "[3.0+7.0i, 4.0+8.0i]")))
   v191_l632)))


(def
 v194_l643
 (cx/complex-tensor
  [[[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]]]
  [[[0.1 0.2] [0.3 0.4]] [[0.5 0.6] [0.7 0.8]]]))


(deftest
 t195_l648
 (is
  ((fn [v] (clojure.string/includes? (str v) "#ComplexTensor [2 2 2]"))
   v194_l643)))
