(ns
 harmonica-book.complex-tensors-generated-test
 (:require
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [scicloj.lalinea.linalg :as la]
  [tech.v3.tensor :as tensor]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v2_l28
 (defn
  re-im
  "Extract [re im] pair from a scalar ComplexTensor."
  [ct]
  [(el/re ct) (el/im ct)]))


(def v4_l41 (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0]))


(deftest t5_l43 (is ((fn [v] (= [3] (t/complex-shape v))) v4_l41)))


(def
 v7_l47
 (let
  [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  {:re (vec (el/re ct)), :im (vec (el/im ct))}))


(deftest
 t8_l51
 (is
  ((fn [v] (and (= (:re v) [1.0 2.0 3.0]) (= (:im v) [4.0 5.0 6.0])))
   v7_l47)))


(def v10_l59 (t/complex-tensor (tensor/->tensor [[1.0 2.0] [3.0 4.0]])))


(deftest
 t11_l61
 (is
  ((fn
    [v]
    (and
     (= [2] (t/complex-shape v))
     (= [1.0 3.0] (vec (el/re v)))
     (= [2.0 4.0] (vec (el/im v)))))
   v10_l59)))


(def v13_l69 (t/complex-tensor-real [5.0 6.0 7.0]))


(deftest
 t14_l71
 (is
  ((fn
    [v]
    (and
     (= [5.0 6.0 7.0] (vec (el/re v)))
     (= [0.0 0.0 0.0] (vec (el/im v)))))
   v13_l69)))


(def v16_l78 (def z (t/complex-tensor (tensor/->tensor [3.0 4.0]))))


(def v17_l80 (t/scalar? z))


(deftest t18_l82 (is (true? v17_l80)))


(def v20_l86 [(el/re z) (el/im z)])


(deftest t21_l88 (is (= v20_l86 [3.0 4.0])))


(def v23_l92 [(count z) (seq z)])


(deftest t24_l94 (is (= v23_l92 [0 nil])))


(def
 v26_l100
 (def M (t/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])))


(def v27_l103 (t/complex-shape M))


(deftest t28_l105 (is (= v27_l103 [2 2])))


(def
 v30_l114
 (let
  [ct (t/complex-tensor [10.0 20.0 30.0] [0.1 0.2 0.3])]
  [(vec (el/re ct)) (vec (el/im ct))]))


(deftest t31_l117 (is (= v30_l114 [[10.0 20.0 30.0] [0.1 0.2 0.3]])))


(def
 v33_l121
 (let
  [ct (t/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])]
  (vec (dtype/shape (el/re ct)))))


(deftest t34_l125 (is (= v33_l121 [2 2])))


(def
 v36_l129
 (t/complex-shape (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])))


(deftest t37_l131 (is (= v36_l129 [3])))


(def
 v39_l141
 (let
  [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  [(el/re (ct 0)) (el/im (ct 0))]))


(deftest t40_l144 (is (= v39_l141 [1.0 4.0])))


(def
 v41_l146
 (let
  [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  (t/scalar? (ct 1))))


(deftest t42_l149 (is (true? v41_l146)))


(def v44_l153 (nth (t/complex-tensor [1.0] [2.0]) 99 :missing))


(deftest t45_l155 (is (= v44_l153 :missing)))


(def
 v47_l161
 (let
  [ct
   (t/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])
   row0
   (ct 0)]
  {:shape (t/complex-shape row0),
   :re (vec (el/re row0)),
   :im (vec (el/im row0))}))


(deftest
 t48_l168
 (is
  ((fn
    [v]
    (and
     (= (:shape v) [2])
     (= (:re v) [1.0 2.0])
     (= (:im v) [5.0 6.0])))
   v47_l161)))


(def
 v50_l174
 (let
  [ct (t/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])]
  [(el/re ((ct 1) 1)) (el/im ((ct 1) 1))]))


(deftest t51_l178 (is (= v50_l174 [4.0 8.0])))


(def
 v53_l184
 (let
  [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
  (mapv el/re (seq ct))))


(deftest t54_l187 (is (= v53_l184 [1.0 2.0])))


(def
 v55_l189
 (let
  [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
  (every? t/scalar? (seq ct))))


(deftest t56_l192 (is (true? v55_l189)))


(def
 v58_l203
 (let
  [a
   (t/complex-tensor [1.0 2.0] [3.0 4.0])
   b
   (t/complex-tensor [5.0 6.0] [7.0 8.0])
   c
   (el/* a b)]
  {:re (vec (el/re c)), :im (vec (el/im c))}))


(deftest
 t60_l213
 (is
  ((fn [v] (and (= (:re v) [-16.0 -20.0]) (= (:im v) [22.0 40.0])))
   v58_l203)))


(def
 v62_l220
 (let
  [ct (el/conj (t/complex-tensor [1.0 2.0] [3.0 -4.0]))]
  {:re (vec (el/re ct)), :im (vec (el/im ct))}))


(deftest
 t63_l224
 (is
  ((fn [v] (and (= (:re v) [1.0 2.0]) (= (:im v) [-3.0 4.0])))
   v62_l220)))


(def
 v65_l231
 (let
  [ct (el/scale (t/complex-tensor [1.0 2.0] [3.0 4.0]) 2.0)]
  {:re (vec (el/re ct)), :im (vec (el/im ct))}))


(deftest
 t66_l235
 (is
  ((fn [v] (and (= (:re v) [2.0 4.0]) (= (:im v) [6.0 8.0])))
   v65_l231)))


(def
 v68_l242
 (let
  [m (el/abs (t/complex-tensor [3.0 0.0] [4.0 1.0]))]
  [(double (m 0)) (double (m 1))]))


(deftest
 t70_l247
 (is
  ((fn
    [v]
    (and
     (< (Math/abs (- (first v) 5.0)) 1.0E-10)
     (< (Math/abs (- (second v) 1.0)) 1.0E-10)))
   v68_l242)))


(def
 v72_l258
 (let
  [a
   (t/complex-tensor [1.0 0.0] [0.0 1.0])
   b
   (t/complex-tensor [0.0 1.0] [1.0 0.0])]
  (el/sum (el/* a b))))


(deftest
 t74_l264
 (is
  ((fn
    [v]
    (let
     [re (el/re v) im (el/im v)]
     (and
      (< (Math/abs re) 1.0E-10)
      (< (Math/abs (- im 2.0)) 1.0E-10))))
   v72_l258)))


(def
 v76_l275
 (let
  [a
   (t/complex-tensor [1.0 0.0] [0.0 1.0])
   b
   (t/complex-tensor [0.0 1.0] [1.0 0.0])]
  (la/dot-conj a b)))


(deftest
 t78_l281
 (is
  ((fn
    [v]
    (let
     [re (el/re v) im (el/im v)]
     (and (< (Math/abs re) 1.0E-10) (< (Math/abs im) 1.0E-10))))
   v76_l275)))


(def
 v80_l287
 (let
  [a
   (t/complex-tensor [3.0 1.0] [4.0 2.0])
   [re im]
   (re-im (la/dot-conj a a))]
  {:norm-sq re, :im-part im}))


(deftest
 t82_l293
 (is
  ((fn
    [v]
    (and
     (< (Math/abs (- (:norm-sq v) 30.0)) 1.0E-10)
     (< (Math/abs (:im-part v)) 1.0E-10)))
   v80_l287)))


(def v84_l301 (def a (t/complex-tensor [1.0 -2.0 3.0] [4.0 5.0 -6.0])))


(def v85_l302 (def b (t/complex-tensor [-3.0 0.5 2.0] [1.0 -1.5 7.0])))


(def v86_l303 (def c (t/complex-tensor [0.0 4.0 -1.0] [2.0 -3.0 0.5])))


(def
 v87_l305
 (defn
  approx=
  "Check that two ComplexTensors are approximately equal."
  [x y tol]
  (let
   [re-diff
    (dfn/- (el/re x) (el/re y))
    im-diff
    (dfn/- (el/im x) (el/im y))
    max-re
    (dfn/reduce-max (dfn/abs re-diff))
    max-im
    (dfn/reduce-max (dfn/abs im-diff))]
   (and (< max-re tol) (< max-im tol)))))


(def v89_l318 (approx= (el/* a b) (el/* b a) 1.0E-10))


(deftest t90_l320 (is (true? v89_l318)))


(def v92_l326 (approx= (el/* (el/* a b) c) (el/* a (el/* b c)) 1.0E-10))


(deftest t93_l330 (is (true? v92_l326)))


(def
 v95_l336
 (let
  [one (t/complex-tensor-real [1.0 1.0 1.0])]
  (approx= (el/* a one) a 1.0E-10)))


(deftest t96_l339 (is (true? v95_l336)))


(def v98_l345 (approx= (el/conj (el/conj a)) a 1.0E-10))


(deftest t99_l347 (is (true? v98_l345)))


(def
 v101_l353
 (approx= (el/conj (el/* a b)) (el/* (el/conj a) (el/conj b)) 1.0E-10))


(deftest t102_l357 (is (true? v101_l353)))


(def
 v104_l363
 (let
  [prod
   (el/* a (el/conj a))
   mag-sq
   (dfn/+ (dfn/* (el/re a) (el/re a)) (dfn/* (el/im a) (el/im a)))]
  (and
   (< (dfn/reduce-max (dfn/abs (dfn/- (el/re prod) mag-sq))) 1.0E-10)
   (< (dfn/reduce-max (dfn/abs (el/im prod))) 1.0E-10))))


(deftest t105_l369 (is (true? v104_l363)))


(def
 v107_l375
 (let
  [lhs (el/abs (el/* a b)) rhs (dfn/* (el/abs a) (el/abs b))]
  (< (dfn/reduce-max (dfn/abs (dfn/- lhs rhs))) 1.0E-10)))


(deftest t108_l379 (is (true? v107_l375)))


(def
 v110_l385
 (let
  [alpha 3.7]
  (approx=
   (el/scale (el/* a b) alpha)
   (el/* (el/scale a alpha) b)
   1.0E-10)))


(deftest t111_l390 (is (true? v110_l385)))


(def
 v113_l396
 (let
  [alpha -2.5]
  (approx=
   (el/conj (el/scale a alpha))
   (el/scale (el/conj a) alpha)
   1.0E-10)))


(deftest t114_l401 (is (true? v113_l396)))


(def
 v116_l407
 (let
  [[re-ab im-ab]
   (re-im (la/dot-conj a b))
   [re-ba im-ba]
   (re-im (la/dot-conj b a))]
  (and
   (< (Math/abs (- re-ab re-ba)) 1.0E-10)
   (< (Math/abs (+ im-ab im-ba)) 1.0E-10))))


(deftest t117_l412 (is (true? v116_l407)))


(def
 v119_l418
 (let
  [[re-aa im-aa] (re-im (la/dot-conj a a))]
  (and (>= re-aa 0.0) (< (Math/abs im-aa) 1.0E-10))))


(deftest t120_l422 (is (true? v119_l418)))


(def
 v121_l424
 (let
  [zero
   (t/complex-tensor-real [0.0 0.0 0.0])
   [re-00 _]
   (re-im (la/dot-conj zero zero))]
  (< (Math/abs re-00) 1.0E-10)))


(deftest t122_l428 (is (true? v121_l424)))


(def
 v124_l434
 (let
  [[re-aa _]
   (re-im (la/dot-conj a a))
   norm-sq
   (dfn/sum
    (dfn/+ (dfn/* (el/re a) (el/re a)) (dfn/* (el/im a) (el/im a))))]
  (< (Math/abs (- re-aa norm-sq)) 1.0E-10)))


(deftest t125_l439 (is (true? v124_l434)))


(def
 v127_l445
 (let
  [[re-ab im-ab]
   (re-im (el/sum (el/* a b)))
   [re-ba im-ba]
   (re-im (el/sum (el/* b a)))]
  (and
   (< (Math/abs (- re-ab re-ba)) 1.0E-10)
   (< (Math/abs (- im-ab im-ba)) 1.0E-10))))


(deftest t128_l450 (is (true? v127_l445)))


(def
 v130_l456
 (let
  [[re-dot im-dot]
   (re-im (el/sum (el/* a b)))
   [re-conj im-conj]
   (re-im (la/dot-conj a (el/conj b)))]
  (and
   (< (Math/abs (- re-dot re-conj)) 1.0E-10)
   (< (Math/abs (- im-dot im-conj)) 1.0E-10))))


(deftest t131_l461 (is (true? v130_l456)))


(def
 v133_l467
 (let
  [[re-ab im-ab]
   (re-im (la/dot-conj a b))
   [re-aa _]
   (re-im (la/dot-conj a a))
   [re-bb _]
   (re-im (la/dot-conj b b))
   lhs
   (+ (* re-ab re-ab) (* im-ab im-ab))
   rhs
   (* re-aa re-bb)]
  (<= (- lhs 1.0E-10) rhs)))


(deftest t134_l474 (is (true? v133_l467)))


(def
 v136_l480
 (let
  [alpha
   3.7
   [re1 im1]
   (re-im (la/dot-conj (el/scale a alpha) b))
   [re2 im2]
   (re-im (la/dot-conj a b))]
  (and
   (< (Math/abs (- re1 (* alpha re2))) 1.0E-10)
   (< (Math/abs (- im1 (* alpha im2))) 1.0E-10))))


(deftest t137_l486 (is (true? v136_l480)))


(def
 v139_l496
 (let
  [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
  (vec (dtype/shape (t/->tensor ct)))))


(deftest t140_l499 (is (= v139_l496 [2 2])))


(def
 v142_l503
 (let
  [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
  (vec (t/->double-array ct))))


(deftest t144_l508 (is (= v142_l503 [1.0 3.0 2.0 4.0])))


(def
 v146_l518
 (def
  mat
  (t/complex-tensor
   [[1.0 2.0 3.0] [4.0 5.0 6.0]]
   [[0.1 0.2 0.3] [0.4 0.5 0.6]])))


(def v147_l523 (t/complex-shape mat))


(deftest t148_l525 (is (= v147_l523 [2 3])))


(def v149_l527 (count mat))


(deftest t150_l529 (is (= v149_l527 2)))


(def
 v152_l533
 (let
  [row (mat 0)]
  {:shape (t/complex-shape row), :re (vec (el/re row))}))


(deftest
 t153_l537
 (is
  ((fn [v] (and (= (:shape v) [3]) (= (:re v) [1.0 2.0 3.0])))
   v152_l533)))


(def
 v155_l542
 (let
  [re-mat (el/re mat) shape (vec (dtype/shape re-mat))]
  {:shape shape,
   :row0 (vec (tensor/select re-mat 0 :all)),
   :row1 (vec (tensor/select re-mat 1 :all))}))


(deftest
 t156_l548
 (is
  ((fn
    [v]
    (and
     (= (:shape v) [2 3])
     (= (:row0 v) [1.0 2.0 3.0])
     (= (:row1 v) [4.0 5.0 6.0])))
   v155_l542)))


(def v158_l557 (str (t/complex 3.0 4.0)))


(deftest
 t159_l559
 (is ((fn [v] (clojure.string/includes? v "ComplexTensor")) v158_l557)))


(def v160_l561 (str (t/complex-tensor [1.0 2.0] [3.0 4.0])))


(deftest t161_l563 (is (= v160_l561 "ComplexTensor<float64>[2]")))


(def
 v162_l565
 (str (t/complex-tensor [[1.0 2.0] [3.0 4.0]] [[5.0 6.0] [7.0 8.0]])))


(deftest t163_l568 (is (= v162_l565 "ComplexTensor<float64>[2 2]")))
