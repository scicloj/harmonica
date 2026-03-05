(ns
 harmonica-book.dft-as-group-fourier-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as lt]
  [scicloj.lalinea.elementwise :as el]
  [harmonica-book.book-helpers :refer [allclose?]]
  [fastmath.transform :as t]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [tech.v3.datatype.convolve :as dt-conv]
  [tech.v3.tensor :as tensor]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l35
 (def
  temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3 3 4 8 13 18 23 26 25 20 14 8 4]))


(def
 v4_l39
 (->
  (tc/dataset {:month (range 24), :temp temperatures})
  (plotly/base
   {:=x :month,
    :=y :temp,
    :=title "Monthly temperatures — two years of data",
    :=x-title "month",
    :=y-title "°C"})
  (plotly/layer-line)
  (plotly/layer-point {:=mark-size 5})
  plotly/plot))


(def v6_l57 (def G (hm/cyclic-group 24)))


(def v7_l59 (hm/elements G))


(deftest t8_l61 (is (= v7_l59 (range 24))))


(def v10_l66 (hm/op G 15 9))


(deftest t11_l68 (is (= v10_l66 0)))


(def v12_l71 (hm/op G 18 10))


(deftest t13_l73 (is (= v12_l71 4)))


(def v15_l78 (hm/inv G 15))


(deftest t16_l80 (is (= v15_l78 9)))


(def v18_l112 (def ct (hm/character-table G)))


(def v19_l114 ct)


(def v21_l118 (allclose? (el/abs (:table ct)) 1.0))


(deftest t22_l120 (is (true? v21_l118)))


(def v24_l125 (allclose? (el/re ((:table ct) 0)) 1.0))


(deftest t25_l127 (is (true? v24_l125)))


(def
 v27_l133
 (->
  (tc/dataset
   (let
    [table (:table ct)]
    (for
     [k [0 1 2 3] g (range 24)]
     {:month g,
      :real-part (el/re ((table k) g)),
      :character (str "chi_" k)})))
  (plotly/base
   {:=x :month,
    :=y :real-part,
    :=color :character,
    :=x-title "Group element g (month)",
    :=y-title "Re(chi_k(g))",
    :=title "Characters of Z/24Z — real parts (cosine components)"})
  (plotly/layer-line)
  (plotly/layer-point {:=mark-size 6})
  plotly/plot))


(def v29_l164 (def signal (lt/complex-tensor-real temperatures)))


(def v30_l166 signal)


(def v31_l168 (def f-hat (hm/fourier-transform ct signal)))


(def v32_l170 f-hat)


(def v34_l174 (el/re (f-hat 0)))


(deftest
 t35_l176
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v34_l174)))


(def
 v37_l181
 (->
  (tc/dataset {:frequency (range 24), :magnitude (vec (el/abs f-hat))})
  (plotly/base
   {:=x :frequency,
    :=y :magnitude,
    :=x-title "Frequency k (character index)",
    :=y-title "|f-hat(k)|",
    :=title "Fourier spectrum of monthly temperatures on Z/24Z"})
  (plotly/layer-line)
  (plotly/layer-point {:=mark-size 6})
  plotly/plot))


(def
 v39_l216
 (let
  [fft-result
   (t/forward-1d (t/transformer :real :fft) temperatures)
   fft-coefficients
   (let
    [data (vec fft-result) n (/ (count data) 2)]
    (lt/complex-tensor
     (mapv (fn [k] (data (* 2 k))) (range n))
     (mapv (fn [k] (data (inc (* 2 k)))) (range n))))]
  (allclose?
   (dtype/sub-buffer (el/abs f-hat) 0 12)
   (el/abs fft-coefficients)
   1.0E-8)))


(deftest t40_l225 (is (true? v39_l216)))


(def
 v42_l242
 (def
  orthogonality-matrix
  (let
   [table (:table ct) sizes (:class-sizes ct) n 24]
   (tensor/compute-tensor
    [n n]
    (fn
     [j k]
     (el/abs (hm/character-inner-product (table j) (table k) sizes n)))
    :float64))))


(def v43_l252 orthogonality-matrix)


(def
 v45_l257
 (allclose?
  orthogonality-matrix
  (tensor/compute-tensor
   [24 24]
   (fn [j k] (if (= j k) 1.0 0.0))
   :float64)))


(deftest t46_l260 (is (true? v45_l257)))


(def
 v48_l269
 (let
  [reconstructed (hm/inverse-fourier-transform ct f-hat)]
  (dfn/reduce-max (el/abs (el/- reconstructed signal)))))


(deftest t49_l271 (is ((fn [err] (< err 1.0E-10)) v48_l269)))


(def
 v51_l290
 (def
  f-fn
  (lt/complex-tensor-real
   [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])))


(def
 v52_l292
 (def
  h-fn
  (lt/complex-tensor-real
   [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v54_l297 (def convolved (hm/convolve ct f-fn h-fn)))


(def
 v55_l299
 (mapv
  (fn* [p1__66219#] (Math/round p1__66219#))
  (vec (el/re convolved))))


(deftest
 t56_l301
 (is (= v55_l299 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def
 v58_l306
 (let
  [f-fn-hat
   (hm/fourier-transform ct f-fn)
   h-fn-hat
   (hm/fourier-transform ct h-fn)
   convolved-hat
   (hm/fourier-transform ct convolved)
   pointwise-product
   (el/* f-fn-hat h-fn-hat)]
  (<
   (dfn/reduce-max (el/abs (el/- convolved-hat pointwise-product)))
   1.0E-8)))


(deftest t59_l312 (is (true? v58_l306)))


(def
 v61_l323
 (let
  [mag-s
   (el/abs signal)
   mag-f
   (el/abs f-hat)
   energy-time
   (dfn/sum (dfn/* mag-s mag-s))
   energy-freq
   (/ (dfn/sum (dfn/* mag-f mag-f)) (double (hm/order G)))]
  (< (Math/abs (- energy-time energy-freq)) 1.0E-8)))


(deftest t62_l329 (is (true? v61_l323)))


(def
 v64_l339
 (def f-real [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))


(def
 v65_l341
 (def h-real [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))


(def
 v67_l346
 (def
  linear-conv
  (vec
   (dt-conv/convolve1d f-real h-real {:mode :full, :edge-mode :zero}))))


(def v68_l349 (count linear-conv))


(deftest t69_l351 (is (= v68_l349 47)))


(def
 v71_l356
 (def
  cyclic-from-linear
  (let
   [n 24]
   (mapv
    (fn
     [i]
     (+
      (linear-conv i)
      (if (< (+ i n) (count linear-conv)) (linear-conv (+ i n)) 0.0)))
    (range n)))))


(def v72_l365 cyclic-from-linear)


(deftest
 t73_l367
 (is
  ((fn
    [v]
    (=
     (mapv long v)
     [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
   v72_l365)))


(def
 v75_l372
 (let
  [group-conv
   (el/re
    (hm/convolve
     ct
     (lt/complex-tensor-real f-real)
     (lt/complex-tensor-real h-real)))]
  (allclose? cyclic-from-linear group-conv)))


(deftest t76_l377 (is (true? v75_l372)))
