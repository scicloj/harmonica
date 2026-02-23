(ns
 harmonica-book.dft-as-group-fourier-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
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
 v3_l34
 (def
  temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3 3 4 8 13 18 23 26 25 20 14 8 4]))


(def
 v4_l38
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


(def v6_l56 (def G (hm/cyclic-group 24)))


(def v7_l58 (hm/elements G))


(deftest t8_l60 (is (= v7_l58 (range 24))))


(def v10_l65 (hm/op G 15 9))


(deftest t11_l67 (is (= v10_l65 0)))


(def v12_l70 (hm/op G 18 10))


(deftest t13_l72 (is (= v12_l70 4)))


(def v15_l77 (hm/inv G 15))


(deftest t16_l79 (is (= v15_l77 9)))


(def v18_l111 (def ct (hm/character-table G)))


(def v19_l113 ct)


(def v21_l117 (allclose? (cx/cabs (:table ct)) 1.0))


(deftest t22_l119 (is (true? v21_l117)))


(def v24_l124 (allclose? (cx/re ((:table ct) 0)) 1.0))


(deftest t25_l126 (is (true? v24_l124)))


(def
 v27_l132
 (->
  (tc/dataset
   (let
    [table (:table ct)]
    (for
     [k [0 1 2 3] g (range 24)]
     {:month g,
      :real-part (cx/re ((table k) g)),
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


(def v29_l163 (def signal (cx/complex-tensor-real temperatures)))


(def v30_l165 signal)


(def v31_l167 (def f-hat (hm/fourier-transform ct signal)))


(def v32_l169 f-hat)


(def v34_l173 (cx/re (f-hat 0)))


(deftest
 t35_l175
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v34_l173)))


(def
 v37_l180
 (->
  (tc/dataset
   {:frequency (range 24), :magnitude (vec (cx/cabs f-hat))})
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
 v39_l215
 (let
  [fft-result
   (t/forward-1d (t/transformer :real :fft) temperatures)
   fft-coefficients
   (let
    [data (vec fft-result) n (/ (count data) 2)]
    (cx/complex-tensor
     (mapv (fn [k] (data (* 2 k))) (range n))
     (mapv (fn [k] (data (inc (* 2 k)))) (range n))))]
  (allclose?
   (dtype/sub-buffer (cx/cabs f-hat) 0 12)
   (cx/cabs fft-coefficients)
   1.0E-8)))


(deftest t40_l224 (is (true? v39_l215)))


(def
 v42_l241
 (def
  orthogonality-matrix
  (let
   [table (:table ct) sizes (:class-sizes ct) n 24]
   (tensor/compute-tensor
    [n n]
    (fn
     [j k]
     (cx/cabs
      (hm/character-inner-product (table j) (table k) sizes n)))
    :float64))))


(def v43_l251 orthogonality-matrix)


(def
 v45_l256
 (allclose?
  orthogonality-matrix
  (tensor/compute-tensor
   [24 24]
   (fn [j k] (if (= j k) 1.0 0.0))
   :float64)))


(deftest t46_l259 (is (true? v45_l256)))


(def
 v48_l268
 (let
  [reconstructed (hm/inverse-fourier-transform ct f-hat)]
  (dfn/reduce-max (cx/cabs (cx/csub reconstructed signal)))))


(deftest t49_l270 (is ((fn [err] (< err 1.0E-10)) v48_l268)))


(def
 v51_l289
 (def
  f-fn
  (cx/complex-tensor-real
   [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])))


(def
 v52_l291
 (def
  h-fn
  (cx/complex-tensor-real
   [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v54_l296 (def convolved (hm/convolve ct f-fn h-fn)))


(def
 v55_l298
 (mapv
  (fn* [p1__120953#] (Math/round p1__120953#))
  (vec (cx/re convolved))))


(deftest
 t56_l300
 (is (= v55_l298 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def
 v58_l305
 (let
  [f-fn-hat
   (hm/fourier-transform ct f-fn)
   h-fn-hat
   (hm/fourier-transform ct h-fn)
   convolved-hat
   (hm/fourier-transform ct convolved)
   pointwise-product
   (cx/cmul f-fn-hat h-fn-hat)]
  (<
   (dfn/reduce-max (cx/cabs (cx/csub convolved-hat pointwise-product)))
   1.0E-8)))


(deftest t59_l311 (is (true? v58_l305)))


(def
 v61_l322
 (let
  [mag-s
   (cx/cabs signal)
   mag-f
   (cx/cabs f-hat)
   energy-time
   (dfn/sum (dfn/* mag-s mag-s))
   energy-freq
   (/ (dfn/sum (dfn/* mag-f mag-f)) (double (hm/order G)))]
  (< (Math/abs (- energy-time energy-freq)) 1.0E-8)))


(deftest t62_l328 (is (true? v61_l322)))


(def
 v64_l338
 (def f-real [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))


(def
 v65_l340
 (def h-real [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))


(def
 v67_l345
 (def
  linear-conv
  (vec
   (dt-conv/convolve1d f-real h-real {:mode :full, :edge-mode :zero}))))


(def v68_l348 (count linear-conv))


(deftest t69_l350 (is (= v68_l348 47)))


(def
 v71_l355
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


(def v72_l364 cyclic-from-linear)


(deftest
 t73_l366
 (is
  ((fn
    [v]
    (=
     (mapv long v)
     [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
   v72_l364)))


(def
 v75_l371
 (let
  [group-conv
   (cx/re
    (hm/convolve
     ct
     (cx/complex-tensor-real f-real)
     (cx/complex-tensor-real h-real)))]
  (allclose? cyclic-from-linear group-conv)))


(deftest t76_l376 (is (true? v75_l371)))
