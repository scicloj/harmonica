(ns
 harmonica-book.dft-as-group-fourier-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [harmonica-book.book-helpers :refer [allclose?]]
  [fastmath.transform :as ft]
  [tech.v3.datatype.convolve :as dt-conv]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l32
 (def
  temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3 3 4 8 13 18 23 26 25 20 14 8 4]))


(def
 v4_l36
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


(def v6_l54 (def G (hm/cyclic-group 24)))


(def v7_l56 (hm/elements G))


(deftest t8_l58 (is (= v7_l56 (range 24))))


(def v10_l63 (hm/op G 15 9))


(deftest t11_l65 (is (= v10_l63 0)))


(def v12_l68 (hm/op G 18 10))


(deftest t13_l70 (is (= v12_l68 4)))


(def v15_l75 (hm/inv G 15))


(deftest t16_l77 (is (= v15_l75 9)))


(def v18_l109 (def ct (hm/character-table G)))


(def v19_l111 ct)


(def v21_l115 (allclose? (el/abs (:table ct)) 1.0))


(deftest t22_l117 (is (true? v21_l115)))


(def v24_l122 (allclose? (el/re ((:table ct) 0)) 1.0))


(deftest t25_l124 (is (true? v24_l122)))


(def
 v27_l130
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


(def v29_l161 (def signal (t/complex-tensor-real temperatures)))


(def v30_l163 signal)


(def v31_l165 (def f-hat (hm/fourier-transform ct signal)))


(def v32_l167 f-hat)


(def v34_l171 (el/re (f-hat 0)))


(deftest
 t35_l173
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v34_l171)))


(def
 v37_l178
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
 v39_l213
 (let
  [fft-result
   (ft/forward-1d (ft/transformer :real :fft) temperatures)
   fft-coefficients
   (let
    [data (vec fft-result) n (/ (count data) 2)]
    (t/complex-tensor
     (mapv (fn [k] (data (* 2 k))) (range n))
     (mapv (fn [k] (data (inc (* 2 k)))) (range n))))]
  (allclose?
   (t/select (el/abs f-hat) (range 12))
   (el/abs fft-coefficients)
   1.0E-8)))


(deftest t40_l222 (is (true? v39_l213)))


(def
 v42_l239
 (def
  orthogonality-matrix
  (let
   [table (:table ct) sizes (:class-sizes ct) n 24]
   (t/compute-tensor
    [n n]
    (fn
     [j k]
     (el/abs (hm/character-inner-product (table j) (table k) sizes n)))
    :float64))))


(def v43_l249 orthogonality-matrix)


(def
 v45_l254
 (allclose?
  orthogonality-matrix
  (t/compute-tensor [24 24] (fn [j k] (if (= j k) 1.0 0.0)) :float64)))


(deftest t46_l257 (is (true? v45_l254)))


(def
 v48_l266
 (let
  [reconstructed (hm/inverse-fourier-transform ct f-hat)]
  (el/reduce-max (el/abs (el/- reconstructed signal)))))


(deftest t49_l268 (is ((fn [err] (< err 1.0E-10)) v48_l266)))


(def
 v51_l287
 (def
  f-fn
  (t/complex-tensor-real
   [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])))


(def
 v52_l289
 (def
  h-fn
  (t/complex-tensor-real
   [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v54_l294 (def convolved (hm/convolve ct f-fn h-fn)))


(def
 v55_l296
 (mapv
  (fn* [p1__91394#] (Math/round p1__91394#))
  (vec (el/re convolved))))


(deftest
 t56_l298
 (is (= v55_l296 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def
 v58_l303
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
   (el/reduce-max (el/abs (el/- convolved-hat pointwise-product)))
   1.0E-8)))


(deftest t59_l309 (is (true? v58_l303)))


(def
 v61_l320
 (let
  [mag-s
   (el/abs signal)
   mag-f
   (el/abs f-hat)
   energy-time
   (el/sum (el/* mag-s mag-s))
   energy-freq
   (/ (el/sum (el/* mag-f mag-f)) (double (hm/order G)))]
  (< (Math/abs (- energy-time energy-freq)) 1.0E-8)))


(deftest t62_l326 (is (true? v61_l320)))


(def
 v64_l336
 (def f-real [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))


(def
 v65_l338
 (def h-real [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))


(def
 v67_l343
 (def
  linear-conv
  (vec
   (dt-conv/convolve1d f-real h-real {:mode :full, :edge-mode :zero}))))


(def v68_l346 (count linear-conv))


(deftest t69_l348 (is (= v68_l346 47)))


(def
 v71_l353
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


(def v72_l362 cyclic-from-linear)


(deftest
 t73_l364
 (is
  ((fn
    [v]
    (=
     (mapv long v)
     [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
   v72_l362)))


(def
 v75_l369
 (let
  [group-conv
   (el/re
    (hm/convolve
     ct
     (t/complex-tensor-real f-real)
     (t/complex-tensor-real h-real)))]
  (allclose? cyclic-from-linear group-conv)))


(deftest t76_l374 (is (true? v75_l369)))
