(ns
 harmonica-book.dft-as-group-fourier-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [fastmath.transform :as t]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [tech.v3.datatype.convolve :as dt-conv]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l30
 (def
  temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3 3 4 8 13 18 23 26 25 20 14 8 4]))


(def
 v4_l34
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


(def v6_l52 (def G (hm/cyclic-group 24)))


(def v7_l54 (hm/elements G))


(deftest t8_l56 (is (= v7_l54 (range 24))))


(def v10_l61 (hm/op G 15 9))


(deftest t11_l63 (is (= v10_l61 0)))


(def v12_l66 (hm/op G 18 10))


(deftest t13_l68 (is (= v12_l66 4)))


(def v15_l73 (hm/inv G 15))


(deftest t16_l75 (is (= v15_l73 9)))


(def v18_l107 (def ct (hm/character-table G)))


(def
 v20_l111
 (let
  [table (:table ct) n (hm/order G)]
  (every?
   (fn [v] (< (Math/abs (- (cx/cabs v) 1.0)) 1.0E-10))
   (for [k (range n) g (range n)] ((table k) g)))))


(deftest t21_l117 (is (true? v20_l111)))


(def
 v23_l122
 (every?
  (fn*
   [p1__111138#]
   (< (Math/abs (- (cx/re p1__111138#) 1.0)) 1.0E-10))
  ((:table ct) 0)))


(deftest t24_l124 (is (true? v23_l122)))


(def
 v26_l130
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


(def v28_l161 (def signal (cx/complex-tensor-real temperatures)))


(def v29_l163 (def f-hat (hm/fourier-transform ct signal)))


(def v31_l167 (cx/re (f-hat 0)))


(deftest
 t32_l169
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v31_l167)))


(def
 v34_l174
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
 v36_l203
 (let
  [fft-result
   (t/forward-1d (t/transformer :real :fft) temperatures)
   fft-coefficients
   (let
    [data (vec fft-result) n (/ (count data) 2)]
    (cx/complex-tensor
     (mapv (fn [k] (data (* 2 k))) (range n))
     (mapv (fn [k] (data (inc (* 2 k)))) (range n))))]
  (every?
   true?
   (map
    (fn [a b] (< (Math/abs (- a b)) 1.0E-8))
    (take 12 (vec (cx/cabs f-hat)))
    (vec (cx/cabs fft-coefficients))))))


(deftest t37_l213 (is (true? v36_l203)))


(def
 v39_l230
 (def
  orthogonality-data
  (let
   [table (:table ct) sizes (:class-sizes ct) n 24]
   (for
    [j (range 4) k (range 4)]
    {:j j,
     :k k,
     :inner-product-magnitude
     (cx/cabs
      (hm/character-inner-product (table j) (table k) sizes n))}))))


(def
 v40_l241
 (kind/table
  {:column-names ["$j$" "$k$" "$|\\langle\\chi_j, \\chi_k\\rangle|$"],
   :row-vectors
   (mapv
    (fn
     [{:keys [j k inner-product-magnitude]}]
     [j k (format "%.10f" inner-product-magnitude)])
    orthogonality-data)}))


(def
 v42_l249
 (every?
  (fn
   [{:keys [j k inner-product-magnitude]}]
   (if
    (= j k)
    (< (Math/abs (- inner-product-magnitude 1.0)) 1.0E-10)
    (< inner-product-magnitude 1.0E-10)))
  orthogonality-data))


(deftest t43_l255 (is (true? v42_l249)))


(def
 v45_l264
 (let
  [reconstructed (hm/inverse-fourier-transform ct f-hat)]
  (dfn/reduce-max (cx/cabs (cx/csub reconstructed signal)))))


(deftest t46_l266 (is ((fn [err] (< err 1.0E-10)) v45_l264)))


(def
 v48_l285
 (def
  f-fn
  (cx/complex-tensor-real
   [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])))


(def
 v49_l287
 (def
  h-fn
  (cx/complex-tensor-real
   [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v51_l292 (def convolved (hm/convolve ct f-fn h-fn)))


(def
 v52_l294
 (mapv
  (fn* [p1__111139#] (Math/round p1__111139#))
  (vec (cx/re convolved))))


(deftest
 t53_l296
 (is (= v52_l294 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def
 v55_l301
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


(deftest t56_l307 (is (true? v55_l301)))


(def
 v58_l318
 (let
  [mag-s
   (cx/cabs signal)
   mag-f
   (cx/cabs f-hat)
   energy-time
   (dfn/sum (dfn/* mag-s mag-s))
   energy-freq
   (/ (dfn/sum (dfn/* mag-f mag-f)) (double (hm/order G)))]
  (< (Math/abs (- energy-time energy-freq)) 1.0E-8)
  (< (Math/abs (- energy-time energy-freq)) 1.0E-8)))


(deftest t59_l325 (is (true? v58_l318)))


(def
 v61_l335
 (def f-real [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))


(def
 v62_l337
 (def h-real [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))


(def
 v64_l342
 (def
  linear-conv
  (vec
   (dt-conv/convolve1d f-real h-real {:mode :full, :edge-mode :zero}))))


(def v65_l345 (count linear-conv))


(deftest t66_l347 (is (= v65_l345 47)))


(def
 v68_l352
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


(def v69_l361 cyclic-from-linear)


(deftest
 t70_l363
 (is
  ((fn
    [v]
    (=
     (mapv long v)
     [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
   v69_l361)))


(def
 v72_l368
 (let
  [group-conv
   (let
    [f
     (cx/complex-tensor-real f-real)
     h
     (cx/complex-tensor-real h-real)]
    (vec (cx/re (hm/convolve ct f h))))]
  (every?
   (fn* [p1__111140#] (< (Math/abs (double p1__111140#)) 1.0E-10))
   (map - cyclic-from-linear group-conv))))


(deftest t73_l374 (is (true? v72_l368)))
