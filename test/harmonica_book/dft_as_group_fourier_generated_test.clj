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


(def v5_l44 (def G (hm/cyclic-group 24)))


(def v6_l46 (hm/elements G))


(deftest t7_l48 (is (= v6_l46 (range 24))))


(def v9_l53 (hm/op G 15 9))


(deftest t10_l55 (is (= v9_l53 0)))


(def v11_l58 (hm/op G 18 10))


(deftest t12_l60 (is (= v11_l58 4)))


(def v14_l65 (hm/inv G 15))


(deftest t15_l67 (is (= v14_l65 9)))


(def v17_l99 (def ct (hm/character-table G)))


(def
 v19_l103
 (let
  [table (:table ct) n (hm/order G)]
  (every?
   (fn [v] (< (Math/abs (- (cx/cabs v) 1.0)) 1.0E-10))
   (for [k (range n) g (range n)] ((table k) g)))))


(deftest t20_l109 (is (true? v19_l103)))


(def
 v22_l114
 (every?
  (fn* [p1__85614#] (< (Math/abs (- (cx/re p1__85614#) 1.0)) 1.0E-10))
  ((:table ct) 0)))


(deftest t23_l116 (is (true? v22_l114)))


(def
 v25_l122
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


(def v27_l153 (def signal (cx/complex-tensor-real temperatures)))


(def v28_l155 (def f-hat (hm/fourier-transform ct signal)))


(def v30_l159 (cx/re (f-hat 0)))


(deftest
 t31_l161
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v30_l159)))


(def
 v33_l166
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
 v35_l195
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


(deftest t36_l205 (is (true? v35_l195)))


(def
 v38_l222
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
 v39_l233
 (kind/table
  {:column-names ["$j$" "$k$" "$|\\langle\\chi_j, \\chi_k\\rangle|$"],
   :row-vectors
   (mapv
    (fn
     [{:keys [j k inner-product-magnitude]}]
     [j k (format "%.10f" inner-product-magnitude)])
    orthogonality-data)}))


(def
 v41_l241
 (every?
  (fn
   [{:keys [j k inner-product-magnitude]}]
   (if
    (= j k)
    (< (Math/abs (- inner-product-magnitude 1.0)) 1.0E-10)
    (< inner-product-magnitude 1.0E-10)))
  orthogonality-data))


(deftest t42_l247 (is (true? v41_l241)))


(def
 v44_l256
 (let
  [reconstructed (hm/inverse-fourier-transform ct f-hat)]
  (dfn/reduce-max (cx/cabs (cx/csub reconstructed signal)))))


(deftest t45_l258 (is ((fn [err] (< err 1.0E-10)) v44_l256)))


(def
 v47_l277
 (def
  f-fn
  (cx/complex-tensor-real
   [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])))


(def
 v48_l279
 (def
  h-fn
  (cx/complex-tensor-real
   [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v50_l284 (def convolved (hm/convolve ct f-fn h-fn)))


(def
 v51_l286
 (mapv
  (fn* [p1__85615#] (Math/round p1__85615#))
  (vec (cx/re convolved))))


(deftest
 t52_l288
 (is (= v51_l286 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def
 v54_l293
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


(deftest t55_l299 (is (true? v54_l293)))


(def
 v57_l310
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


(deftest t58_l317 (is (true? v57_l310)))


(def
 v60_l327
 (def f-real [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))


(def
 v61_l329
 (def h-real [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))


(def
 v63_l334
 (def
  linear-conv
  (vec
   (dt-conv/convolve1d f-real h-real {:mode :full, :edge-mode :zero}))))


(def v64_l337 (count linear-conv))


(deftest t65_l339 (is (= v64_l337 47)))


(def
 v67_l344
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


(def v68_l353 cyclic-from-linear)


(deftest
 t69_l355
 (is
  ((fn
    [v]
    (=
     (mapv long v)
     [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
   v68_l353)))


(def
 v71_l360
 (let
  [group-conv
   (let
    [f
     (cx/complex-tensor-real f-real)
     h
     (cx/complex-tensor-real h-real)]
    (vec (cx/re (hm/convolve ct f h))))]
  (every?
   (fn* [p1__85616#] (< (Math/abs (double p1__85616#)) 1.0E-10))
   (map - cyclic-from-linear group-conv))))


(deftest t72_l366 (is (true? v71_l360)))
