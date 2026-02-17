(ns
 harmonica-book.dft-as-group-fourier-generated-test
 (:require
  [scicloj.harmonica.core :as hm]
  [fastmath.complex :as c]
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
   (fn [v] (< (Math/abs (- (c/abs v) 1.0)) 1.0E-10))
   (for [k (range n) g (range n)] ((table k) g)))))


(deftest t20_l109 (is (true? v19_l103)))


(def
 v22_l114
 (every?
  (fn* [p1__113369#] (< (Math/abs (- (c/re p1__113369#) 1.0)) 1.0E-10))
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
      :real-part (c/re ((table k) g)),
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


(def
 v27_l153
 (def
  signal
  (mapv
   (fn* [p1__113370#] (c/complex (double p1__113370#)))
   temperatures)))


(def v28_l155 (def f-hat (hm/fourier-transform ct signal)))


(def v30_l159 (c/re (f-hat 0)))


(deftest
 t31_l161
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v30_l159)))


(def
 v33_l166
 (->
  (tc/dataset {:frequency (range 24), :magnitude (mapv c/abs f-hat)})
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
    (mapv
     (fn [k] (c/complex (data (* 2 k)) (data (inc (* 2 k)))))
     (range n)))]
  (every?
   true?
   (map
    (fn [a b] (< (Math/abs (- a b)) 1.0E-8))
    (mapv c/abs (take 12 f-hat))
    (mapv c/abs fft-coefficients)))))


(deftest t36_l207 (is (true? v35_l195)))


(def
 v38_l224
 (def
  orthogonality-data
  (let
   [table (:table ct) sizes (:class-sizes ct) n 24]
   (for
    [j (range 4) k (range 4)]
    {:j j,
     :k k,
     :inner-product-magnitude
     (c/abs
      (hm/character-inner-product (table j) (table k) sizes n))}))))


(def
 v39_l235
 (kind/table
  {:column-names ["$j$" "$k$" "$|\\langle\\chi_j, \\chi_k\\rangle|$"],
   :row-vectors
   (mapv
    (fn
     [{:keys [j k inner-product-magnitude]}]
     [j k (format "%.10f" inner-product-magnitude)])
    orthogonality-data)}))


(def
 v41_l243
 (every?
  (fn
   [{:keys [j k inner-product-magnitude]}]
   (if
    (= j k)
    (< (Math/abs (- inner-product-magnitude 1.0)) 1.0E-10)
    (< inner-product-magnitude 1.0E-10)))
  orthogonality-data))


(deftest t42_l249 (is (true? v41_l243)))


(def
 v44_l258
 (let
  [reconstructed (hm/inverse-fourier-transform ct f-hat)]
  (apply
   max
   (map
    (fn [orig recon] (c/abs (c/sub recon orig)))
    signal
    reconstructed))))


(deftest t45_l264 (is ((fn [err] (< err 1.0E-10)) v44_l258)))


(def
 v47_l283
 (def
  f-fn
  (mapv
   (fn* [p1__113371#] (c/complex (double p1__113371#)))
   [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])))


(def
 v48_l285
 (def
  h-fn
  (mapv
   (fn* [p1__113372#] (c/complex (double p1__113372#)))
   [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v50_l290 (def convolved (hm/convolve ct f-fn h-fn)))


(def
 v51_l292
 (mapv (fn* [p1__113373#] (Math/round (c/re p1__113373#))) convolved))


(deftest
 t52_l294
 (is (= v51_l292 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def
 v54_l300
 (let
  [f-fn-hat
   (hm/fourier-transform ct f-fn)
   h-fn-hat
   (hm/fourier-transform ct h-fn)
   convolved-hat
   (hm/fourier-transform ct convolved)
   pointwise-product
   (mapv c/mult f-fn-hat h-fn-hat)]
  (every?
   true?
   (map
    (fn [a b] (< (c/abs (c/sub a b)) 1.0E-8))
    convolved-hat
    pointwise-product))))


(deftest t55_l309 (is (true? v54_l300)))


(def
 v57_l320
 (let
  [energy-time
   (reduce
    +
    (map
     (fn* [p1__113374#] (let [m (c/abs p1__113374#)] (* m m)))
     signal))
   energy-freq
   (/
    (reduce
     +
     (map
      (fn* [p1__113375#] (let [m (c/abs p1__113375#)] (* m m)))
      f-hat))
    (double (hm/order G)))]
  (< (Math/abs (- energy-time energy-freq)) 1.0E-8)))


(deftest t58_l325 (is (true? v57_l320)))


(def
 v60_l335
 (def f-real [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))


(def
 v61_l337
 (def h-real [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))


(def
 v63_l342
 (def
  linear-conv
  (vec
   (dt-conv/convolve1d f-real h-real {:mode :full, :edge-mode :zero}))))


(def v64_l345 (count linear-conv))


(deftest t65_l347 (is (= v64_l345 47)))


(def
 v67_l352
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


(def v68_l361 cyclic-from-linear)


(deftest
 t69_l363
 (is
  ((fn
    [v]
    (=
     (mapv long v)
     [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
   v68_l361)))


(def
 v71_l368
 (let
  [group-conv
   (let
    [f
     (mapv (fn* [p1__113376#] (c/complex (double p1__113376#))) f-real)
     h
     (mapv
      (fn* [p1__113377#] (c/complex (double p1__113377#)))
      h-real)]
    (mapv
     (fn* [p1__113378#] (c/re p1__113378#))
     (hm/convolve ct f h)))]
  (every?
   (fn* [p1__113379#] (< (Math/abs (double p1__113379#)) 1.0E-10))
   (map - cyclic-from-linear group-conv))))


(deftest t72_l374 (is (true? v71_l368)))
