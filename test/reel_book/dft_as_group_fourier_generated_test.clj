(ns
 reel-book.dft-as-group-fourier-generated-test
 (:require
  [scicloj.reel.core :as reel]
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


(def v5_l42 (def G (reel/cyclic-group 24)))


(def v6_l44 (reel/elements G))


(deftest t7_l46 (is (= v6_l44 (range 24))))


(def v9_l51 (reel/op G 15 9))


(deftest t10_l53 (is (= v9_l51 0)))


(def v11_l56 (reel/op G 18 10))


(deftest t12_l58 (is (= v11_l56 4)))


(def v14_l63 (reel/inv G 15))


(deftest t15_l65 (is (= v14_l63 9)))


(def v17_l90 (def ct (reel/character-table G)))


(def
 v19_l94
 (let
  [table (:table ct) n (reel/order G)]
  (every?
   (fn [v] (< (Math/abs (- (c/abs v) 1.0)) 1.0E-10))
   (for [k (range n) g (range n)] ((table k) g)))))


(deftest t20_l100 (is (true? v19_l94)))


(def
 v22_l105
 (every?
  (fn* [p1__55684#] (< (Math/abs (- (c/re p1__55684#) 1.0)) 1.0E-10))
  ((:table ct) 0)))


(deftest t23_l107 (is (true? v22_l105)))


(def
 v25_l113
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
 v27_l144
 (def
  signal
  (mapv
   (fn* [p1__55685#] (c/complex (double p1__55685#)))
   temperatures)))


(def v28_l146 (def f-hat (reel/fourier-transform ct signal)))


(def v30_l150 (c/re (f-hat 0)))


(deftest
 t31_l152
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v30_l150)))


(def
 v33_l157
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
 v35_l186
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


(deftest t36_l198 (is (true? v35_l186)))


(def
 v38_l215
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
      (reel/character-inner-product (table j) (table k) sizes n))}))))


(def
 v39_l226
 (kind/table
  {:column-names ["$j$" "$k$" "$|\\langle\\chi_j, \\chi_k\\rangle|$"],
   :row-vectors
   (mapv
    (fn
     [{:keys [j k inner-product-magnitude]}]
     [j k (format "%.10f" inner-product-magnitude)])
    orthogonality-data)}))


(def
 v41_l234
 (every?
  (fn
   [{:keys [j k inner-product-magnitude]}]
   (if
    (= j k)
    (< (Math/abs (- inner-product-magnitude 1.0)) 1.0E-10)
    (< inner-product-magnitude 1.0E-10)))
  orthogonality-data))


(deftest t42_l240 (is (true? v41_l234)))


(def
 v44_l249
 (let
  [reconstructed (reel/inverse-fourier-transform ct f-hat)]
  (apply
   max
   (map
    (fn [orig recon] (c/abs (c/sub recon orig)))
    signal
    reconstructed))))


(deftest t45_l255 (is ((fn [err] (< err 1.0E-10)) v44_l249)))


(def
 v47_l271
 (def
  f-fn
  (mapv
   (fn* [p1__55686#] (c/complex (double p1__55686#)))
   [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])))


(def
 v48_l273
 (def
  h-fn
  (mapv
   (fn* [p1__55687#] (c/complex (double p1__55687#)))
   [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v50_l278 (def convolved (reel/convolve ct f-fn h-fn)))


(def
 v51_l280
 (mapv (fn* [p1__55688#] (Math/round (c/re p1__55688#))) convolved))


(deftest
 t52_l282
 (is (= v51_l280 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def
 v54_l288
 (let
  [f-fn-hat
   (reel/fourier-transform ct f-fn)
   h-fn-hat
   (reel/fourier-transform ct h-fn)
   convolved-hat
   (reel/fourier-transform ct convolved)
   pointwise-product
   (mapv c/mult f-fn-hat h-fn-hat)]
  (every?
   true?
   (map
    (fn [a b] (< (c/abs (c/sub a b)) 1.0E-8))
    convolved-hat
    pointwise-product))))


(deftest t55_l297 (is (true? v54_l288)))


(def
 v57_l306
 (let
  [energy-time
   (reduce
    +
    (map
     (fn* [p1__55689#] (let [m (c/abs p1__55689#)] (* m m)))
     signal))
   energy-freq
   (/
    (reduce
     +
     (map
      (fn* [p1__55690#] (let [m (c/abs p1__55690#)] (* m m)))
      f-hat))
    (double (reel/order G)))]
  (< (Math/abs (- energy-time energy-freq)) 1.0E-8)))


(deftest t58_l311 (is (true? v57_l306)))


(def
 v60_l321
 (def f-real [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))


(def
 v61_l323
 (def h-real [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))


(def
 v63_l328
 (def
  linear-conv
  (vec
   (dt-conv/convolve1d f-real h-real {:mode :full, :edge-mode :zero}))))


(def v64_l331 (count linear-conv))


(deftest t65_l333 (is (= v64_l331 47)))


(def
 v67_l338
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


(def v68_l347 cyclic-from-linear)


(deftest
 t69_l349
 (is
  ((fn
    [v]
    (=
     (mapv long v)
     [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
   v68_l347)))


(def
 v71_l354
 (let
  [group-conv
   (let
    [f
     (mapv (fn* [p1__55691#] (c/complex (double p1__55691#))) f-real)
     h
     (mapv (fn* [p1__55692#] (c/complex (double p1__55692#))) h-real)]
    (mapv
     (fn* [p1__55693#] (c/re p1__55693#))
     (reel/convolve ct f h)))]
  (every?
   (fn* [p1__55694#] (< (Math/abs (double p1__55694#)) 1.0E-10))
   (map - cyclic-from-linear group-conv))))


(deftest t72_l360 (is (true? v71_l354)))
