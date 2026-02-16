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


(def v5_l41 (def G (reel/cyclic-group 24)))


(def v6_l43 (reel/elements G))


(deftest t7_l45 (is (= v6_l43 (range 24))))


(def v9_l50 (reel/op G 15 9))


(deftest t10_l52 (is (= v9_l50 0)))


(def v11_l55 (reel/op G 18 10))


(deftest t12_l57 (is (= v11_l55 4)))


(def v14_l62 (reel/inv G 15))


(deftest t15_l64 (is (= v14_l62 9)))


(def v17_l89 (def ct (reel/character-table G)))


(def
 v19_l93
 (let
  [table (:table ct) n (reel/order G)]
  (every?
   (fn [v] (< (Math/abs (- (c/abs v) 1.0)) 1.0E-10))
   (for [k (range n) g (range n)] ((table k) g)))))


(deftest t20_l99 (is (true? v19_l93)))


(def
 v22_l104
 (every?
  (fn* [p1__74209#] (< (Math/abs (- (c/re p1__74209#) 1.0)) 1.0E-10))
  ((:table ct) 0)))


(deftest t23_l106 (is (true? v22_l104)))


(def
 v25_l112
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
 v27_l143
 (def
  signal
  (mapv
   (fn* [p1__74210#] (c/complex (double p1__74210#)))
   temperatures)))


(def v28_l145 (def f-hat (reel/fourier-transform ct signal)))


(def v30_l149 (c/re (f-hat 0)))


(deftest
 t31_l151
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v30_l149)))


(def
 v33_l156
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
 v35_l185
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


(deftest t36_l197 (is (true? v35_l185)))


(def
 v38_l214
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
 v39_l225
 (kind/table
  {:column-names ["j" "k" "|<chi_j, chi_k>|"],
   :row-vectors
   (mapv
    (fn
     [{:keys [j k inner-product-magnitude]}]
     [j k (format "%.10f" inner-product-magnitude)])
    orthogonality-data)}))


(def
 v41_l233
 (every?
  (fn
   [{:keys [j k inner-product-magnitude]}]
   (if
    (= j k)
    (< (Math/abs (- inner-product-magnitude 1.0)) 1.0E-10)
    (< inner-product-magnitude 1.0E-10)))
  orthogonality-data))


(deftest t42_l239 (is (true? v41_l233)))


(def
 v44_l248
 (let
  [reconstructed (reel/inverse-fourier-transform ct f-hat)]
  (apply
   max
   (map
    (fn [orig recon] (c/abs (c/sub recon orig)))
    signal
    reconstructed))))


(deftest t45_l254 (is ((fn [err] (< err 1.0E-10)) v44_l248)))


(def
 v47_l270
 (def
  f-fn
  (mapv
   (fn* [p1__74211#] (c/complex (double p1__74211#)))
   [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])))


(def
 v48_l272
 (def
  h-fn
  (mapv
   (fn* [p1__74212#] (c/complex (double p1__74212#)))
   [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v50_l277 (def convolved (reel/convolve ct f-fn h-fn)))


(def
 v51_l279
 (mapv (fn* [p1__74213#] (Math/round (c/re p1__74213#))) convolved))


(deftest
 t52_l281
 (is (= v51_l279 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def
 v54_l287
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


(deftest t55_l296 (is (true? v54_l287)))


(def
 v57_l305
 (let
  [energy-time
   (reduce
    +
    (map
     (fn* [p1__74214#] (let [m (c/abs p1__74214#)] (* m m)))
     signal))
   energy-freq
   (/
    (reduce
     +
     (map
      (fn* [p1__74215#] (let [m (c/abs p1__74215#)] (* m m)))
      f-hat))
    (double (reel/order G)))]
  (< (Math/abs (- energy-time energy-freq)) 1.0E-8)))


(deftest t58_l310 (is (true? v57_l305)))


(def
 v60_l320
 (def f-real [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))


(def
 v61_l322
 (def h-real [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))


(def
 v63_l327
 (def
  linear-conv
  (vec
   (dt-conv/convolve1d f-real h-real {:mode :full, :edge-mode :zero}))))


(def v64_l330 (count linear-conv))


(deftest t65_l332 (is (= v64_l330 47)))


(def
 v67_l337
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


(def v68_l346 cyclic-from-linear)


(deftest
 t69_l348
 (is
  ((fn
    [v]
    (=
     (mapv long v)
     [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
   v68_l346)))


(def
 v71_l353
 (let
  [group-conv
   (let
    [f
     (mapv (fn* [p1__74216#] (c/complex (double p1__74216#))) f-real)
     h
     (mapv (fn* [p1__74217#] (c/complex (double p1__74217#))) h-real)]
    (mapv
     (fn* [p1__74218#] (c/re p1__74218#))
     (reel/convolve ct f h)))]
  (every?
   (fn* [p1__74219#] (< (Math/abs (double p1__74219#)) 1.0E-10))
   (map - cyclic-from-linear group-conv))))


(deftest t72_l359 (is (true? v71_l353)))
