(ns
 reel-book.dft-as-group-fourier-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [fastmath.complex :as c]
  [fastmath.transform :as t]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l29 (def temperatures [20 22 25 23 21 19 18 20]))


(def v5_l37 (def Z8 (reel/cyclic-group 8)))


(def v6_l39 (reel/elements Z8))


(deftest t7_l41 (is (= v6_l39 (range 8))))


(def v9_l46 (reel/op Z8 3 5))


(deftest t10_l48 (is (= v9_l46 0)))


(def v11_l51 (reel/op Z8 6 5))


(deftest t12_l53 (is (= v11_l51 3)))


(def v14_l58 (reel/inv Z8 3))


(deftest t15_l60 (is (= v14_l58 5)))


(def v17_l85 (def ct (reel/character-table Z8)))


(def
 v19_l90
 (def
  ct-display-data
  (let
   [table (:table ct) n (reel/order Z8)]
   (for
    [k (range n) g (range n)]
    {:k k,
     :g g,
     :re (c/re ((table k) g)),
     :im (c/im ((table k) g)),
     :magnitude (c/abs ((table k) g))}))))


(def
 v21_l102
 (every?
  (fn*
   [p1__71999#]
   (< (Math/abs (- (:magnitude p1__71999#) 1.0)) 1.0E-10))
  ct-display-data))


(deftest t22_l104 (is (true? v21_l102)))


(def v24_l109 (mapv c/re ((:table ct) 0)))


(deftest t25_l111 (is (= v24_l109 [1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0])))


(def
 v27_l117
 (def
  character-plot-data
  (let
   [table (:table ct)]
   (tc/dataset
    (for
     [k [0 1 2 3] g (range 8)]
     {:element g,
      :real-part (c/re ((table k) g)),
      :character (str "chi_" k)})))))


(def
 v28_l126
 (->
  character-plot-data
  (plotly/base
   {:=x :element,
    :=y :real-part,
    :=color :character,
    :=x-title "Group element g",
    :=y-title "Re(chi_k(g))",
    :=title "Characters of Z/8Z — real parts (cosine components)"})
  (plotly/layer-line)
  (plotly/layer-point {:=mark-size 8})
  plotly/plot))


(def
 v30_l149
 (def
  signal
  (mapv
   (fn* [p1__72000#] (c/complex (double p1__72000#)))
   temperatures)))


(def v31_l151 (def f-hat (reel/fourier-transform ct signal)))


(def v33_l155 (c/re (f-hat 0)))


(deftest
 t34_l157
 (is ((fn [v] (< (Math/abs (- v 168.0)) 1.0E-10)) v33_l155)))


(def
 v36_l162
 (def
  magnitude-data
  (tc/dataset {:frequency (range 8), :magnitude (mapv c/abs f-hat)})))


(def
 v37_l167
 (->
  magnitude-data
  (plotly/base
   {:=x :frequency,
    :=y :magnitude,
    :=x-title "Frequency k (character index)",
    :=y-title "|f-hat(k)|",
    :=title "Fourier spectrum of temperatures on Z/8Z"})
  (plotly/layer-line)
  (plotly/layer-point {:=mark-size 8})
  plotly/plot))


(def v39_l185 (def fft-transformer (t/transformer :real :fft)))


(def
 v40_l186
 (def fft-result (t/forward-1d fft-transformer temperatures)))


(def
 v42_l192
 (def
  fft-coefficients
  (let
   [data (vec fft-result) n (/ (count data) 2)]
   (mapv
    (fn [k] (c/complex (data (* 2 k)) (data (inc (* 2 k)))))
    (range n)))))


(def v44_l202 (def our-magnitudes (mapv c/abs (take 4 f-hat))))


(def v45_l203 (def fft-magnitudes (mapv c/abs fft-coefficients)))


(def
 v46_l205
 (every?
  true?
  (map
   (fn [a b] (< (Math/abs (- a b)) 1.0E-8))
   our-magnitudes
   fft-magnitudes)))


(deftest t47_l210 (is (true? v46_l205)))


(def
 v49_l227
 (def
  orthogonality-data
  (let
   [table (:table ct) sizes (:class-sizes ct) n 8]
   (for
    [j (range 4) k (range 4)]
    {:j j,
     :k k,
     :inner-product-magnitude
     (c/abs
      (reel/character-inner-product (table j) (table k) sizes n))}))))


(def
 v50_l238
 (kind/table
  {:column-names ["j" "k" "|<chi_j, chi_k>|"],
   :row-vectors
   (mapv
    (fn
     [{:keys [j k inner-product-magnitude]}]
     [j k (format "%.10f" inner-product-magnitude)])
    orthogonality-data)}))


(def
 v52_l246
 (every?
  (fn
   [{:keys [j k inner-product-magnitude]}]
   (if
    (= j k)
    (< (Math/abs (- inner-product-magnitude 1.0)) 1.0E-10)
    (< inner-product-magnitude 1.0E-10)))
  orthogonality-data))


(deftest t53_l252 (is (true? v52_l246)))


(def
 v55_l261
 (def reconstructed (reel/inverse-fourier-transform ct f-hat)))


(def
 v57_l265
 (def
  max-reconstruction-error
  (apply
   max
   (map
    (fn [orig recon] (c/abs (c/sub recon orig)))
    signal
    reconstructed))))


(def v58_l271 max-reconstruction-error)


(deftest t59_l273 (is ((fn [err] (< err 1.0E-10)) v58_l271)))


(def
 v61_l289
 (def
  f-fn
  (mapv
   (fn* [p1__72001#] (c/complex (double p1__72001#)))
   [1 2 0 0 0 0 0 3])))


(def
 v62_l290
 (def
  h-fn
  (mapv
   (fn* [p1__72002#] (c/complex (double p1__72002#)))
   [0 1 1 0 0 0 0 0])))


(def v64_l294 (def convolved (reel/convolve ct f-fn h-fn)))


(def
 v65_l296
 (mapv (fn* [p1__72003#] (Math/round (c/re p1__72003#))) convolved))


(deftest t66_l298 (is (= v65_l296 [3 4 3 2 0 0 0 0])))


(def v68_l304 (def f-fn-hat (reel/fourier-transform ct f-fn)))


(def v69_l305 (def h-fn-hat (reel/fourier-transform ct h-fn)))


(def v70_l306 (def convolved-hat (reel/fourier-transform ct convolved)))


(def v71_l307 (def pointwise-product (mapv c/mult f-fn-hat h-fn-hat)))


(def
 v72_l309
 (every?
  true?
  (map
   (fn [a b] (< (c/abs (c/sub a b)) 1.0E-8))
   convolved-hat
   pointwise-product)))


(deftest t73_l314 (is (true? v72_l309)))


(def
 v75_l323
 (def
  energy-time-domain
  (reduce
   +
   (map
    (fn* [p1__72004#] (let [m (c/abs p1__72004#)] (* m m)))
    signal))))


(def
 v76_l326
 (def
  energy-freq-domain
  (/
   (reduce
    +
    (map
     (fn* [p1__72005#] (let [m (c/abs p1__72005#)] (* m m)))
     f-hat))
   (double (reel/order Z8)))))


(def
 v77_l330
 (< (Math/abs (- energy-time-domain energy-freq-domain)) 1.0E-8))


(deftest t78_l332 (is (true? v77_l330)))
