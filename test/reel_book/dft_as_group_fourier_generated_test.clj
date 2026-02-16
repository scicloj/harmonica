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


(def v3_l30 (def temperatures [20 22 25 23 21 19 18 20]))


(def v5_l38 (def Z8 (reel/cyclic-group 8)))


(def v6_l40 (reel/elements Z8))


(deftest t7_l42 (is (= v6_l40 (range 8))))


(def v9_l47 (reel/op Z8 3 5))


(deftest t10_l49 (is (= v9_l47 0)))


(def v11_l52 (reel/op Z8 6 5))


(deftest t12_l54 (is (= v11_l52 3)))


(def v14_l59 (reel/inv Z8 3))


(deftest t15_l61 (is (= v14_l59 5)))


(def v17_l86 (def ct (reel/character-table Z8)))


(def
 v19_l91
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
 v21_l103
 (every?
  (fn*
   [p1__72937#]
   (< (Math/abs (- (:magnitude p1__72937#) 1.0)) 1.0E-10))
  ct-display-data))


(deftest t22_l105 (is (true? v21_l103)))


(def v24_l110 (mapv c/re ((:table ct) 0)))


(deftest t25_l112 (is (= v24_l110 [1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0])))


(def
 v27_l118
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
 v28_l127
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
 v30_l150
 (def
  signal
  (mapv
   (fn* [p1__72938#] (c/complex (double p1__72938#)))
   temperatures)))


(def v31_l152 (def f-hat (reel/fourier-transform ct signal)))


(def v33_l156 (c/re (f-hat 0)))


(deftest
 t34_l158
 (is ((fn [v] (< (Math/abs (- v 168.0)) 1.0E-10)) v33_l156)))


(def
 v36_l163
 (def
  magnitude-data
  (tc/dataset {:frequency (range 8), :magnitude (mapv c/abs f-hat)})))


(def
 v37_l168
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


(def v39_l186 (def fft-transformer (t/transformer :real :fft)))


(def
 v40_l187
 (def fft-result (t/forward-1d fft-transformer temperatures)))


(def
 v42_l193
 (def
  fft-coefficients
  (let
   [data (vec fft-result) n (/ (count data) 2)]
   (mapv
    (fn [k] (c/complex (data (* 2 k)) (data (inc (* 2 k)))))
    (range n)))))


(def v44_l203 (def our-magnitudes (mapv c/abs (take 4 f-hat))))


(def v45_l204 (def fft-magnitudes (mapv c/abs fft-coefficients)))


(def
 v46_l206
 (every?
  true?
  (map
   (fn [a b] (< (Math/abs (- a b)) 1.0E-8))
   our-magnitudes
   fft-magnitudes)))


(deftest t47_l211 (is (true? v46_l206)))


(def
 v49_l228
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
 v50_l239
 (kind/table
  {:column-names ["j" "k" "|<chi_j, chi_k>|"],
   :row-vectors
   (mapv
    (fn
     [{:keys [j k inner-product-magnitude]}]
     [j k (format "%.10f" inner-product-magnitude)])
    orthogonality-data)}))


(def
 v52_l247
 (every?
  (fn
   [{:keys [j k inner-product-magnitude]}]
   (if
    (= j k)
    (< (Math/abs (- inner-product-magnitude 1.0)) 1.0E-10)
    (< inner-product-magnitude 1.0E-10)))
  orthogonality-data))


(deftest t53_l253 (is (true? v52_l247)))


(def
 v55_l262
 (def reconstructed (reel/inverse-fourier-transform ct f-hat)))


(def
 v57_l266
 (def
  max-reconstruction-error
  (apply
   max
   (map
    (fn [orig recon] (c/abs (c/sub recon orig)))
    signal
    reconstructed))))


(def v58_l272 max-reconstruction-error)


(deftest t59_l274 (is ((fn [err] (< err 1.0E-10)) v58_l272)))


(def
 v61_l290
 (def
  f-fn
  (mapv
   (fn* [p1__72939#] (c/complex (double p1__72939#)))
   [1 2 0 0 0 0 0 3])))


(def
 v62_l291
 (def
  h-fn
  (mapv
   (fn* [p1__72940#] (c/complex (double p1__72940#)))
   [0 1 1 0 0 0 0 0])))


(def v64_l295 (def convolved (reel/convolve ct f-fn h-fn)))


(def
 v65_l297
 (mapv (fn* [p1__72941#] (Math/round (c/re p1__72941#))) convolved))


(deftest t66_l299 (is (= v65_l297 [3 4 3 2 0 0 0 0])))


(def v68_l305 (def f-fn-hat (reel/fourier-transform ct f-fn)))


(def v69_l306 (def h-fn-hat (reel/fourier-transform ct h-fn)))


(def v70_l307 (def convolved-hat (reel/fourier-transform ct convolved)))


(def v71_l308 (def pointwise-product (mapv c/mult f-fn-hat h-fn-hat)))


(def
 v72_l310
 (every?
  true?
  (map
   (fn [a b] (< (c/abs (c/sub a b)) 1.0E-8))
   convolved-hat
   pointwise-product)))


(deftest t73_l315 (is (true? v72_l310)))


(def
 v75_l324
 (def
  energy-time-domain
  (reduce
   +
   (map
    (fn* [p1__72942#] (let [m (c/abs p1__72942#)] (* m m)))
    signal))))


(def
 v76_l327
 (def
  energy-freq-domain
  (/
   (reduce
    +
    (map
     (fn* [p1__72943#] (let [m (c/abs p1__72943#)] (* m m)))
     f-hat))
   (double (reel/order Z8)))))


(def
 v77_l331
 (< (Math/abs (- energy-time-domain energy-freq-domain)) 1.0E-8))


(deftest t78_l333 (is (true? v77_l331)))


(def v80_l344 (def f-real [1 2 0 0 0 0 0 3]))


(def v81_l345 (def h-real [0 1 1 0 0 0 0 0]))


(def
 v83_l349
 (def
  linear-conv
  (vec
   (dt-conv/convolve1d f-real h-real {:mode :full, :edge-mode :zero}))))


(def v84_l352 linear-conv)


(deftest t85_l354 (is ((fn [v] (= (count v) 15)) v84_l352)))


(def
 v87_l359
 (def
  cyclic-from-linear
  (let
   [n 8]
   (mapv
    (fn
     [i]
     (+
      (linear-conv i)
      (if (< (+ i n) (count linear-conv)) (linear-conv (+ i n)) 0.0)))
    (range n)))))


(def v88_l368 cyclic-from-linear)


(deftest
 t89_l370
 (is ((fn [v] (= (mapv long v) [3 4 3 2 0 0 0 0])) v88_l368)))


(def
 v91_l375
 (def
  group-conv
  (let
   [f
    (mapv (fn* [p1__72944#] (c/complex (double p1__72944#))) f-real)
    h
    (mapv (fn* [p1__72945#] (c/complex (double p1__72945#))) h-real)]
   (mapv (fn* [p1__72946#] (c/re p1__72946#)) (reel/convolve ct f h)))))


(def
 v93_l382
 (every?
  (fn* [p1__72947#] (< (Math/abs (double p1__72947#)) 1.0E-10))
  (map - cyclic-from-linear group-conv)))


(deftest t94_l385 (is (true? v93_l382)))
