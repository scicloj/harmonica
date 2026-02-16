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
 v19_l94
 (def
  ct-display-data
  (let
   [table (:table ct) n (reel/order G)]
   (for
    [k (range n) g (range n)]
    {:k k,
     :g g,
     :re (c/re ((table k) g)),
     :im (c/im ((table k) g)),
     :magnitude (c/abs ((table k) g))}))))


(def
 v21_l106
 (every?
  (fn*
   [p1__73877#]
   (< (Math/abs (- (:magnitude p1__73877#) 1.0)) 1.0E-10))
  ct-display-data))


(deftest t22_l108 (is (true? v21_l106)))


(def
 v24_l113
 (every?
  (fn* [p1__73878#] (< (Math/abs (- (c/re p1__73878#) 1.0)) 1.0E-10))
  ((:table ct) 0)))


(deftest t25_l115 (is (true? v24_l113)))


(def
 v27_l121
 (def
  character-plot-data
  (let
   [table (:table ct)]
   (tc/dataset
    (for
     [k [0 1 2 3] g (range 24)]
     {:month g,
      :real-part (c/re ((table k) g)),
      :character (str "chi_" k)})))))


(def
 v28_l130
 (->
  character-plot-data
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
 v30_l155
 (def
  signal
  (mapv
   (fn* [p1__73879#] (c/complex (double p1__73879#)))
   temperatures)))


(def v31_l157 (def f-hat (reel/fourier-transform ct signal)))


(def v33_l161 (c/re (f-hat 0)))


(deftest
 t34_l163
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v33_l161)))


(def
 v36_l168
 (def
  magnitude-data
  (tc/dataset {:frequency (range 24), :magnitude (mapv c/abs f-hat)})))


(def
 v37_l173
 (->
  magnitude-data
  (plotly/base
   {:=x :frequency,
    :=y :magnitude,
    :=x-title "Frequency k (character index)",
    :=y-title "|f-hat(k)|",
    :=title "Fourier spectrum of monthly temperatures on Z/24Z"})
  (plotly/layer-line)
  (plotly/layer-point {:=mark-size 6})
  plotly/plot))


(def v39_l196 (def fft-transformer (t/transformer :real :fft)))


(def
 v40_l197
 (def fft-result (t/forward-1d fft-transformer temperatures)))


(def
 v42_l203
 (def
  fft-coefficients
  (let
   [data (vec fft-result) n (/ (count data) 2)]
   (mapv
    (fn [k] (c/complex (data (* 2 k)) (data (inc (* 2 k)))))
    (range n)))))


(def v44_l213 (def our-magnitudes (mapv c/abs (take 12 f-hat))))


(def v45_l214 (def fft-magnitudes (mapv c/abs fft-coefficients)))


(def
 v46_l216
 (every?
  true?
  (map
   (fn [a b] (< (Math/abs (- a b)) 1.0E-8))
   our-magnitudes
   fft-magnitudes)))


(deftest t47_l221 (is (true? v46_l216)))


(def
 v49_l238
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
 v50_l249
 (kind/table
  {:column-names ["j" "k" "|<chi_j, chi_k>|"],
   :row-vectors
   (mapv
    (fn
     [{:keys [j k inner-product-magnitude]}]
     [j k (format "%.10f" inner-product-magnitude)])
    orthogonality-data)}))


(def
 v52_l257
 (every?
  (fn
   [{:keys [j k inner-product-magnitude]}]
   (if
    (= j k)
    (< (Math/abs (- inner-product-magnitude 1.0)) 1.0E-10)
    (< inner-product-magnitude 1.0E-10)))
  orthogonality-data))


(deftest t53_l263 (is (true? v52_l257)))


(def
 v55_l272
 (def reconstructed (reel/inverse-fourier-transform ct f-hat)))


(def
 v57_l276
 (def
  max-reconstruction-error
  (apply
   max
   (map
    (fn [orig recon] (c/abs (c/sub recon orig)))
    signal
    reconstructed))))


(def v58_l282 max-reconstruction-error)


(deftest t59_l284 (is ((fn [err] (< err 1.0E-10)) v58_l282)))


(def
 v61_l300
 (def
  f-fn
  (mapv
   (fn* [p1__73880#] (c/complex (double p1__73880#)))
   [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])))


(def
 v62_l302
 (def
  h-fn
  (mapv
   (fn* [p1__73881#] (c/complex (double p1__73881#)))
   [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v64_l307 (def convolved (reel/convolve ct f-fn h-fn)))


(def
 v65_l309
 (mapv (fn* [p1__73882#] (Math/round (c/re p1__73882#))) convolved))


(deftest
 t66_l311
 (is (= v65_l309 [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))


(def v68_l317 (def f-fn-hat (reel/fourier-transform ct f-fn)))


(def v69_l318 (def h-fn-hat (reel/fourier-transform ct h-fn)))


(def v70_l319 (def convolved-hat (reel/fourier-transform ct convolved)))


(def v71_l320 (def pointwise-product (mapv c/mult f-fn-hat h-fn-hat)))


(def
 v72_l322
 (every?
  true?
  (map
   (fn [a b] (< (c/abs (c/sub a b)) 1.0E-8))
   convolved-hat
   pointwise-product)))


(deftest t73_l327 (is (true? v72_l322)))


(def
 v75_l336
 (def
  energy-time-domain
  (reduce
   +
   (map
    (fn* [p1__73883#] (let [m (c/abs p1__73883#)] (* m m)))
    signal))))


(def
 v76_l339
 (def
  energy-freq-domain
  (/
   (reduce
    +
    (map
     (fn* [p1__73884#] (let [m (c/abs p1__73884#)] (* m m)))
     f-hat))
   (double (reel/order G)))))


(def
 v77_l343
 (< (Math/abs (- energy-time-domain energy-freq-domain)) 1.0E-8))


(deftest t78_l345 (is (true? v77_l343)))


(def
 v80_l355
 (def f-real [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))


(def
 v81_l357
 (def h-real [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))


(def
 v83_l362
 (def
  linear-conv
  (vec
   (dt-conv/convolve1d f-real h-real {:mode :full, :edge-mode :zero}))))


(def v84_l365 (count linear-conv))


(deftest t85_l367 (is (= v84_l365 47)))


(def
 v87_l372
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


(def v88_l381 cyclic-from-linear)


(deftest
 t89_l383
 (is
  ((fn
    [v]
    (=
     (mapv long v)
     [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
   v88_l381)))


(def
 v91_l388
 (def
  group-conv
  (let
   [f
    (mapv (fn* [p1__73885#] (c/complex (double p1__73885#))) f-real)
    h
    (mapv (fn* [p1__73886#] (c/complex (double p1__73886#))) h-real)]
   (mapv (fn* [p1__73887#] (c/re p1__73887#)) (reel/convolve ct f h)))))


(def
 v93_l395
 (every?
  (fn* [p1__73888#] (< (Math/abs (double p1__73888#)) 1.0E-10))
  (map - cyclic-from-linear group-conv)))


(deftest t94_l398 (is (true? v93_l395)))
