(ns
 harmonica-book.random-walks-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [harmonica-book.book-helpers :refer [allclose?]]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [tech.v3.datatype.convolve :as dt-conv]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l48
 (defn
  gaussian-pdf
  "Gaussian density at x with given mean and standard deviation."
  [mu sigma x]
  (let
   [z (/ (- x mu) sigma)]
   (/ (Math/exp (* -0.5 z z)) (* sigma (Math/sqrt (* 2.0 Math/PI)))))))


(def
 v5_l59
 (defn
  gaussian-kernel
  "Sampled Gaussian kernel (integrates to ~1) for numerical convolution."
  [sigma dx]
  (let
   [hw (long (/ (* 4 sigma) dx)) ks (range (- hw) (inc hw))]
   (double-array
    (map
     (fn [ki] (let [x (* ki dx)] (* dx (gaussian-pdf 0 sigma x))))
     ks)))))


(def
 v6_l69
 (let
  [xs
   (vec (range -5.0 5.01 0.05))
   dx
   0.05
   raw
   (double-array
    (map
     (fn
      [x]
      (+
       (* 0.5 (Math/exp (* -8.0 (* (+ x 2.0) (+ x 2.0)))))
       (* 0.35 (Math/exp (* -12.0 (* (- x 0.5) (- x 0.5)))))
       (* 0.15 (Math/exp (* -3.0 (* (- x 2.5) (- x 2.5)))))))
     xs))
   signal
   (dfn// raw (* dx (dfn/sum raw)))
   rows
   (vec
    (concat
     (for
      [i (range (count xs))]
      {:x (xs i), :density (signal i), :curve "original"})
     (for
      [sigma
       [0.3 0.6 1.2]
       :let
       [smoothed
        (dt-conv/convolve1d
         signal
         (gaussian-kernel sigma dx)
         {:mode :same})]
       i
       (range (count xs))]
      {:x (xs i),
       :density (smoothed i),
       :curve (str "smoothed, σ=" sigma)})))]
  (->
   (tc/dataset rows)
   (plotly/base
    {:=x :x,
     :=y :density,
     :=color :curve,
     :=title "Convolution with a Gaussian smooths",
     :=x-title "x",
     :=y-title "density"})
   (plotly/layer-line)
   plotly/plot)))


(def
 v8_l107
 (defn
  convolve-2d
  "Convolve a 2D grid (vec of vecs) with a Gaussian of width sigma,\n   using separable 1D convolution (rows then columns)."
  [grid sigma dx]
  (let
   [n
    (count grid)
    k
    (gaussian-kernel sigma dx)
    row-conv
    (mapv
     (fn
      [row]
      (vec (dt-conv/convolve1d (double-array row) k {:mode :same})))
     grid)]
   (let
    [col-conv
     (mapv
      (fn
       [j]
       (vec
        (dt-conv/convolve1d
         (double-array
          (map (fn* [p1__66682#] (nth p1__66682# j)) row-conv))
         k
         {:mode :same})))
      (range n))]
    (vec
     (for [i (range n)] (vec (for [j (range n)] ((col-conv j) i)))))))))


(def
 v9_l125
 (let
  [xs
   (vec (range -3.0 3.1 0.25))
   dx
   0.25
   raw
   (vec
    (for
     [y xs]
     (vec
      (for
       [x xs]
       (+
        (*
         0.5
         (Math/exp
          (- (+ (* 5 (+ x 1.0) (+ x 1.0)) (* 5 (- y 0.8) (- y 0.8))))))
        (*
         0.35
         (Math/exp
          (-
           (+ (* 10 (- x 1.2) (- x 1.2)) (* 10 (+ y 1.0) (+ y 1.0))))))
        (*
         0.2
         (Math/exp
          (- (+ (* 3 (* x x)) (* 8 (- y 1.5) (- y 1.5)))))))))))
   z1
   raw
   z2
   (convolve-2d raw 0.5 dx)
   z3
   (convolve-2d raw 1.2 dx)
   zmax
   (apply max (flatten raw))]
  (kind/plotly
   {:data
    [{:y xs,
      :colorscale "Viridis",
      :type "heatmap",
      :showscale false,
      :xaxis "x",
      :z z1,
      :yaxis "y",
      :zmax zmax,
      :x xs,
      :zmin 0}
     {:y xs,
      :colorscale "Viridis",
      :type "heatmap",
      :showscale false,
      :xaxis "x2",
      :z z2,
      :yaxis "y2",
      :zmax zmax,
      :x xs,
      :zmin 0}
     {:y xs,
      :colorscale "Viridis",
      :type "heatmap",
      :showscale false,
      :xaxis "x3",
      :z z3,
      :yaxis "y3",
      :zmax zmax,
      :x xs,
      :zmin 0}],
    :layout
    {:xaxis3 {:domain [0.7 1], :visible false},
     :grid {:rows 1, :columns 3, :pattern "independent"},
     :xaxis2 {:domain [0.35 0.65], :visible false},
     :width 700,
     :xaxis {:domain [0 0.3], :visible false},
     :title "2D smoothing: original → σ=0.5 → σ=1.2",
     :yaxis {:scaleanchor "x", :visible false},
     :yaxis3 {:scaleanchor "x3", :visible false},
     :yaxis2 {:scaleanchor "x2", :visible false},
     :height 270,
     :margin {:t 40, :b 20, :l 20, :r 20}}})))


(def v11_l177 (def n 24))


(def v12_l178 (def G (hm/cyclic-group n)))


(def v13_l179 (def ct (hm/character-table G)))


(def
 v15_l185
 (def
  step-dist
  (t/complex-tensor-real
   (vec
    (for
     [g (range n)]
     (case g 0 (/ 1.0 3) 1 (/ 1.0 3) 23 (/ 1.0 3) 0.0))))))


(def v17_l197 (dfn/sum (el/re step-dist)))


(deftest
 t18_l199
 (is ((fn [v] (< (Math/abs (- v 1.0)) 1.0E-10)) v17_l197)))


(def
 v20_l205
 (defn
  make-delta
  "Point mass at position 0 on a group of order n."
  [n]
  (t/complex-tensor-real (vec (cons 1.0 (repeat (dec n) 0.0))))))


(def v21_l211 (def delta-0 (make-delta n)))


(def
 v23_l217
 (defn
  walk-distributions
  "Compute distributions at each step up to t-max, returning a vector."
  [ct step-dist n t-max]
  (loop
   [dist (make-delta n) t 0 acc []]
   (if
    (> t t-max)
    acc
    (recur (hm/convolve ct dist step-dist) (inc t) (conj acc dist))))))


(def
 v25_l227
 (let
  [dists
   (walk-distributions ct step-dist n 100)
   steps
   [0 1 3 10 30 100]
   rows
   (vec
    (for
     [t steps g (range n)]
     {:position g,
      :probability (el/re ((dists t) g)),
      :steps (str "t=" t)}))]
  (->
   (tc/dataset rows)
   (plotly/base
    {:=x :position,
     :=y :probability,
     :=color :steps,
     :=title "Random walk on Z/24Z — distribution at various times",
     :=x-title "position",
     :=y-title "probability"})
   (plotly/layer-line)
   (plotly/layer-point {:=mark-size 8})
   plotly/plot)))


(def v27_l255 (def uniform (vec (repeat n (/ 1.0 n)))))


(def
 v28_l257
 (let
  [dists
   (walk-distributions ct step-dist n 200)
   tv-data
   (vec
    (map-indexed
     (fn
      [t dist]
      {:step t,
       :tv-distance
       (hm/total-variation-distance (el/re dist) uniform)})
     dists))]
  (->
   (tc/dataset tv-data)
   (plotly/base
    {:=x :step,
     :=y :tv-distance,
     :=title "Total variation distance to uniform — Z/24Z",
     :=x-title "steps",
     :=y-title "TV distance"})
   (plotly/layer-line)
   plotly/plot)))


(def v30_l274 (hm/total-variation-distance (el/re delta-0) uniform))


(deftest
 t31_l276
 (is
  ((fn [v] (< (Math/abs (- v (- 1.0 (/ 1.0 24)))) 1.0E-10)) v30_l274)))


(def
 v33_l281
 (let
  [dists (walk-distributions ct step-dist n 200)]
  (hm/total-variation-distance (el/re (dists 200)) uniform)))


(deftest t34_l284 (is ((fn [v] (< v 0.01)) v33_l281)))


(def v36_l302 (def step-hat (hm/fourier-transform ct step-dist)))


(def
 v38_l306
 (let
  [rows
   (vec (for [k (range n)] {:k k, :magnitude (el/abs (step-hat k))}))]
  (->
   (tc/dataset rows)
   (plotly/base
    {:=x :k,
     :=y :magnitude,
     :=title "Fourier spectrum of the step distribution",
     :=x-title "frequency k",
     :=y-title "|μ̂(k)|"})
   (plotly/layer-bar)
   plotly/plot)))


(def v40_l319 (el/abs (step-hat 0)))


(deftest
 t41_l321
 (is ((fn [v] (< (Math/abs (- v 1.0)) 1.0E-10)) v40_l319)))


(def
 v43_l331
 (def
  max-nontrivial
  (apply max (for [k (range 1 n)] (el/abs (step-hat k))))))


(def v44_l334 max-nontrivial)


(def
 v46_l343
 (let
  [t
   10
   dists
   (walk-distributions ct step-dist n t)
   conv-t-hat
   (hm/fourier-transform ct (dists t))
   power-t
   (reduce el/* (repeat t step-hat))]
  (allclose? (el/abs conv-t-hat) (el/abs power-t) 1.0E-8)))


(deftest t47_l349 (is (true? v46_l343)))


(def
 v49_l353
 (let
  [steps
   [1 5 15 40]
   rows
   (vec
    (for
     [t steps k (range n)]
     {:k k,
      :power (Math/pow (el/abs (step-hat k)) t),
      :steps (str "t=" t)}))]
  (->
   (tc/dataset rows)
   (plotly/base
    {:=x :k,
     :=y :power,
     :=color :steps,
     :=title "|μ̂(k)|^t — Fourier coefficients after t steps",
     :=x-title "frequency k",
     :=y-title "|μ̂(k)|^t"})
   (plotly/layer-line)
   (plotly/layer-point {:=mark-size 8})
   plotly/plot)))


(def
 v51_l378
 (def
  step-nn
  (t/complex-tensor-real
   (vec (for [g (range n)] (case g 1 0.5 23 0.5 0.0))))))


(def
 v53_l385
 (def
  step-long
  (t/complex-tensor-real
   (vec
    (for [g (range n)] (case g 0 0.2 1 0.2 2 0.2 22 0.2 23 0.2 0.0))))))


(def
 v54_l390
 (let
  [walks
   [["nearest-neighbor" step-nn]
    ["lazy (±1, 0)" step-dist]
    ["long-range (±2)" step-long]]
   tv-data
   (vec
    (for
     [[label step]
      walks
      :let
      [dists (walk-distributions ct step n 80)]
      t
      (range 81)]
     {:step t,
      :tv-distance
      (hm/total-variation-distance (el/re (dists t)) uniform),
      :walk label}))]
  (->
   (tc/dataset tv-data)
   (plotly/base
    {:=x :step,
     :=y :tv-distance,
     :=color :walk,
     :=title "Convergence rate depends on step distribution",
     :=x-title "steps",
     :=y-title "TV distance"})
   (plotly/layer-line)
   plotly/plot)))


(def
 v56_l410
 (let
  [walks
   {"nearest-neighbor" step-nn,
    "lazy (±1, 0)" step-dist,
    "long-range (±2)" step-long}
   rows
   (vec
    (for
     [[label step] walks k (range (inc (/ n 2)))]
     (let
      [fhat (hm/fourier-transform ct step)]
      {:k k, :magnitude (el/abs (fhat k)), :walk label})))]
  (->
   (tc/dataset rows)
   (plotly/base
    {:=x :k,
     :=y :magnitude,
     :=color :walk,
     :=title "Fourier spectra of different step distributions",
     :=x-title "frequency k",
     :=y-title "|μ̂(k)|"})
   (plotly/layer-line)
   (plotly/layer-point {:=mark-size 8})
   plotly/plot)))


(def
 v58_l431
 (kind/table
  {:column-names ["Walk" "max |μ̂(k)|, k≠0" "Spectral gap"],
   :row-vectors
   (mapv
    (fn
     [[label step]]
     (let
      [fhat
       (hm/fourier-transform ct step)
       m
       (apply max (for [k (range 1 n)] (el/abs (fhat k))))]
      [label (format "%.4f" m) (format "%.4f" (- 1.0 m))]))
    [["nearest-neighbor" step-nn]
     ["lazy (±1, 0)" step-dist]
     ["long-range (±2)" step-long]])}))


(def v60_l451 (def m 12))


(def
 v61_l452
 (def G2d (hm/product-group (hm/cyclic-group m) (hm/cyclic-group m))))


(def v62_l453 (def ct2d (hm/character-table G2d)))


(def v63_l455 (def n2d (hm/order G2d)))


(def v64_l457 n2d)


(deftest t65_l459 (is (= v64_l457 144)))


(def v67_l465 (def elts2d (vec (hm/elements G2d))))


(def
 v68_l467
 (def
  step-2d
  (let
   [neighbors #{[0 0] [1 0] [11 0] [0 11] [0 1]}]
   (t/complex-tensor-real
    (mapv (fn [e] (if (neighbors e) 0.2 0.0)) elts2d)))))


(def v69_l472 (dfn/sum (el/re step-2d)))


(deftest
 t70_l474
 (is ((fn [v] (< (Math/abs (- v 1.0)) 1.0E-10)) v69_l472)))


(def
 v72_l479
 (defn
  dist-to-grid
  "Reshape a distribution vector on Z/m x Z/m into a grid for heatmaps."
  [dist-vec m]
  (vec
   (for
    [i (range m)]
    (vec (for [j (range m)] (el/re (dist-vec (+ (* i m) j)))))))))


(def v73_l486 (def dists-2d (walk-distributions ct2d step-2d n2d 60)))


(def
 v74_l488
 (let
  [steps
   [0 1 5 15 30 60]
   traces
   (vec
    (map-indexed
     (fn
      [col t]
      {:type "heatmap",
       :z (dist-to-grid (dists-2d t) m),
       :colorscale "Viridis",
       :showscale false,
       :zmin 0,
       :zmax 0.15,
       :xaxis (if (zero? col) "x" (str "x" (inc col))),
       :yaxis (if (zero? col) "y" (str "y" (inc col)))})
     steps))
   annotations
   (vec
    (map-indexed
     (fn
      [col t]
      (let
       [c (rem col 3) r (quot col 3)]
       {:text (str "t=" t),
        :xref "paper",
        :yref "paper",
        :x (+ (* c 0.35) 0.15),
        :y (- 1.0 (* r 0.52) -0.02),
        :showarrow false,
        :font {:size 13}}))
     steps))]
  (kind/plotly
   {:data traces,
    :layout
    {:xaxis3 {:domain [0.72 1], :visible false},
     :grid {:rows 2, :columns 3, :pattern "independent"},
     :xaxis4 {:domain [0 0.28], :visible false},
     :xaxis2 {:domain [0.36 0.64], :visible false},
     :yaxis6 {:domain [0 0.48], :scaleanchor "x6", :visible false},
     :width 650,
     :xaxis {:domain [0 0.28], :visible false},
     :title "Random walk on Z/12Z × Z/12Z",
     :xaxis5 {:domain [0.36 0.64], :visible false},
     :yaxis5 {:domain [0 0.48], :scaleanchor "x5", :visible false},
     :xaxis6 {:domain [0.72 1], :visible false},
     :yaxis {:domain [0.52 1], :scaleanchor "x", :visible false},
     :yaxis3 {:domain [0.52 1], :scaleanchor "x3", :visible false},
     :annotations annotations,
     :yaxis2 {:domain [0.52 1], :scaleanchor "x2", :visible false},
     :yaxis4 {:domain [0 0.48], :scaleanchor "x4", :visible false},
     :height 450,
     :margin {:t 40, :b 20, :l 20, :r 20}}})))


(def v76_l535 (def uniform-2d (vec (repeat n2d (/ 1.0 (double n2d))))))


(def
 v77_l537
 (let
  [tv-data
   (vec
    (map-indexed
     (fn
      [t dist]
      {:step t,
       :tv-distance
       (hm/total-variation-distance (el/re dist) uniform-2d)})
     dists-2d))]
  (->
   (tc/dataset tv-data)
   (plotly/base
    {:=x :step,
     :=y :tv-distance,
     :=title "Total variation distance — Z/12Z × Z/12Z",
     :=x-title "steps",
     :=y-title "TV distance"})
   (plotly/layer-line)
   plotly/plot)))
