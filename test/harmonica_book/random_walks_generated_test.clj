(ns
 harmonica-book.random-walks-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [harmonica-book.book-helpers :refer [allclose?]]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l51
 (defn
  gaussian-pdf
  "Gaussian density at x with given mean and standard deviation."
  [mu sigma x]
  (let
   [z (/ (- x mu) sigma)]
   (/ (Math/exp (* -0.5 z z)) (* sigma (Math/sqrt (* 2.0 Math/PI)))))))


(def
 v5_l61
 (let
  [xs
   (vec (range -6.0 6.01 0.05))
   sigma
   0.8
   rows
   (vec
    (for
     [n [1 2 4 8] x xs]
     {:x x,
      :density (gaussian-pdf 0 (* sigma (Math/sqrt (double n))) x),
      :steps (str n (if (= n 1) " step" " steps"))}))]
  (->
   (tc/dataset rows)
   (plotly/base
    {:=x :x,
     :=y :density,
     :=color :steps,
     :=title "Repeated convolution of a Gaussian",
     :=x-title "x",
     :=y-title "density"})
   (plotly/layer-line)
   plotly/plot)))


(def
 v7_l82
 (let
  [xs
   (vec (range -3.0 3.1 0.25))
   sigma1
   0.6
   sigma2
   1.5
   z1
   (vec
    (for
     [y xs]
     (vec
      (for
       [x xs]
       (* (gaussian-pdf 0 sigma1 x) (gaussian-pdf 0 sigma1 y))))))
   z2
   (vec
    (for
     [y xs]
     (vec
      (for
       [x xs]
       (* (gaussian-pdf 0 sigma2 x) (gaussian-pdf 0 sigma2 y))))))]
  (kind/plotly
   {:data
    [{:type "heatmap",
      :z z1,
      :x xs,
      :y xs,
      :colorscale "Viridis",
      :showscale false,
      :xaxis "x",
      :yaxis "y"}
     {:type "heatmap",
      :z z2,
      :x xs,
      :y xs,
      :colorscale "Viridis",
      :showscale false,
      :xaxis "x2",
      :yaxis "y2"}],
    :layout
    {:grid {:rows 1, :columns 2, :pattern "independent"},
     :xaxis2 {:domain [0.55 1], :title "x"},
     :width 650,
     :xaxis {:domain [0 0.45], :title "x"},
     :title "2D Gaussian: narrow (σ=0.6) vs. convolved (σ=1.5)",
     :yaxis {:title "y", :scaleanchor "x"},
     :yaxis2 {:title "y", :scaleanchor "x2"},
     :height 320,
     :margin {:t 40, :b 40, :l 50, :r 20}}})))


(def v9_l124 (def n 24))


(def v10_l125 (def G (hm/cyclic-group n)))


(def v11_l126 (def ct (hm/character-table G)))


(def
 v13_l132
 (def
  step-dist
  (cx/complex-tensor-real
   (vec
    (for
     [g (range n)]
     (case g 0 (/ 1.0 3) 1 (/ 1.0 3) 23 (/ 1.0 3) 0.0))))))


(def v15_l144 (dfn/sum (cx/re step-dist)))


(deftest
 t16_l146
 (is ((fn [v] (< (Math/abs (- v 1.0)) 1.0E-10)) v15_l144)))


(def
 v18_l152
 (defn
  make-delta
  "Point mass at position 0 on a group of order n."
  [n]
  (cx/complex-tensor-real (vec (cons 1.0 (repeat (dec n) 0.0))))))


(def v19_l158 (def delta-0 (make-delta n)))


(def
 v21_l164
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
 v23_l174
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
      :probability (cx/re ((dists t) g)),
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
   (plotly/layer-point {:=mark-size 4})
   plotly/plot)))


(def v25_l202 (def uniform (vec (repeat n (/ 1.0 n)))))


(def
 v26_l204
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
       (hm/total-variation-distance (cx/re dist) uniform)})
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


(def v28_l221 (hm/total-variation-distance (cx/re delta-0) uniform))


(deftest
 t29_l223
 (is
  ((fn [v] (< (Math/abs (- v (- 1.0 (/ 1.0 24)))) 1.0E-10)) v28_l221)))


(def
 v31_l228
 (let
  [dists (walk-distributions ct step-dist n 200)]
  (hm/total-variation-distance (cx/re (dists 200)) uniform)))


(deftest t32_l231 (is ((fn [v] (< v 0.01)) v31_l228)))


(def v34_l249 (def step-hat (hm/fourier-transform ct step-dist)))


(def
 v36_l253
 (let
  [rows
   (vec (for [k (range n)] {:k k, :magnitude (cx/cabs (step-hat k))}))]
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


(def v38_l266 (cx/cabs (step-hat 0)))


(deftest
 t39_l268
 (is ((fn [v] (< (Math/abs (- v 1.0)) 1.0E-10)) v38_l266)))


(def
 v41_l278
 (def
  max-nontrivial
  (apply max (for [k (range 1 n)] (cx/cabs (step-hat k))))))


(def v42_l281 max-nontrivial)


(def
 v44_l290
 (let
  [t
   10
   dists
   (walk-distributions ct step-dist n t)
   conv-t-hat
   (hm/fourier-transform ct (dists t))
   power-t
   (reduce cx/cmul (repeat t step-hat))]
  (allclose? (cx/cabs conv-t-hat) (cx/cabs power-t) 1.0E-8)))


(deftest t45_l296 (is (true? v44_l290)))


(def
 v47_l300
 (let
  [steps
   [1 5 15 40]
   rows
   (vec
    (for
     [t steps k (range n)]
     {:k k,
      :power (Math/pow (cx/cabs (step-hat k)) t),
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
   (plotly/layer-point {:=mark-size 4})
   plotly/plot)))


(def
 v49_l325
 (def
  step-nn
  (cx/complex-tensor-real
   (vec (for [g (range n)] (case g 1 0.5 23 0.5 0.0))))))


(def
 v51_l332
 (def
  step-long
  (cx/complex-tensor-real
   (vec
    (for [g (range n)] (case g 0 0.2 1 0.2 2 0.2 22 0.2 23 0.2 0.0))))))


(def
 v52_l337
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
      (hm/total-variation-distance (cx/re (dists t)) uniform),
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
 v54_l357
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
      {:k k, :magnitude (cx/cabs (fhat k)), :walk label})))]
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
   (plotly/layer-point {:=mark-size 4})
   plotly/plot)))


(def
 v56_l378
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
       (apply max (for [k (range 1 n)] (cx/cabs (fhat k))))]
      [label (format "%.4f" m) (format "%.4f" (- 1.0 m))]))
    [["nearest-neighbor" step-nn]
     ["lazy (±1, 0)" step-dist]
     ["long-range (±2)" step-long]])}))


(def v58_l398 (def m 12))


(def
 v59_l399
 (def G2d (hm/product-group (hm/cyclic-group m) (hm/cyclic-group m))))


(def v60_l400 (def ct2d (hm/character-table G2d)))


(def v61_l402 (def n2d (hm/order G2d)))


(def v62_l404 n2d)


(deftest t63_l406 (is (= v62_l404 144)))


(def v65_l412 (def elts2d (vec (hm/elements G2d))))


(def
 v66_l414
 (def
  step-2d
  (let
   [neighbors #{[0 0] [1 0] [11 0] [0 11] [0 1]}]
   (cx/complex-tensor-real
    (mapv (fn [e] (if (neighbors e) 0.2 0.0)) elts2d)))))


(def v67_l419 (dfn/sum (cx/re step-2d)))


(deftest
 t68_l421
 (is ((fn [v] (< (Math/abs (- v 1.0)) 1.0E-10)) v67_l419)))


(def
 v70_l426
 (defn
  dist-to-grid
  "Reshape a distribution vector on Z/m x Z/m into a grid for heatmaps."
  [dist-vec m]
  (vec
   (for
    [i (range m)]
    (vec (for [j (range m)] (cx/re (dist-vec (+ (* i m) j)))))))))


(def v71_l433 (def dists-2d (walk-distributions ct2d step-2d n2d 60)))


(def
 v72_l435
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


(def v74_l482 (def uniform-2d (vec (repeat n2d (/ 1.0 (double n2d))))))


(def
 v75_l484
 (let
  [tv-data
   (vec
    (map-indexed
     (fn
      [t dist]
      {:step t,
       :tv-distance
       (hm/total-variation-distance (cx/re dist) uniform-2d)})
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
