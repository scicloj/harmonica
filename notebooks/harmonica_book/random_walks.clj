;; # Random Walks and Convolution
;;
;; When you take a random step, then another, then another, the resulting
;; position is the **convolution** of the individual step distributions.
;; Convolution is the algebraic operation that describes how randomness
;; accumulates.
;;
;; On the real line, this is the familiar fact that the sum of independent
;; Gaussians is Gaussian — with variances adding. On a finite group, the
;; same idea applies: repeated random steps converge to the uniform
;; distribution, and the [Fourier transform](https://en.wikipedia.org/wiki/Fourier_analysis_on_finite_groups)
;; tells us exactly how fast.
;;
;; This notebook builds intuition for convolution step by step:
;;
;; - On the **real line**, where it's visually obvious
;; - On **cyclic groups** $\mathbb{Z}/n\mathbb{Z}$, where the DFT makes convergence precise
;; - On **2D product groups**, where we can still visualize
;;
;; Along the way, we'll ask: what happens when the group is too large
;; or too complex to visualize?

(ns harmonica-book.random-walks
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
   [harmonica-book.book-helpers :refer [allclose?]]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [tech.v3.datatype.convolve :as dt-conv]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Convolution on the real line
;;
;; The [convolution](https://en.wikipedia.org/wiki/Convolution) of two
;; functions $f$ and $g$ is the integral
;;
;; $$(f * g)(x) = \int f(t)\, g(x - t)\, dt$$
;;
;; When $f$ is an arbitrary distribution and $g$ is a
;; [Gaussian](https://en.wikipedia.org/wiki/Normal_distribution)
;; bell curve, the convolution $f * g$ is a **smoothed** version of $f$.
;; The wider the Gaussian (larger $\sigma$), the more smoothing.

(defn gaussian-pdf
  "Gaussian density at x with given mean and standard deviation."
  [mu sigma x]
  (let [z (/ (- x mu) sigma)]
    (/ (Math/exp (* -0.5 z z))
       (* sigma (Math/sqrt (* 2.0 Math/PI))))))

;; To see this, start with a bumpy distribution — three peaks of
;; different widths and heights — and convolve it with Gaussians
;; of increasing width.

(defn gaussian-kernel
  "Sampled Gaussian kernel (integrates to ~1) for numerical convolution."
  [sigma dx]
  (let [hw (long (/ (* 4 sigma) dx))
        ks (range (- hw) (inc hw))]
    (double-array (map (fn [ki]
                         (let [x (* ki dx)]
                           (* dx (gaussian-pdf 0 sigma x))))
                       ks))))

(let [xs (vec (range -5.0 5.01 0.05))
      dx 0.05
      ;; A bumpy distribution: three peaks of different shapes
      raw (double-array
           (map (fn [x]
                  (+ (* 0.5 (Math/exp (* -8.0 (* (+ x 2.0) (+ x 2.0)))))
                     (* 0.35 (Math/exp (* -12.0 (* (- x 0.5) (- x 0.5)))))
                     (* 0.15 (Math/exp (* -3.0 (* (- x 2.5) (- x 2.5)))))))
                xs))
      ;; Normalize so it integrates to 1
      signal (dfn// raw (* dx (dfn/sum raw)))
      rows (vec (concat
                 (for [i (range (count xs))]
                   {:x (xs i) :density (signal i) :curve "original"})
                 (for [sigma [0.3 0.6 1.2]
                       :let [smoothed (dt-conv/convolve1d
                                       signal (gaussian-kernel sigma dx)
                                       {:mode :same})]
                       i (range (count xs))]
                   {:x (xs i) :density (smoothed i)
                    :curve (str "smoothed, σ=" sigma)})))]
  (-> (tc/dataset rows)
      (plotly/base {:=x :x :=y :density :=color :curve
                    :=title "Convolution with a Gaussian smooths"
                    :=x-title "x" :=y-title "density"})
      (plotly/layer-line)
      plotly/plot))

;; The narrow peak at $x = 0.5$ is the first to blur away; the broader
;; hump at $x = 2.5$ persists longer. With each increase in $\sigma$, sharp
;; features wash out and the distribution approaches a single broad mound.
;; **Convolution is smoothing.**

;; ## Spreading in two dimensions
;;
;; The same effect in 2D: a bumpy surface with several peaks gets
;; smoothed into a broad mound.

(defn convolve-2d
  "Convolve a 2D grid (vec of vecs) with a Gaussian of width sigma,
   using separable 1D convolution (rows then columns)."
  [grid sigma dx]
  (let [n (count grid)
        k (gaussian-kernel sigma dx)
        row-conv (mapv (fn [row]
                         (vec (dt-conv/convolve1d (double-array row) k {:mode :same})))
                       grid)]
    (let [col-conv (mapv (fn [j]
                           (vec (dt-conv/convolve1d
                                 (double-array (map #(nth % j) row-conv))
                                 k {:mode :same})))
                         (range n))]
      (vec (for [i (range n)]
             (vec (for [j (range n)]
                    ((col-conv j) i))))))))

(let [xs (vec (range -3.0 3.1 0.25))
      dx 0.25
      ;; Three off-center peaks of different shapes
      raw (vec (for [y xs]
                 (vec (for [x xs]
                        (+ (* 0.5 (Math/exp (- (+ (* 5 (+ x 1.0) (+ x 1.0))
                                                   (* 5 (- y 0.8) (- y 0.8))))))
                           (* 0.35 (Math/exp (- (+ (* 10 (- x 1.2) (- x 1.2))
                                                    (* 10 (+ y 1.0) (+ y 1.0))))))
                           (* 0.2 (Math/exp (- (+ (* 3 (* x x))
                                                   (* 8 (- y 1.5) (- y 1.5)))))))))))
      z1 raw
      z2 (convolve-2d raw 0.5 dx)
      z3 (convolve-2d raw 1.2 dx)
      zmax (apply max (flatten raw))]
  (kind/plotly
   {:data [{:type "heatmap" :z z1 :x xs :y xs
            :colorscale "Viridis" :showscale false
            :zmin 0 :zmax zmax
            :xaxis "x" :yaxis "y"}
           {:type "heatmap" :z z2 :x xs :y xs
            :colorscale "Viridis" :showscale false
            :zmin 0 :zmax zmax
            :xaxis "x2" :yaxis "y2"}
           {:type "heatmap" :z z3 :x xs :y xs
            :colorscale "Viridis" :showscale false
            :zmin 0 :zmax zmax
            :xaxis "x3" :yaxis "y3"}]
    :layout {:title "2D smoothing: original → σ=0.5 → σ=1.2"
             :grid {:rows 1 :columns 3 :pattern "independent"}
             :xaxis {:domain [0 0.30] :visible false}
             :yaxis {:scaleanchor "x" :visible false}
             :xaxis2 {:domain [0.35 0.65] :visible false}
             :yaxis2 {:scaleanchor "x2" :visible false}
             :xaxis3 {:domain [0.70 1] :visible false}
             :yaxis3 {:scaleanchor "x3" :visible false}
             :width 700 :height 270
             :margin {:t 40 :b 20 :l 20 :r 20}}}))

;; The three distinct peaks in the original merge into a single blob
;; as $\sigma$ increases — the 2D analogue of what we saw on the line.
;; Continuous distributions keep spreading forever — they never reach
;; a "uniform" limit. On a **finite group**, the story has a satisfying
;; ending: the distribution converges to uniform, and the Fourier
;; transform tells us exactly how fast.

;; ## Random walk on $\mathbb{Z}/n\mathbb{Z}$
;;
;; Consider a random walk on the [cyclic group](https://en.wikipedia.org/wiki/Cyclic_group)
;; $\mathbb{Z}/n\mathbb{Z}$. Think of $n$ positions arranged in a circle.
;; At each step, we move to a neighboring position (or stay put) at random.

(def n 24)
(def G (hm/cyclic-group n))
(def ct (hm/character-table G))

;; The **step distribution** assigns probabilities to each group element.
;; For a simple nearest-neighbor walk: probability $1/3$ each for moving
;; left ($-1$), staying ($0$), or moving right ($+1$).

(def step-dist
  (cx/complex-tensor-real
   (vec (for [g (range n)]
          (case g
            0 (/ 1.0 3)
            1 (/ 1.0 3)
            23 (/ 1.0 3)
            0.0)))))

;; This is a probability distribution on the group: non-negative,
;; sums to 1.

(dfn/sum (cx/re step-dist))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 1.0)) 1e-10))])

;; The **initial distribution** is a point mass at the identity (position 0):
;; all probability concentrated at one element.

(defn make-delta
  "Point mass at position 0 on a group of order n."
  [n]
  (cx/complex-tensor-real
   (vec (cons 1.0 (repeat (dec n) 0.0)))))

(def delta-0 (make-delta n))

;; After one step, the distribution is the step distribution itself.
;; After $t$ steps, the distribution is the $t$-fold convolution
;; $\mu^{*t} = \mu * \mu * \cdots * \mu$.

(defn walk-distributions
  "Compute distributions at each step up to t-max, returning a vector."
  [ct step-dist n t-max]
  (loop [dist (make-delta n) t 0 acc []]
    (if (> t t-max)
      acc
      (recur (hm/convolve ct dist step-dist) (inc t) (conj acc dist)))))

;; Let's watch the distribution evolve:

(let [dists (walk-distributions ct step-dist n 100)
      steps [0 1 3 10 30 100]
      rows (vec (for [t steps g (range n)]
                  {:position g
                   :probability (cx/re ((dists t) g))
                   :steps (str "t=" t)}))]
  (-> (tc/dataset rows)
      (plotly/base {:=x :position :=y :probability :=color :steps
                    :=title "Random walk on Z/24Z — distribution at various times"
                    :=x-title "position" :=y-title "probability"})
      (plotly/layer-line)
      (plotly/layer-point {:=mark-size 8})
      plotly/plot))

;; At $t = 0$, all probability is at position 0. After a few steps,
;; a bell-shaped bump forms (just like the Gaussian on the line). After
;; many steps, the distribution flattens to $1/24$ everywhere — the
;; **uniform distribution** on the group.

;; ## Measuring convergence
;;
;; The [total variation distance](https://en.wikipedia.org/wiki/Total_variation_distance_of_probability_measures)
;; quantifies how far the walk is from uniform:
;;
;; $$\|P - U\|_{\mathrm{TV}} = \frac{1}{2}\sum_{g \in G} |P(g) - 1/n|$$
;;
;; It ranges from 0 (perfect mixing) to $1 - 1/n$ (point mass).

(def uniform (vec (repeat n (/ 1.0 n))))

(let [dists (walk-distributions ct step-dist n 200)
      tv-data (vec (map-indexed
                    (fn [t dist]
                      {:step t
                       :tv-distance (hm/total-variation-distance (cx/re dist) uniform)})
                    dists))]
  (-> (tc/dataset tv-data)
      (plotly/base {:=x :step :=y :tv-distance
                    :=title "Total variation distance to uniform — Z/24Z"
                    :=x-title "steps" :=y-title "TV distance"})
      (plotly/layer-line)
      plotly/plot))

;; The TV distance decays smoothly toward 0.

;; At step 0, the TV distance is $1 - 1/n$:

(hm/total-variation-distance (cx/re delta-0) uniform)

(kind/test-last
 [(fn [v] (< (Math/abs (- v (- 1.0 (/ 1.0 24)))) 1e-10))])

;; After 200 steps, it's very small:

(let [dists (walk-distributions ct step-dist n 200)]
  (hm/total-variation-distance (cx/re (dists 200)) uniform))

(kind/test-last
 [(fn [v] (< v 0.01))])

;; ## The Fourier perspective
;;
;; Why does the walk converge, and what controls the rate?
;;
;; The [Fourier transform](dft_as_group_fourier.html) decomposes a
;; function on $\mathbb{Z}/n\mathbb{Z}$ into $n$ frequency components.
;; Convolution in the group domain becomes **pointwise multiplication**
;; in the Fourier domain.
;;
;; So after $t$ steps, the Fourier coefficient at frequency $k$ is:
;;
;; $$\widehat{\mu^{*t}}(k) = \hat{\mu}(k)^t$$
;;
;; Each coefficient gets raised to the $t$-th power.

(def step-hat (hm/fourier-transform ct step-dist))

;; The magnitudes of the Fourier coefficients:

(let [rows (vec (for [k (range n)]
                  {:k k :magnitude (cx/cabs (step-hat k))}))]
  (-> (tc/dataset rows)
      (plotly/base {:=x :k :=y :magnitude
                    :=title "Fourier spectrum of the step distribution"
                    :=x-title "frequency k" :=y-title "|μ̂(k)|"})
      (plotly/layer-bar)
      plotly/plot))

;; The $k = 0$ coefficient has magnitude 1 — this is the total
;; probability, which is conserved. Every other coefficient has magnitude
;; strictly less than 1.

(cx/cabs (step-hat 0))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 1.0)) 1e-10))])

;; When raised to the $t$-th power, coefficients with $|\hat{\mu}(k)| < 1$
;; decay geometrically toward 0. The **only surviving component** in the
;; long run is $k = 0$, which corresponds to the uniform distribution.
;;
;; The convergence rate is controlled by the **largest non-trivial**
;; Fourier coefficient — the one closest to 1:

(def max-nontrivial
  (apply max (for [k (range 1 n)] (cx/cabs (step-hat k)))))

max-nontrivial

;; This is $|\hat{\mu}(1)| = |\hat{\mu}(n{-}1)|$
;; (the two slowest-decaying modes). After $t$ steps, the dominant
;; non-trivial contribution is approximately $|\hat{\mu}(1)|^t$.

;; Let's verify the convolution theorem: the Fourier transform of the
;; $t$-fold convolution equals the $t$-th power of the Fourier coefficients.

(let [t 10
      dists (walk-distributions ct step-dist n t)
      conv-t-hat (hm/fourier-transform ct (dists t))
      power-t (reduce cx/cmul (repeat t step-hat))]
  (allclose? (cx/cabs conv-t-hat) (cx/cabs power-t) 1e-8))

(kind/test-last [true?])

;; Let's visualize how the Fourier spectrum shrinks over time:

(let [steps [1 5 15 40]
      rows (vec (for [t steps k (range n)]
                  {:k k
                   :power (Math/pow (cx/cabs (step-hat k)) t)
                   :steps (str "t=" t)}))]
  (-> (tc/dataset rows)
      (plotly/base {:=x :k :=y :power :=color :steps
                    :=title "|μ̂(k)|^t — Fourier coefficients after t steps"
                    :=x-title "frequency k" :=y-title "|μ̂(k)|^t"})
      (plotly/layer-line)
      (plotly/layer-point {:=mark-size 8})
      plotly/plot))

;; After a few steps, only the components near $k = 0$ (and its mirror
;; near $k = 24$) survive. After many steps, everything decays to 0
;; except $k = 0$ — the walk has mixed.

;; ## Different step distributions
;;
;; The choice of step distribution changes the Fourier spectrum and
;; therefore the convergence rate.

;; **Nearest-neighbor (no laziness)**: move left or right with equal
;; probability, never stay put.

(def step-nn
  (cx/complex-tensor-real
   (vec (for [g (range n)]
          (case g 1 0.5, 23 0.5, 0.0)))))

;; **Long-range**: uniform on $\{-2, -1, 0, +1, +2\}$.

(def step-long
  (cx/complex-tensor-real
   (vec (for [g (range n)]
          (case g 0 0.2, 1 0.2, 2 0.2, 22 0.2, 23 0.2, 0.0)))))

(let [walks [["nearest-neighbor" step-nn]
             ["lazy (±1, 0)" step-dist]
             ["long-range (±2)" step-long]]
      tv-data (vec (for [[label step] walks
                         :let [dists (walk-distributions ct step n 80)]
                         t (range 81)]
                     {:step t
                      :tv-distance (hm/total-variation-distance (cx/re (dists t)) uniform)
                      :walk label}))]
  (-> (tc/dataset tv-data)
      (plotly/base {:=x :step :=y :tv-distance :=color :walk
                    :=title "Convergence rate depends on step distribution"
                    :=x-title "steps" :=y-title "TV distance"})
      (plotly/layer-line)
      plotly/plot))

;; The long-range walk mixes fastest (it explores the circle more
;; aggressively). The nearest-neighbor walk is slowest (it takes small
;; steps). The Fourier spectrum explains why:

(let [walks {"nearest-neighbor" step-nn
             "lazy (±1, 0)" step-dist
             "long-range (±2)" step-long}
      rows (vec (for [[label step] walks
                      k (range (inc (/ n 2)))]
                  (let [fhat (hm/fourier-transform ct step)]
                    {:k k
                     :magnitude (cx/cabs (fhat k))
                     :walk label})))]
  (-> (tc/dataset rows)
      (plotly/base {:=x :k :=y :magnitude :=color :walk
                    :=title "Fourier spectra of different step distributions"
                    :=x-title "frequency k" :=y-title "|μ̂(k)|"})
      (plotly/layer-line)
      (plotly/layer-point {:=mark-size 8})
      plotly/plot))

;; The long-range walk has smaller Fourier coefficients (further from 1)
;; at every frequency — so every mode decays faster. The key quantity is
;; the **spectral gap**: the difference $1 - |\hat{\mu}(1)|$.

(kind/table
 {:column-names ["Walk" "max |μ̂(k)|, k≠0" "Spectral gap"]
  :row-vectors
  (mapv (fn [[label step]]
          (let [fhat (hm/fourier-transform ct step)
                m (apply max (for [k (range 1 n)] (cx/cabs (fhat k))))]
            [label (format "%.4f" m) (format "%.4f" (- 1.0 m))]))
        [["nearest-neighbor" step-nn]
         ["lazy (±1, 0)" step-dist]
         ["long-range (±2)" step-long]])})

;; A larger spectral gap means faster mixing.

;; ## Random walk on a 2D group
;;
;; The same analysis extends to
;; [product groups](product_group_dft.html).
;; On $\mathbb{Z}/m\mathbb{Z} \times \mathbb{Z}/n\mathbb{Z}$, we
;; can visualize the walk as a distribution on a grid.

(def m 12)
(def G2d (hm/product-group (hm/cyclic-group m) (hm/cyclic-group m)))
(def ct2d (hm/character-table G2d))

(def n2d (hm/order G2d))

n2d

(kind/test-last [= 144])

;; The step distribution moves one step in a random coordinate direction
;; (or stays put): equal probability on $(\pm 1, 0)$, $(0, \pm 1)$, and
;; $(0, 0)$.

(def elts2d (vec (hm/elements G2d)))

(def step-2d
  (let [neighbors #{[0 0] [1 0] [11 0] [0 1] [0 11]}]
    (cx/complex-tensor-real
     (mapv (fn [e] (if (neighbors e) 0.2 0.0)) elts2d))))

(dfn/sum (cx/re step-2d))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 1.0)) 1e-10))])

;; Let's watch the 2D distribution evolve.

(defn dist-to-grid
  "Reshape a distribution vector on Z/m x Z/m into a grid for heatmaps."
  [dist-vec m]
  (vec (for [i (range m)]
         (vec (for [j (range m)]
                (cx/re (dist-vec (+ (* i m) j))))))))

(def dists-2d (walk-distributions ct2d step-2d n2d 60))

(let [steps [0 1 5 15 30 60]
      traces (vec (map-indexed
                   (fn [col t]
                     {:type "heatmap"
                      :z (dist-to-grid (dists-2d t) m)
                      :colorscale "Viridis"
                      :showscale false
                      :zmin 0 :zmax 0.15
                      :xaxis (if (zero? col) "x" (str "x" (inc col)))
                      :yaxis (if (zero? col) "y" (str "y" (inc col)))})
                   steps))
      annotations (vec (map-indexed
                        (fn [col t]
                          (let [c (rem col 3) r (quot col 3)]
                            {:text (str "t=" t)
                             :xref "paper" :yref "paper"
                             :x (+ (* c 0.35) 0.15)
                             :y (- 1.0 (* r 0.52) -0.02)
                             :showarrow false
                             :font {:size 13}}))
                        steps))]
  (kind/plotly
   {:data traces
    :layout {:title "Random walk on Z/12Z × Z/12Z"
             :grid {:rows 2 :columns 3 :pattern "independent"}
             :xaxis {:domain [0 0.28] :visible false}
             :yaxis {:domain [0.52 1] :scaleanchor "x" :visible false}
             :xaxis2 {:domain [0.36 0.64] :visible false}
             :yaxis2 {:domain [0.52 1] :scaleanchor "x2" :visible false}
             :xaxis3 {:domain [0.72 1] :visible false}
             :yaxis3 {:domain [0.52 1] :scaleanchor "x3" :visible false}
             :xaxis4 {:domain [0 0.28] :visible false}
             :yaxis4 {:domain [0 0.48] :scaleanchor "x4" :visible false}
             :xaxis5 {:domain [0.36 0.64] :visible false}
             :yaxis5 {:domain [0 0.48] :scaleanchor "x5" :visible false}
             :xaxis6 {:domain [0.72 1] :visible false}
             :yaxis6 {:domain [0 0.48] :scaleanchor "x6" :visible false}
             :annotations annotations
             :width 650 :height 450
             :margin {:t 40 :b 20 :l 20 :r 20}}}))

;; The walk starts as a single bright pixel, spreads into a diamond
;; shape (reflecting the axis-aligned step distribution), and gradually
;; flattens to a uniform yellow.

;; The TV distance tells the same story:

(def uniform-2d (vec (repeat n2d (/ 1.0 (double n2d)))))

(let [tv-data (vec (map-indexed
                    (fn [t dist]
                      {:step t
                       :tv-distance (hm/total-variation-distance (cx/re dist) uniform-2d)})
                    dists-2d))]
  (-> (tc/dataset tv-data)
      (plotly/base {:=x :step :=y :tv-distance
                    :=title "Total variation distance — Z/12Z × Z/12Z"
                    :=x-title "steps" :=y-title "TV distance"})
      (plotly/layer-line)
      plotly/plot))

;; ## Beyond circles and grids
;;
;; On $\mathbb{Z}/n\mathbb{Z}$, we visualized a bar chart converging to
;; flat. On a 2D grid, we visualized a heatmap spreading to uniform.
;; In both cases, the Fourier transform decomposed the walk into
;; independently decaying modes, and the **spectral gap** determined
;; the mixing rate.
;;
;; But what about a group that isn't a circle or a grid?
;;
;; The **symmetric group** $S_n$ — the group of all permutations of
;; $n$ objects — has $n!$ elements. Even for $n = 10$, that's over 3
;; million permutations. You can't draw a bar chart with 3 million bars,
;; and there's no natural "circle" or "grid" to lay them out on.
;;
;; Yet the mathematics is exactly the same:
;;
;; - A **random walk** is defined by a step distribution on $S_n$
;; - After $t$ steps, the distribution is the $t$-fold convolution $\mu^{*t}$
;; - The **Fourier transform** decomposes it into components indexed by irreducible representations
;; - Each component decays at a rate determined by its eigenvalue
;; - The **spectral gap** controls the mixing time
;;
;; The only difference is that the "frequencies" are no longer cosines
;; on a circle — they are the [irreducible characters](character_theory.html)
;; of $S_n$, indexed by integer partitions. The character tables we
;; computed via the Murnaghan-Nakayama rule serve exactly this purpose.
;;
;; The next chapters apply this framework to two natural random walks
;; on $S_n$:
;;
;; - [Random Transpositions](random_transpositions.html) — the walk
;;   generated by swapping two random cards. The distribution is a class
;;   function, so scalar Fourier analysis (characters alone) suffices.
;;   Mixing time: $\tfrac{1}{2}n\ln n$ steps.
;;
;; - [Riffle Shuffles](riffle_shuffle.html) — the physically realistic
;;   shuffle. The distribution is **not** a class function, so we need
;;   the full matrix-valued Fourier transform. Mixing time: approximately
;;   $\tfrac{3}{2}\log_2 n$ shuffles — famously, "seven shuffles suffice"
;;   for a 52-card deck.
