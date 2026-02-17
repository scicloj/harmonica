(ns
 reel-book.chord-geometry-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.reel.protocols :as p]
  [fastmath.complex :as c]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l25
 (def
  pitch-names
  ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"]))


(def
 v5_l31
 (defn
  chord-plot
  "Draw a chord as a polygon on the pitch class circle."
  [pcs title]
  (let
   [n
    12
    angles
    (mapv
     (fn [i] (- (* 2 Math/PI (/ i (double n))) (/ Math/PI 2)))
     (range n))
    xs
    (mapv (fn* [p1__96737#] (Math/cos p1__96737#)) angles)
    ys
    (mapv (fn* [p1__96738#] (Math/sin p1__96738#)) angles)
    pcs-sorted
    (vec (sort pcs))
    chord-xs
    (mapv (fn [i] (xs i)) pcs-sorted)
    chord-ys
    (mapv (fn [i] (ys i)) pcs-sorted)
    colors
    (mapv (fn [i] (if ((set pcs) i) "#e74c3c" "#bdc3c7")) (range n))
    sizes
    (mapv (fn [i] (if ((set pcs) i) 14 8)) (range n))]
   (kind/plotly
    {:data
     [{:type "scatter",
       :mode "markers+text",
       :x (vec xs),
       :y (vec ys),
       :text (vec pitch-names),
       :textposition "top center",
       :marker {:size (vec sizes), :color (vec colors)},
       :showlegend false}
      {:type "scatter",
       :mode "lines",
       :x (conj chord-xs (first chord-xs)),
       :y (conj chord-ys (first chord-ys)),
       :line {:color "#e74c3c", :width 2},
       :fill "toself",
       :fillcolor "rgba(231,76,60,0.15)",
       :showlegend false}],
     :layout
     {:title title,
      :xaxis {:visible false, :scaleanchor "y"},
      :yaxis {:visible false},
      :width 350,
      :height 350,
      :margin {:t 40, :b 10, :l 10, :r 10}}}))))


(def v6_l61 (chord-plot #{0 7 4} "C major on the pitch class circle"))


(def
 v8_l71
 (let
  [c-major
   [0 4 7]
   orbit
   (mapv
    (fn
     [k]
     (let
      [transposed
       (sort
        (mapv
         (fn* [p1__96739#] (mod (+ p1__96739# (long k)) 12))
         c-major))]
      {:transposition k, :notes (str (mapv pitch-names transposed))}))
    (range 12))]
  (kind/table
   {:column-names ["Transposition" "Chord"],
    :row-vectors
    (mapv
     (fn [{:keys [transposition notes]}] [transposition notes])
     orbit)})))


(def
 v10_l91
 (let
  [G
   (reel/cyclic-group 12)
   act
   (fn [g x] (mod (+ (long x) (long g)) 12))
   {:keys [domain], act-sub :act}
   (reel/subset-action act (range 12) 3)
   orbs
   (reel/orbits G act-sub domain)]
  (count orbs)))


(deftest t11_l97 (is (= v10_l91 19)))


(def
 v13_l105
 (defn
  interval-vector
  "Compute the interval vector of a pitch class set.\n  Counts the number of each interval class (1 through 6)."
  [pcs]
  (let
   [pcs-vec
    (vec (sort pcs))
    n
    (count pcs-vec)
    intervals
    (for
     [i (range n) j (range (inc i) n)]
     (let
      [diff (mod (- (pcs-vec j) (pcs-vec i)) 12)]
      (min diff (- 12 diff))))]
   (mapv (fn [ic] (count (filter #{ic} intervals))) (range 1 7)))))


(def
 v14_l118
 (let
  [G
   (reel/cyclic-group 12)
   act
   (fn [g x] (mod (+ (long x) (long g)) 12))
   {:keys [domain], act-sub :act}
   (reel/subset-action act (range 12) 3)
   orbs
   (reel/orbits G act-sub domain)
   rows
   (sort-by
    first
    (mapv
     (fn
      [orb]
      (let
       [rep (first (sort orb)) iv (interval-vector rep)]
       [rep iv (count orb)]))
     orbs))]
  (kind/table
   {:column-names ["Representative" "Interval vector" "Orbit size"],
    :row-vectors
    (mapv (fn [[rep iv size]] [(str rep) (str iv) size]) rows)})))


(def
 v16_l148
 (let
  [G
   (reel/dihedral-group 12)
   act
   (fn
    [[t k] x]
    (case
     t
     :r
     (mod (+ (long x) (long k)) 12)
     :s
     (mod (- (long k) (long x)) 12)))
   {:keys [domain], act-sub :act}
   (reel/subset-action act (range 12) 3)
   orbs
   (reel/orbits G act-sub domain)]
  (count orbs)))


(deftest t17_l157 (is (= v16_l148 12)))


(def
 v19_l164
 (let
  [G-c
   (reel/cyclic-group 12)
   G-d
   (reel/dihedral-group 12)
   act-c
   (fn [g x] (mod (+ (long x) (long g)) 12))
   act-d
   (fn
    [[t k] x]
    (case
     t
     :r
     (mod (+ (long x) (long k)) 12)
     :s
     (mod (- (long k) (long x)) 12)))
   {domain-3 :domain, act-c-sub :act}
   (reel/subset-action act-c (range 12) 3)
   {_ :domain, act-d-sub :act}
   (reel/subset-action act-d (range 12) 3)
   orbs-c
   (reel/orbits G-c act-c-sub domain-3)
   orbs-d
   (reel/orbits G-d act-d-sub domain-3)
   c-reps
   (mapv (fn [orb] (first (sort orb))) orbs-c)
   d-orbit-of
   (fn
    [rep]
    (first
     (filter (fn* [p1__96740#] (contains? p1__96740# rep)) orbs-d)))
   merged-groups
   (group-by d-orbit-of c-reps)
   merged-rows
   (sort-by
    (comp str first first)
    (mapv
     (fn [[_ reps]] [(mapv str (sort reps)) (count reps)])
     merged-groups))]
  (kind/table
   {:column-names ["$C_{12}$ types merged" "Count"],
    :row-vectors
    (mapv (fn [[reps cnt]] [(str reps) cnt]) merged-rows)})))


(def
 v21_l204
 (let
  [G
   (reel/dihedral-group 12)
   act
   (fn
    [[t k] x]
    (case
     t
     :r
     (mod (+ (long x) (long k)) 12)
     :s
     (mod (- (long k) (long x)) 12)))
   {:keys [domain], act-sub :act}
   (reel/subset-action act (range 12) 3)
   orbs
   (reel/orbits G act-sub domain)
   reps
   (mapv (fn* [p1__96741#] (first (sort p1__96741#))) orbs)
   ivs
   (mapv interval-vector reps)
   iv-groups
   (group-by identity ivs)]
  (every? (fn* [p1__96742#] (= 1 (count (val p1__96742#)))) iv-groups)))


(deftest t22_l216 (is (true? v21_l204)))


(def
 v24_l226
 (let
  [G-c
   (reel/cyclic-group 12)
   G-d
   (reel/dihedral-group 12)
   act-c
   (fn [g x] (mod (+ (long x) (long g)) 12))
   act-d
   (fn
    [[t k] x]
    (case
     t
     :r
     (mod (+ (long x) (long k)) 12)
     :s
     (mod (- (long k) (long x)) 12)))
   results
   (mapv
    (fn
     [k]
     (let
      [{domain-k :domain, act-c-k :act}
       (reel/subset-action act-c (range 12) k)
       {_ :domain, act-d-k :act}
       (reel/subset-action act-d (range 12) k)
       n-trans
       (count (reel/orbits G-c act-c-k domain-k))
       n-dihed
       (count (reel/orbits G-d act-d-k domain-k))]
      {:k k,
       :subsets (count domain-k),
       :under-C12 n-trans,
       :under-D12 n-dihed}))
    (range 1 12))]
  (kind/table
   {:column-names
    ["Chord size $k$"
     "Total subsets"
     "Types ($C_{12}$)"
     "Types ($D_{12}$)"],
    :row-vectors
    (mapv
     (fn
      [{:keys [k subsets under-C12 under-D12]}]
      [k subsets under-C12 under-D12])
     results)})))


(def
 v26_l260
 (let
  [G
   (reel/cyclic-group 12)
   ct
   (reel/character-table G)
   f-vals
   (mapv (fn [x] (c/complex (if (#{0 7 4} x) 1.0 0.0) 0.0)) (range 12))
   f-hat
   (reel/fourier-transform ct f-vals)]
  (kind/table
   {:column-names ["Frequency $k$" "$|\\hat{f}(k)|^2$"],
    :row-vectors
    (mapv
     (fn
      [k]
      (let
       [fk (f-hat k) mag-sq (+ (* (fk 0) (fk 0)) (* (fk 1) (fk 1)))]
       [k (format "%.4f" mag-sq)]))
     (range 12))})))


(def
 v28_l281
 (let
  [G
   (reel/cyclic-group 12)
   ct
   (reel/character-table G)
   chord-a
   [0 4 7]
   chord-b
   [6 10 1]
   f-a
   (mapv
    (fn [x] (c/complex (if ((set chord-a) x) 1.0 0.0) 0.0))
    (range 12))
   f-b
   (mapv
    (fn [x] (c/complex (if ((set chord-b) x) 1.0 0.0) 0.0))
    (range 12))
   hat-a
   (reel/fourier-transform ct f-a)
   hat-b
   (reel/fourier-transform ct f-b)
   mag-sq
   (fn [fk] (+ (* (fk 0) (fk 0)) (* (fk 1) (fk 1))))]
  (every?
   (fn
    [k]
    (< (Math/abs (- (mag-sq (hat-a k)) (mag-sq (hat-b k)))) 1.0E-10))
   (range 12))))


(deftest t29_l294 (is (true? v28_l281)))


(def v31_l301 (chord-plot #{0 7 4} "Major triad"))


(def v32_l303 (chord-plot #{0 7 3} "Minor triad"))


(def v33_l305 (chord-plot #{0 4 8} "Augmented triad"))


(def v34_l307 (chord-plot #{0 6 3} "Diminished triad"))
