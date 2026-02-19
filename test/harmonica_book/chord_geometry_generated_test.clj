(ns
 harmonica-book.chord-geometry-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [harmonica-book.book-helpers :refer [allclose?]]
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
    (mapv (fn* [p1__106541#] (Math/cos p1__106541#)) angles)
    ys
    (mapv (fn* [p1__106542#] (Math/sin p1__106542#)) angles)
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
         (fn* [p1__106543#] (mod (+ p1__106543# (long k)) 12))
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
   (hm/cyclic-group 12)
   act
   (fn [g x] (mod (+ (long x) (long g)) 12))
   {:keys [domain], act-sub :act}
   (hm/subset-action act (range 12) 3)
   orbs
   (hm/orbits G act-sub domain)]
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
   (hm/cyclic-group 12)
   act
   (fn [g x] (mod (+ (long x) (long g)) 12))
   {:keys [domain], act-sub :act}
   (hm/subset-action act (range 12) 3)
   orbs
   (hm/orbits G act-sub domain)
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
   (hm/dihedral-group 12)
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
   (hm/subset-action act (range 12) 3)
   orbs
   (hm/orbits G act-sub domain)]
  (count orbs)))


(deftest t17_l157 (is (= v16_l148 12)))


(def
 v19_l164
 (let
  [G-c
   (hm/cyclic-group 12)
   G-d
   (hm/dihedral-group 12)
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
   (hm/subset-action act-c (range 12) 3)
   {_ :domain, act-d-sub :act}
   (hm/subset-action act-d (range 12) 3)
   orbs-c
   (hm/orbits G-c act-c-sub domain-3)
   orbs-d
   (hm/orbits G-d act-d-sub domain-3)
   c-reps
   (mapv (fn [orb] (first (sort orb))) orbs-c)
   d-orbit-of
   (fn
    [rep]
    (first
     (filter (fn* [p1__106544#] (contains? p1__106544# rep)) orbs-d)))
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
   (hm/dihedral-group 12)
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
   (hm/subset-action act (range 12) 3)
   orbs
   (hm/orbits G act-sub domain)
   reps
   (mapv (fn* [p1__106545#] (first (sort p1__106545#))) orbs)
   ivs
   (mapv interval-vector reps)
   iv-groups
   (group-by identity ivs)]
  (every?
   (fn* [p1__106546#] (= 1 (count (val p1__106546#))))
   iv-groups)))


(deftest t22_l216 (is (true? v21_l204)))


(def
 v24_l226
 (let
  [G-c
   (hm/cyclic-group 12)
   G-d
   (hm/dihedral-group 12)
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
       (hm/subset-action act-c (range 12) k)
       {_ :domain, act-d-k :act}
       (hm/subset-action act-d (range 12) k)
       n-trans
       (count (hm/orbits G-c act-c-k domain-k))
       n-dihed
       (count (hm/orbits G-d act-d-k domain-k))]
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
   (hm/cyclic-group 12)
   ct
   (hm/character-table G)
   f-vals
   (cx/complex-tensor-real
    (mapv (fn [x] (if (#{0 7 4} x) 1.0 0.0)) (range 12)))
   f-hat
   (hm/fourier-transform ct f-vals)]
  (kind/table
   {:column-names ["Frequency $k$" "$|\\hat{f}(k)|^2$"],
    :row-vectors
    (mapv
     (fn
      [k]
      (let
       [fk
        (f-hat k)
        mag-sq
        (let [r (cx/re fk) i (cx/im fk)] (+ (* r r) (* i i)))]
       [k (format "%.4f" mag-sq)]))
     (range 12))})))


(def
 v28_l281
 (let
  [G
   (hm/cyclic-group 12)
   ct
   (hm/character-table G)
   chord-a
   [0 4 7]
   chord-b
   [6 10 1]
   f-a
   (cx/complex-tensor-real
    (mapv (fn [x] (if ((set chord-a) x) 1.0 0.0)) (range 12)))
   f-b
   (cx/complex-tensor-real
    (mapv (fn [x] (if ((set chord-b) x) 1.0 0.0)) (range 12)))
   hat-a
   (hm/fourier-transform ct f-a)
   hat-b
   (hm/fourier-transform ct f-b)]
  (allclose? (cx/cabs hat-a) (cx/cabs hat-b))))


(deftest t29_l291 (is (true? v28_l281)))


(def v31_l298 (chord-plot #{0 7 4} "Major triad"))


(def v32_l300 (chord-plot #{0 7 3} "Minor triad"))


(def v33_l302 (chord-plot #{0 4 8} "Augmented triad"))


(def v34_l304 (chord-plot #{0 6 3} "Diminished triad"))


(def
 v36_l320
 (defn
  prime-form
  "Compute the prime form of a pitch class set under TnI equivalence.\n  The prime form is the most compact representative: transpose all\n  rotations and inversions to start at 0, then pick the lexicographically\n  smallest."
  [pcs]
  (let
   [pcs-vec
    (vec (sort pcs))
    n
    12
    transpositions
    (for
     [k (range n)]
     (vec
      (sort
       (map (fn* [p1__106547#] (mod (+ p1__106547# k) n)) pcs-vec))))
    inversions
    (for
     [k (range n)]
     (vec
      (sort
       (map (fn* [p1__106548#] (mod (- k p1__106548#) n)) pcs-vec))))
    normalize
    (fn
     [s]
     (let
      [base (first s)]
      (mapv (fn* [p1__106549#] (mod (- p1__106549# base) n)) s)))
    candidates
    (map normalize (concat transpositions inversions))]
   (first (sort candidates)))))


(def
 v38_l343
 (let
  [G
   (hm/dihedral-group 12)
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
   (hm/subset-action act (range 12) 3)
   orbs
   (hm/orbits G act-sub domain)
   primes
   (sort (mapv (fn [orb] (prime-form (first orb))) orbs))
   forte-catalog
   [[0 1 2]
    [0 1 3]
    [0 1 4]
    [0 1 5]
    [0 1 6]
    [0 2 4]
    [0 2 5]
    [0 2 6]
    [0 2 7]
    [0 3 6]
    [0 3 7]
    [0 4 8]]
   forte-names
   ["3-1"
    "3-2"
    "3-3"
    "3-4"
    "3-5"
    "3-6"
    "3-7"
    "3-8"
    "3-9"
    "3-10"
    "3-11"
    "3-12"]
   musical-names
   ["chromatic cluster"
    "—"
    "—"
    "—"
    "Viennese trichord"
    "whole-tone"
    "—"
    "—"
    "stack of fifths"
    "diminished"
    "major/minor triad"
    "augmented triad"]]
  (kind/table
   {:column-names
    ["Forte number" "Prime form" "Interval vector" "Musical name"],
    :row-vectors
    (mapv
     (fn
      [forte pf name]
      [forte (str pf) (str (interval-vector pf)) name])
     forte-names
     forte-catalog
     musical-names)})))


(def
 v40_l371
 (let
  [G
   (hm/dihedral-group 12)
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
   (hm/subset-action act (range 12) 3)
   orbs
   (hm/orbits G act-sub domain)
   computed
   (sort (mapv (fn [orb] (prime-form (first orb))) orbs))
   catalog
   [[0 1 2]
    [0 1 3]
    [0 1 4]
    [0 1 5]
    [0 1 6]
    [0 2 4]
    [0 2 5]
    [0 2 6]
    [0 2 7]
    [0 3 6]
    [0 3 7]
    [0 4 8]]]
  (= computed catalog)))


(deftest t41_l384 (is (true? v40_l371)))


(def
 v43_l389
 (let
  [G
   (hm/dihedral-group 12)
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
   (hm/subset-action act (range 12) 4)
   orbs
   (hm/orbits G act-sub domain)]
  (count orbs)))


(deftest t44_l398 (is (= v43_l389 29)))


(def
 v46_l405
 (let
  [z15 [0 1 4 6] z29 [0 1 3 7]]
  (= (interval-vector z15) (interval-vector z29))))


(deftest t47_l409 (is (true? v46_l405)))
