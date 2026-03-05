(ns
 harmonica-book.chord-geometry-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [harmonica-book.book-helpers :refer [allclose?]]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l27 (def sample-rate 44100.0))


(def
 v4_l29
 (defn
  midi->freq
  "MIDI note number to frequency. A4 (69) = 440 Hz."
  [midi]
  (* 440.0 (Math/pow 2.0 (/ (- midi 69.0) 12.0)))))


(def
 v5_l34
 (defn
  chord->samples
  "Render a chord (collection of MIDI notes) as audio samples."
  [midi-notes duration]
  (let
   [n-samples
    (long (* duration sample-rate))
    amp
    (/ 2500.0 (count midi-notes))
    attack
    (long (* 0.02 sample-rate))
    release
    (long (* 0.1 sample-rate))]
   (t/make-reader
    :float32
    n-samples
    (let
     [env
      (cond
       (< idx attack)
       (/ (double idx) attack)
       (> idx (- n-samples release))
       (/ (double (- n-samples idx)) release)
       :else
       (Math/exp (* -1.0 (/ (double (- idx attack)) n-samples))))
      phase
      (/ (double idx) sample-rate)
      wave
      (reduce
       +
       (map
        (fn
         [m]
         (let
          [f (midi->freq m)]
          (+
           (* 0.65 (Math/sin (* 2.0 Math/PI f phase)))
           (* 0.25 (Math/sin (* 2.0 Math/PI 2.0 f phase)))
           (* 0.1 (Math/sin (* 2.0 Math/PI 3.0 f phase))))))
        midi-notes))]
     (float (* amp env wave)))))))


(def
 v6_l58
 (defn
  chord-sequence->samples
  "Render a sequence of chords as audio. Each chord is a collection of MIDI notes."
  [chords chord-dur]
  (let
   [n-chord
    (long (* chord-dur sample-rate))
    n-total
    (* (count chords) n-chord)
    amp-per-note
    2500.0
    attack
    (long (* 0.015 sample-rate))
    sounding
    (long (* 0.85 n-chord))
    release
    (long (* 0.06 sample-rate))]
   (t/make-reader
    :float32
    n-total
    (let
     [chord-idx
      (quot idx n-chord)
      t
      (rem idx n-chord)
      midi-notes
      (nth chords chord-idx)
      n-notes
      (count midi-notes)
      amp
      (/ amp-per-note n-notes)]
     (if
      (>= t sounding)
      (float 0.0)
      (let
       [env
        (cond
         (< t attack)
         (/ (double t) attack)
         (> t (- sounding release))
         (*
          (Math/exp (* -1.5 (/ (double (- t attack)) sounding)))
          (/ (double (- sounding t)) release))
         :else
         (Math/exp (* -1.5 (/ (double (- t attack)) sounding))))
        phase
        (/ (double t) sample-rate)
        wave
        (reduce
         +
         (map
          (fn
           [m]
           (let
            [f (midi->freq m)]
            (+
             (* 0.65 (Math/sin (* 2.0 Math/PI f phase)))
             (* 0.25 (Math/sin (* 2.0 Math/PI 2.0 f phase)))
             (* 0.1 (Math/sin (* 2.0 Math/PI 3.0 f phase))))))
          midi-notes))]
       (float (* amp env wave)))))))))


(def
 v7_l92
 (defn
  play-chord
  "Play a chord given as pitch-class numbers (0-11). Octave is C4 (MIDI 60)."
  [pcs]
  (let
   [midi (mapv (fn* [p1__90071#] (+ 60 p1__90071#)) (sort pcs))]
   (kind/audio
    {:samples (chord->samples midi 1.5), :sample-rate sample-rate}))))


(def
 v8_l99
 (defn
  play-chords
  "Play a sequence of chords. Each chord is a set of pitch-class numbers."
  [chord-seq]
  (let
   [midi-chords
    (mapv
     (fn [pcs] (mapv (fn* [p1__90072#] (+ 60 p1__90072#)) (sort pcs)))
     chord-seq)]
   (kind/audio
    {:samples (chord-sequence->samples midi-chords 0.6),
     :sample-rate sample-rate}))))


(def
 v10_l111
 (def
  pitch-names
  ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"]))


(def
 v12_l117
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
    (mapv (fn* [p1__90073#] (Math/cos p1__90073#)) angles)
    ys
    (mapv (fn* [p1__90074#] (Math/sin p1__90074#)) angles)
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


(def v13_l147 (chord-plot #{0 7 4} "C major on the pitch class circle"))


(def v14_l149 (play-chord #{0 7 4}))


(def
 v16_l159
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
        (mapv (fn* [p1__90075#] (mod (+ p1__90075# k) 12)) c-major))]
      {:transposition k, :notes (str (mapv pitch-names transposed))}))
    (range 12))]
  (kind/table
   {:column-names ["Transposition" "Chord"],
    :row-vectors
    (mapv
     (fn [{:keys [transposition notes]}] [transposition notes])
     orbit)})))


(def v18_l173 (play-chords [[0 4 7] [1 5 8] [2 6 9] [3 7 10]]))


(def
 v20_l183
 (let
  [G
   (hm/cyclic-group 12)
   act
   (fn [g x] (mod (+ x g) 12))
   {:keys [domain], act-sub :act}
   (hm/subset-action act (range 12) 3)
   orbs
   (hm/orbits G act-sub domain)]
  (count orbs)))


(deftest t21_l189 (is (= v20_l183 19)))


(def
 v23_l197
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
 v24_l210
 (let
  [G
   (hm/cyclic-group 12)
   act
   (fn [g x] (mod (+ x g) 12))
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
 v26_l240
 (let
  [G
   (hm/dihedral-group 12)
   act
   (fn [[t k] x] (case t :r (mod (+ x k) 12) :s (mod (- k x) 12)))
   {:keys [domain], act-sub :act}
   (hm/subset-action act (range 12) 3)
   orbs
   (hm/orbits G act-sub domain)]
  (count orbs)))


(deftest t27_l249 (is (= v26_l240 12)))


(def v29_l258 (play-chords [[0 4 7] [0 5 8]]))


(def
 v31_l262
 (let
  [G-c
   (hm/cyclic-group 12)
   G-d
   (hm/dihedral-group 12)
   act-c
   (fn [g x] (mod (+ x g) 12))
   act-d
   (fn [[t k] x] (case t :r (mod (+ x k) 12) :s (mod (- k x) 12)))
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
     (filter (fn* [p1__90076#] (contains? p1__90076# rep)) orbs-d)))
   merged-groups
   (group-by d-orbit-of c-reps)
   merged-rows
   (sort-by
    (comp str first first)
    (mapv
     (fn [[_ reps]] [(mapv str (sort reps)) (count reps)])
     merged-groups))]
  (kind/table
   {:column-names ["C₁₂ types merged" "Count"],
    :row-vectors
    (mapv (fn [[reps cnt]] [(str reps) cnt]) merged-rows)})))


(def
 v33_l302
 (let
  [G
   (hm/dihedral-group 12)
   act
   (fn [[t k] x] (case t :r (mod (+ x k) 12) :s (mod (- k x) 12)))
   {:keys [domain], act-sub :act}
   (hm/subset-action act (range 12) 3)
   orbs
   (hm/orbits G act-sub domain)
   reps
   (mapv (fn* [p1__90077#] (first (sort p1__90077#))) orbs)
   ivs
   (mapv interval-vector reps)
   iv-groups
   (group-by identity ivs)]
  (every? (fn* [p1__90078#] (= 1 (count (val p1__90078#)))) iv-groups)))


(deftest t34_l314 (is (true? v33_l302)))


(def
 v36_l324
 (let
  [G-c
   (hm/cyclic-group 12)
   G-d
   (hm/dihedral-group 12)
   act-c
   (fn [g x] (mod (+ x g) 12))
   act-d
   (fn [[t k] x] (case t :r (mod (+ x k) 12) :s (mod (- k x) 12)))
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
    ["Chord size k" "Total subsets" "Types (C₁₂)" "Types (D₁₂)"],
    :row-vectors
    (mapv
     (fn
      [{:keys [k subsets under-C12 under-D12]}]
      [k subsets under-C12 under-D12])
     results)})))


(def
 v38_l358
 (let
  [G
   (hm/cyclic-group 12)
   ct
   (hm/character-table G)
   f-vals
   (t/complex-tensor-real
    (mapv (fn [x] (if (#{0 7 4} x) 1.0 0.0)) (range 12)))
   f-hat
   (hm/fourier-transform ct f-vals)]
  (kind/table
   {:column-names ["Frequency k" "|f̂(k)|²"],
    :row-vectors
    (mapv
     (fn
      [k]
      (let
       [fk
        (f-hat k)
        mag-sq
        (let [r (el/re fk) i (el/im fk)] (+ (* r r) (* i i)))]
       [k (format "%.4f" mag-sq)]))
     (range 12))})))


(def
 v40_l379
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
   (t/complex-tensor-real
    (mapv (fn [x] (if ((set chord-a) x) 1.0 0.0)) (range 12)))
   f-b
   (t/complex-tensor-real
    (mapv (fn [x] (if ((set chord-b) x) 1.0 0.0)) (range 12)))
   hat-a
   (hm/fourier-transform ct f-a)
   hat-b
   (hm/fourier-transform ct f-b)]
  (allclose? (el/abs hat-a) (el/abs hat-b))))


(deftest t41_l389 (is (true? v40_l379)))


(def v43_l396 (chord-plot #{0 7 4} "Major triad"))


(def v44_l398 (play-chord #{0 7 4}))


(def v45_l400 (chord-plot #{0 7 3} "Minor triad"))


(def v46_l402 (play-chord #{0 7 3}))


(def v47_l404 (chord-plot #{0 4 8} "Augmented triad"))


(def v48_l406 (play-chord #{0 4 8}))


(def v49_l408 (chord-plot #{0 6 3} "Diminished triad"))


(def v50_l410 (play-chord #{0 6 3}))


(def
 v52_l426
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
       (map (fn* [p1__90079#] (mod (+ p1__90079# k) n)) pcs-vec))))
    inversions
    (for
     [k (range n)]
     (vec
      (sort
       (map (fn* [p1__90080#] (mod (- k p1__90080#) n)) pcs-vec))))
    normalize
    (fn
     [s]
     (let
      [base (first s)]
      (mapv (fn* [p1__90081#] (mod (- p1__90081# base) n)) s)))
    candidates
    (map normalize (concat transpositions inversions))]
   (first (sort candidates)))))


(def
 v54_l449
 (let
  [G
   (hm/dihedral-group 12)
   act
   (fn [[t k] x] (case t :r (mod (+ x k) 12) :s (mod (- k x) 12)))
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
 v56_l477
 (let
  [G
   (hm/dihedral-group 12)
   act
   (fn [[t k] x] (case t :r (mod (+ x k) 12) :s (mod (- k x) 12)))
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


(deftest t57_l490 (is (true? v56_l477)))


(def
 v59_l495
 (let
  [G
   (hm/dihedral-group 12)
   act
   (fn [[t k] x] (case t :r (mod (+ x k) 12) :s (mod (- k x) 12)))
   {:keys [domain], act-sub :act}
   (hm/subset-action act (range 12) 4)
   orbs
   (hm/orbits G act-sub domain)]
  (count orbs)))


(deftest t60_l504 (is (= v59_l495 29)))


(def
 v62_l511
 (let
  [z15 [0 1 4 6] z29 [0 1 3 7]]
  (= (interval-vector z15) (interval-vector z29))))


(deftest t63_l515 (is (true? v62_l511)))
