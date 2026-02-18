;; # Chord Geometry — Music Theory as Group Action
;;
;; Western music's 12 [pitch classes](https://en.wikipedia.org/wiki/Pitch_class) form the cyclic group $\mathbb{Z}/12\mathbb{Z}$.
;; Chords are subsets. Two chords related by **[transposition](https://en.wikipedia.org/wiki/Transposition_(music))** — shifting all
;; notes by the same interval — are the "same type": C major and D major are both
;; "major." Chord types are orbits under the group action.
;;
;; This notebook uses group actions to classify chords, connecting abstract
;; algebra to something every musician knows intuitively.

(ns harmonica-book.chord-geometry
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.protocols :as p]
   [scicloj.harmonica.linalg.complex :as cx]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Pitch Classes on the Clock
;;
;; The 12 pitch classes {C, C#, D, ..., B} form a circle, like hours on a
;; clock. We label them 0 through 11:

(def pitch-names
  ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])

;; A chord is a subset of this circle. For example, C major = {C, E, G} = {0, 4, 7}.
;; On the clock face, it's a triangle inscribed in the circle.

(defn chord-plot
  "Draw a chord as a polygon on the pitch class circle."
  [pcs title]
  (let [n 12
        angles (mapv (fn [i] (- (* 2 Math/PI (/ i (double n))) (/ Math/PI 2))) (range n))
        xs (mapv #(Math/cos %) angles)
        ys (mapv #(Math/sin %) angles)
        pcs-sorted (vec (sort pcs))
        chord-xs (mapv (fn [i] (xs i)) pcs-sorted)
        chord-ys (mapv (fn [i] (ys i)) pcs-sorted)
        colors (mapv (fn [i] (if ((set pcs) i) "#e74c3c" "#bdc3c7")) (range n))
        sizes (mapv (fn [i] (if ((set pcs) i) 14 8)) (range n))]
    (kind/plotly
     {:data [{:type "scatter" :mode "markers+text"
              :x (vec xs) :y (vec ys)
              :text (vec pitch-names) :textposition "top center"
              :marker {:size (vec sizes) :color (vec colors)}
              :showlegend false}
             {:type "scatter" :mode "lines"
              :x (conj chord-xs (first chord-xs))
              :y (conj chord-ys (first chord-ys))
              :line {:color "#e74c3c" :width 2}
              :fill "toself" :fillcolor "rgba(231,76,60,0.15)"
              :showlegend false}]
      :layout {:title title
               :xaxis {:visible false :scaleanchor "y"}
               :yaxis {:visible false}
               :width 350 :height 350
               :margin {:t 40 :b 10 :l 10 :r 10}}})))

(chord-plot #{0 4 7} "C major on the pitch class circle")

;; ## Transposition as Group Action
;;
;; **Transposition** by $k$ semitones shifts every note: $T_k(x) = x + k \pmod{12}$.
;; The 12 transpositions form the cyclic group $C_{12}$.
;;
;; C major = {0, 4, 7}. Transposing by 2 gives D major = {2, 6, 9}.
;; All major chords are in the same orbit under $C_{12}$.

(let [c-major [0 4 7]
      orbit (mapv (fn [k]
                    (let [transposed (sort (mapv #(mod (+ % (long k)) 12) c-major))]
                      {:transposition k
                       :notes (str (mapv pitch-names transposed))}))
                  (range 12))]
  (kind/table
   {:column-names ["Transposition" "Chord"]
    :row-vectors (mapv (fn [{:keys [transposition notes]}]
                         [transposition notes])
                       orbit)}))

;; All 12 results are different — major chords form a single orbit of size 12.

;; ## Classifying Trichords
;;
;; A **trichord** is any 3-note subset of $\mathbb{Z}/12\mathbb{Z}$.
;; There are $\binom{12}{3} = 220$ trichords in total.
;; How many distinct types are there up to transposition?

(let [G (hm/cyclic-group 12)
      act (fn [g x] (mod (+ (long x) (long g)) 12))
      {:keys [domain] act-sub :act} (hm/subset-action act (range 12) 3)
      orbs (hm/orbits G act-sub domain)]
  (count orbs))

(kind/test-last [= 19])

;; There are exactly **19 trichord types** under transposition.
;; These are known as **set classes** in music theory, cataloged by
;; Allen Forte.

;; Let's list them with their interval content:

(defn interval-vector
  "Compute the interval vector of a pitch class set.
  Counts the number of each interval class (1 through 6)."
  [pcs]
  (let [pcs-vec (vec (sort pcs))
        n (count pcs-vec)
        intervals (for [i (range n)
                        j (range (inc i) n)]
                    (let [diff (mod (- (pcs-vec j) (pcs-vec i)) 12)]
                      (min diff (- 12 diff))))]
    (mapv (fn [ic] (count (filter #{ic} intervals)))
          (range 1 7))))

(let [G (hm/cyclic-group 12)
      act (fn [g x] (mod (+ (long x) (long g)) 12))
      {:keys [domain] act-sub :act} (hm/subset-action act (range 12) 3)
      orbs (hm/orbits G act-sub domain)
      rows (sort-by first
                    (mapv (fn [orb]
                            (let [rep (first (sort orb))
                                  iv (interval-vector rep)]
                              [rep iv (count orb)]))
                          orbs))]
  (kind/table
   {:column-names ["Representative" "Interval vector" "Orbit size"]
    :row-vectors (mapv (fn [[rep iv size]]
                         [(str rep) (str iv) size])
                       rows)}))

;; Every row is a distinct chord type. Some orbits have size 12 (the
;; chord is not symmetric under any transposition), others have smaller
;; orbits when the chord has internal symmetry — like the augmented
;; triad {0, 4, 8} with orbit size 4, or the diminished triad.

;; ## Adding Inversion: The Dihedral Group
;;
;; **[Inversion](https://en.wikipedia.org/wiki/Inversion_(music)#Pitch_class_inversion)** maps pitch class $x$ to $-x \pmod{12}$ — it flips intervals
;; upside down. Combined with transposition, the symmetry group becomes the
;; dihedral group $D_{12}$ (order 24).
;;
;; Under $D_{12}$, some trichord types that were distinct under $C_{12}$
;; get merged. For example, a chord and its inversion become the same type.

(let [G (hm/dihedral-group 12)
      act (fn [[t k] x]
            (case t
              :r (mod (+ (long x) (long k)) 12)
              :s (mod (- (long k) (long x)) 12)))
      {:keys [domain] act-sub :act} (hm/subset-action act (range 12) 3)
      orbs (hm/orbits G act-sub domain)]
  (count orbs))

(kind/test-last [= 12])

;; Only **12 types** remain under $D_{12}$, down from 19.
;; The 7 pairs that merged are trichords related by inversion.

;; Let's see which types merged:

(let [G-c (hm/cyclic-group 12)
      G-d (hm/dihedral-group 12)
      act-c (fn [g x] (mod (+ (long x) (long g)) 12))
      act-d (fn [[t k] x]
              (case t
                :r (mod (+ (long x) (long k)) 12)
                :s (mod (- (long k) (long x)) 12)))
      {domain-3 :domain act-c-sub :act} (hm/subset-action act-c (range 12) 3)
      {_ :domain act-d-sub :act} (hm/subset-action act-d (range 12) 3)
      orbs-c (hm/orbits G-c act-c-sub domain-3)
      orbs-d (hm/orbits G-d act-d-sub domain-3)
      ;; For each C12 orbit, find which D12 orbit contains its representative
      c-reps (mapv (fn [orb] (first (sort orb))) orbs-c)
      d-orbit-of (fn [rep]
                   (first (filter #(contains? % rep) orbs-d)))
      ;; Group C12-reps by their D12 orbit
      merged-groups (group-by d-orbit-of c-reps)
      merged-rows (sort-by (comp str first first)
                           (mapv (fn [[_ reps]]
                                   [(mapv str (sort reps))
                                    (count reps)])
                                 merged-groups))]
  (kind/table
   {:column-names ["$C_{12}$ types merged" "Count"]
    :row-vectors (mapv (fn [[reps cnt]]
                         [(str reps) cnt])
                       merged-rows)}))

;; The rows with count 2 are pairs of inversionally related chord types
;; that become one type under $D_{12}$. Count 1 means the chord type
;; is its own inversion (palindromic interval structure).

;; ## Interval Vectors and the Z-Relation
;;
;; The **[interval vector](https://en.wikipedia.org/wiki/Interval_vector)** counts how many of each interval class a chord
;; contains. Most chord types have unique interval vectors, but occasionally
;; two different types share the same one — the **[Z-relation](https://en.wikipedia.org/wiki/Z-relation)**.
;;
;; For trichords under $D_{12}$, no Z-relation occurs. Let's verify:

(let [G (hm/dihedral-group 12)
      act (fn [[t k] x]
            (case t
              :r (mod (+ (long x) (long k)) 12)
              :s (mod (- (long k) (long x)) 12)))
      {:keys [domain] act-sub :act} (hm/subset-action act (range 12) 3)
      orbs (hm/orbits G act-sub domain)
      reps (mapv #(first (sort %)) orbs)
      ivs (mapv interval-vector reps)
      iv-groups (group-by identity ivs)]
  (every? #(= 1 (count (val %))) iv-groups))

(kind/test-last [true?])

;; All 12 trichord types (under $D_{12}$) have distinct interval vectors —
;; no Z-relation among trichords.

;; ## Beyond Trichords
;;
;; The same analysis applies to any chord size. Let's count set classes
;; for all sizes:

(let [G-c (hm/cyclic-group 12)
      G-d (hm/dihedral-group 12)
      act-c (fn [g x] (mod (+ (long x) (long g)) 12))
      act-d (fn [[t k] x]
              (case t
                :r (mod (+ (long x) (long k)) 12)
                :s (mod (- (long k) (long x)) 12)))
      results (mapv (fn [k]
                      (let [{domain-k :domain act-c-k :act} (hm/subset-action act-c (range 12) k)
                            {_ :domain act-d-k :act} (hm/subset-action act-d (range 12) k)
                            n-trans (count (hm/orbits G-c act-c-k domain-k))
                            n-dihed (count (hm/orbits G-d act-d-k domain-k))]
                        {:k k
                         :subsets (count domain-k)
                         :under-C12 n-trans
                         :under-D12 n-dihed}))
                    (range 1 12))]
  (kind/table
   {:column-names ["Chord size $k$" "Total subsets" "Types ($C_{12}$)" "Types ($D_{12}$)"]
    :row-vectors (mapv (fn [{:keys [k subsets under-C12 under-D12]}]
                         [k subsets under-C12 under-D12])
                       results)}))

;; The symmetry between $k$ and $12-k$ is the complement relation:
;; a $k$-note chord and its $(12-k)$-note complement are in bijection.

;; ## Connecting to the Fourier Transform
;;
;; The characteristic function of a chord is a function on $\mathbb{Z}/12\mathbb{Z}$:
;; $f(x) = 1$ if pitch $x$ is in the chord, $0$ otherwise.
;;
;; The Fourier transform of this function decomposes it into the irreducible
;; representations of $\mathbb{Z}/12\mathbb{Z}$ — the 12 "frequency" components.

(let [G (hm/cyclic-group 12)
      ct (hm/character-table G)
      ;; C major = {0, 4, 7}
      f-vals (cx/complex-tensor-real (mapv (fn [x] (if (#{0 4 7} x) 1.0 0.0)) (range 12)))
      f-hat (hm/fourier-transform ct f-vals)]
  (kind/table
   {:column-names ["Frequency $k$" "$|\\hat{f}(k)|^2$"]
    :row-vectors (mapv (fn [k]
                         (let [fk (f-hat k)
                               mag-sq (let [r (cx/re fk) i (cx/im fk)] (+ (* r r) (* i i)))]
                           [k (format "%.4f" mag-sq)]))
                       (range 12))}))

;; The Fourier magnitudes are invariant under transposition — transposing
;; by $k$ multiplies each $\hat{f}(j)$ by $e^{2\pi i j k/12}$, which doesn't
;; change the magnitude. This is why the Fourier magnitudes detect chord
;; *type*, not chord *position*.

;; Let's verify: the major triad and its transposition by a tritone (6 semitones)
;; have the same Fourier magnitude profile:

(let [G (hm/cyclic-group 12)
      ct (hm/character-table G)
      chord-a [0 4 7]
      chord-b [6 10 1]
      f-a (cx/complex-tensor-real (mapv (fn [x] (if ((set chord-a) x) 1.0 0.0)) (range 12)))
      f-b (cx/complex-tensor-real (mapv (fn [x] (if ((set chord-b) x) 1.0 0.0)) (range 12)))
      hat-a (hm/fourier-transform ct f-a)
      hat-b (hm/fourier-transform ct f-b)
      mag-sq (fn [fk] (let [r (cx/re fk) i (cx/im fk)] (+ (* r r) (* i i))))]
  (every? (fn [k]
            (< (Math/abs (- (mag-sq (hat-a k)) (mag-sq (hat-b k)))) 1e-10))
          (range 12)))

(kind/test-last [true?])

;; ## Visualizing Chord Types
;;
;; Each trichord type is a triangle on the clock face. Let's visualize
;; a few familiar chord types:

(chord-plot #{0 4 7} "Major triad")

(chord-plot #{0 3 7} "Minor triad")

(chord-plot #{0 4 8} "Augmented triad")

(chord-plot #{0 3 6} "Diminished triad")

;; The augmented triad is maximally symmetric — an equilateral triangle.
;; Its orbit under $C_{12}$ has only 4 elements (each transposition by
;; 4 semitones returns the same chord). The major and minor triads are
;; related by inversion.


;; ## Forte Numbers
;;
;; Allen Forte's catalog assigns a standard identifier to each set class
;; under $D_{12}$ equivalence. The [Forte number](https://en.wikipedia.org/wiki/Forte_number) $k\text{-}m$ means the $m$-th
;; set class of cardinality $k$ in Forte's ordering.
;;
;; We can compute prime forms and assign Forte numbers from first principles
;; using the $D_{12}$ orbits we already have.

(defn prime-form
  "Compute the prime form of a pitch class set under TnI equivalence.
  The prime form is the most compact representative: transpose all
  rotations and inversions to start at 0, then pick the lexicographically
  smallest."
  [pcs]
  (let [pcs-vec (vec (sort pcs))
        n 12
        ;; All transpositions
        transpositions (for [k (range n)]
                         (vec (sort (map #(mod (+ % k) n) pcs-vec))))
        ;; All inversions followed by transpositions
        inversions (for [k (range n)]
                     (vec (sort (map #(mod (- k %) n) pcs-vec))))
        ;; Normalize each to start at 0
        normalize (fn [s] (let [base (first s)]
                            (mapv #(mod (- % base) n) s)))
        candidates (map normalize (concat transpositions inversions))]
    (first (sort candidates))))

;; The 12 trichord set classes under $D_{12}$, computed by harmonica
;; and labeled with [Forte numbers](https://en.wikipedia.org/wiki/List_of_set_classes):

(let [G (hm/dihedral-group 12)
      act (fn [[t k] x]
            (case t
              :r (mod (+ (long x) (long k)) 12)
              :s (mod (- (long k) (long x)) 12)))
      {:keys [domain] act-sub :act} (hm/subset-action act (range 12) 3)
      orbs (hm/orbits G act-sub domain)
      primes (sort (mapv (fn [orb]
                           (prime-form (first orb)))
                         orbs))
      ;; Forte's catalog order for trichords
      forte-catalog [[0 1 2] [0 1 3] [0 1 4] [0 1 5] [0 1 6]
                     [0 2 4] [0 2 5] [0 2 6] [0 2 7]
                     [0 3 6] [0 3 7] [0 4 8]]
      forte-names ["3-1" "3-2" "3-3" "3-4" "3-5"
                   "3-6" "3-7" "3-8" "3-9"
                   "3-10" "3-11" "3-12"]
      musical-names ["chromatic cluster" "—" "—" "—" "Viennese trichord"
                     "whole-tone" "—" "—" "stack of fifths"
                     "diminished" "major/minor triad" "augmented triad"]]
  (kind/table
   {:column-names ["Forte number" "Prime form" "Interval vector" "Musical name"]
    :row-vectors (mapv (fn [forte pf name]
                         [forte (str pf) (str (interval-vector pf)) name])
                       forte-names forte-catalog musical-names)}))

;; Verify that our computed prime forms match the standard catalog exactly:

(let [G (hm/dihedral-group 12)
      act (fn [[t k] x]
            (case t
              :r (mod (+ (long x) (long k)) 12)
              :s (mod (- (long k) (long x)) 12)))
      {:keys [domain] act-sub :act} (hm/subset-action act (range 12) 3)
      orbs (hm/orbits G act-sub domain)
      computed (sort (mapv (fn [orb] (prime-form (first orb))) orbs))
      catalog [[0 1 2] [0 1 3] [0 1 4] [0 1 5] [0 1 6]
               [0 2 4] [0 2 5] [0 2 6] [0 2 7]
               [0 3 6] [0 3 7] [0 4 8]]]
  (= computed catalog))

(kind/test-last [true?])

;; The same approach works for tetrachords. There are 29 tetrachord
;; set classes under $D_{12}$:

(let [G (hm/dihedral-group 12)
      act (fn [[t k] x]
            (case t
              :r (mod (+ (long x) (long k)) 12)
              :s (mod (- (long k) (long x)) 12)))
      {:keys [domain] act-sub :act} (hm/subset-action act (range 12) 4)
      orbs (hm/orbits G act-sub domain)]
  (count orbs))

(kind/test-last [= 29])

;; Two of these — 4-Z15 and 4-Z29 — share the same interval vector
;; $\langle111111\rangle$ despite being different set classes. This is
;; the [Z-relation](https://en.wikipedia.org/wiki/Z-relation), the
;; only instance among tetrachords:

(let [z15 [0 1 4 6]
      z29 [0 1 3 7]]
  (= (interval-vector z15) (interval-vector z29)))

(kind/test-last [true?])

;; ## Summary
;;
;; This notebook demonstrated:
;;
;; - **Pitch classes as $\mathbb{Z}/12\mathbb{Z}$**: the chromatic scale is a cyclic group
;; - **Transposition as group action**: $C_{12}$ acts on pitch class subsets
;; - **Chord types as orbits**: 220 trichords $\to$ 19 types under $C_{12}$, 12 under $D_{12}$
;; - **Interval vectors**: a transposition-invariant fingerprint of a chord
;; - **Fourier magnitudes**: another invariant, connecting to representation theory
;; - **Inversional equivalence**: the dihedral group $D_{12}$ merges chord/inversion pairs
;; - **Forte numbers**: standard catalog of set classes, computed from $D_{12}$ orbits
;; - **Z-relation**: distinct set classes sharing the same interval vector

;; For another perspective on music and group theory — transforming
;; melodies via the Klein four-group — see
;; [Hearing Symmetry](hearing_symmetry.html).

;; For the general framework of Burnside counting and Pólya enumeration, see
;; [Counting Necklaces](counting_necklaces.html).

