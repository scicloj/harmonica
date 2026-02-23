;; # Quickstart
;;
;; [harmonica](https://github.com/scicloj/harmonica) is a Clojure library
;; for computational [group theory](https://en.wikipedia.org/wiki/Group_theory)
;; and [representation theory](https://en.wikipedia.org/wiki/Representation_theory).
;;
;; **Groups are the mathematics of symmetry.** When a problem has symmetry,
;; group theory turns brute force into elegant formulas. This page shows
;; four examples — each solved in a few lines.

(ns harmonica-book.quickstart
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Counting necklaces
;;
;; With 8 beads and 3 colors, there are $3^8 = 6561$ possible colorings.
;; But many are just rotations of each other. How many are truly distinct?
;;
;; The cyclic group $C_8$ captures rotational symmetry. Its
;; [cycle index](https://en.wikipedia.org/wiki/Cycle_index) encodes the
;; structure of all 8 rotations, and
;; [Pólya's theorem](https://en.wikipedia.org/wiki/Pólya_enumeration_theorem)
;; evaluates it — no enumeration needed.

(defn rotation-action [n]
  (fn [g x] (mod (+ x g) n)))

(let [G (hm/cyclic-group 8)
      ci (hm/cycle-index G (rotation-action 8) (range 8))]
  (hm/polya-count ci 3))

(kind/test-last [= 834])

;; If flipping the necklace over also counts as "the same," the symmetry
;; group is the [dihedral group](https://en.wikipedia.org/wiki/Dihedral_group)
;; $D_8$ (rotations + reflections):

(defn dihedral-action [n]
  (fn [[t k] x]
    (case t
      :r (mod (+ x k) n)
      :s (mod (- k x) n))))

(let [G (hm/dihedral-group 8)
      ci (hm/cycle-index G (dihedral-action 8) (range 8))]
  (hm/polya-count ci 3))

(kind/test-last [= 498])

;; 834 necklaces, 498 bracelets — from the group's structure alone.
;; This works even for $n = 100$ where enumeration is impossible.
;; For the full story, see
;; [Counting Necklaces](counting_necklaces.html).

;; ## The DFT you already know
;;
;; The [Discrete Fourier Transform](https://en.wikipedia.org/wiki/Discrete_Fourier_transform) —
;; behind MP3, JPEG, and spectral analysis — is the Fourier transform on the
;; cyclic group $\mathbb{Z}/n\mathbb{Z}$. The character table **is** the
;; DFT matrix.

(def G (hm/cyclic-group 24))
(def ct (hm/character-table G))

;; Monthly temperatures (°C) over two years:

(def temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3
   3 4 8 13 18 23 26 25 20 14 8 4])

(-> (tc/dataset {:month (range 24) :temp temperatures})
    (plotly/base {:=x :month :=y :temp
                  :=title "Monthly temperatures (°C)"
                  :=x-title "month" :=y-title "°C"})
    (plotly/layer-line)
    (plotly/layer-point {:=mark-size 5})
    plotly/plot)

(def f-hat (hm/fourier-transform ct (cx/complex-tensor-real temperatures)))

f-hat

;; The Fourier magnitudes reveal which frequencies carry the signal's energy:

(let [n 24
      magnitudes (mapv (fn [k] {:k k :magnitude (cx/cabs (f-hat k))})
                       (range (inc (/ n 2))))]
  (-> (tc/dataset magnitudes)
      (plotly/base {:=x :k :=y :magnitude})
      (plotly/layer-bar)
      (plotly/update-data
       (fn [d] (assoc d :=layout
                      {:title "Fourier magnitudes |f̂(k)|"
                       :xaxis {:title "frequency k" :dtick 1}
                       :yaxis {:title "|f̂(k)|"}
                       :width 500 :height 300})))
      plotly/plot))

;; The DC component ($k = 0$) is the sum of all values. The dominant
;; non-DC component is $k = 2$ — two cycles in 24 months, the annual cycle:

(cx/re (f-hat 0))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 320.0)) 1e-10))])

;; Verify $k = 2$ dominates: its magnitude exceeds all other non-DC components.

(let [mags (mapv (fn [k] [k (cx/cabs (f-hat k))]) (range 1 (inc (/ 24 2))))]
  (first (apply max-key second mags)))

(kind/test-last [= 2])

;; Inverse transform recovers the original signal exactly:

(let [recovered (cx/re (hm/inverse-fourier-transform ct f-hat))]
  (< (dfn/reduce-max (dfn/abs (dfn/- recovered temperatures))) 1e-10))

(kind/test-last [true?])

;; This generalizes: the same framework (group → character table →
;; Fourier transform) works for **any** finite group. For the full story,
;; see [The DFT as Group Fourier Transform](dft_as_group_fourier.html).

;; ## Symmetry you can see
;;
;; Draw one curve. Apply the dihedral group $D_6$ — rotations and
;; reflections of a hexagon — and get a snowflake.

(defn make-rosette [n motif]
  (let [matrices
        (mapcat
         (fn [k]
           (let [a (* 2.0 Math/PI (/ (double k) (double n)))]
             [[[(Math/cos a) (- (Math/sin a))]
               [(Math/sin a) (Math/cos a)]]
              [[(Math/cos a) (Math/sin a)]
               [(Math/sin a) (- (Math/cos a))]]]))
         (range n))]
    (mapv (fn [[[a b] [c d]]]
            (mapv (fn [[x y]] [(+ (* a x) (* b y))
                               (+ (* c x) (* d y))])
                  motif))
          matrices)))

(let [motif (mapv (fn [i]
                    (let [t (/ (double i) 30)
                          r (+ 0.3 (* 0.5 t))
                          a (* t 0.9)]
                      [(* r (Math/cos a)) (* r (Math/sin a))]))
                  (range 31))
      copies (make-rosette 6 motif)
      colors (cycle ["#e74c3c" "#3498db" "#2ecc71" "#f39c12" "#9b59b6" "#e67e22"
                     "#c0392b" "#2980b9" "#27ae60" "#d35400" "#8e44ad" "#1abc9c"])]
  (kind/plotly
   {:data (vec (map-indexed
                (fn [i copy]
                  {:type "scatter" :mode "lines"
                   :x (mapv first copy)
                   :y (mapv second copy)
                   :line {:color (nth colors i) :width 2}
                   :showlegend false})
                copies))
    :layout {:title "D₆ rosette — 12 copies of one curve"
             :xaxis {:visible false :scaleanchor "y"}
             :yaxis {:visible false}
             :width 400 :height 400}}))

;; Each copy is a group element's action on the original curve.
;; The $D_6$ group has 12 elements — 6 rotations and 6 reflections —
;; producing 12 copies. For more, see
;; [Symmetry Sketchpad](symmetry_sketchpad.html).

;; ## Symmetry you can hear
;;
;; The [Klein four-group](https://en.wikipedia.org/wiki/Klein_four-group)
;; $V_4$ gives four ways to transform a melody:
;; original, retrograde (backwards), inversion (upside down),
;; and retrograde inversion (both). Composers from Bach to Schoenberg
;; have used these symmetries as compositional tools.

(def V4 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2)))

;; The subject of Bach's [Art of Fugue](https://en.wikipedia.org/wiki/The_Art_of_Fugue) — a theme Bach designed
;; to work under all four $V_4$ transformations. Pairs of `[pitch duration]`
;; (MIDI note numbers, duration in seconds):

(def motif [[62 0.8] [69 0.8] [65 0.8] [62 0.5] [61 0.5] [62 0.5] [64 0.5] [65 0.8]])

(defn apply-v4 [pivot [r i] melody]
  (let [m (if (= i 1) (mapv (fn [[p d]] [(- (* 2 pivot) p) d]) melody) melody)]
    (if (= r 1) (vec (reverse m)) m)))

;; The four forms:

(let [pivot (ffirst motif)]
  {:original (mapv first motif)
   :retrograde (mapv first (apply-v4 pivot [1 0] motif))
   :inversion (mapv first (apply-v4 pivot [0 1] motif))
   :retrograde-inv (mapv first (apply-v4 pivot [1 1] motif))})

;; Synthesize and play them:

(def sample-rate 44100.0)

(defn play [melody]
  (let [amp 2500.0
        offsets (reductions + 0 (map (fn [[_ d]] (long (* d sample-rate))) melody))
        total (last offsets)]
    (kind/audio
     {:sample-rate sample-rate
      :samples
      (dtype/make-reader
       :float32 total
       (let [[note-idx note-start]
             (loop [i 0]
               (if (< idx (nth offsets (inc i)))
                 [i (nth offsets i)]
                 (recur (inc i))))
             [pitch dur] (melody note-idx)
             n-note (long (* dur sample-rate))
             t (- idx note-start)
             attack (long (* 0.015 sample-rate))
             sounding (long (* 0.85 n-note))
             release (long (* 0.06 sample-rate))
             freq (* 440.0 (Math/pow 2.0 (/ (- (double pitch) 69.0) 12.0)))
             phase (/ (double t) sample-rate)]
         (if (>= t sounding)
           (float 0.0)
           (let [env (cond
                       (< t attack) (/ (double t) attack)
                       (> t (- sounding release))
                       (* (Math/exp (* -1.5 (/ (double (- t attack)) sounding)))
                          (/ (double (- sounding t)) release))
                       :else (Math/exp (* -1.5 (/ (double (- t attack)) sounding))))
                 wave (+ (* 0.65 (Math/sin (* 2.0 Math/PI freq phase)))
                         (* 0.25 (Math/sin (* 2.0 Math/PI 2.0 freq phase)))
                         (* 0.10 (Math/sin (* 2.0 Math/PI 3.0 freq phase))))]
             (float (* amp env wave))))))})))

;; Traverse the whole group — each element transforms the motif differently:

(play motif)

(play (apply-v4 (ffirst motif) [1 0] motif))

(play (apply-v4 (ffirst motif) [0 1] motif))

(play (apply-v4 (ffirst motif) [1 1] motif))

;; For twelve-tone rows, transposition, and more musical group theory,
;; see [Hearing Symmetry](hearing_symmetry.html).

;; ## The library at a glance
;;
;; harmonica provides:
;;
;; | Concept | Groups |
;; |:--------|:-------|
;; | [Cyclic groups](groups_and_structure.html) Z/nZ | Clock arithmetic, DFT |
;; | [Dihedral groups](groups_and_structure.html) Dₙ | Polygon symmetries, necklaces |
;; | [Symmetric groups](symmetric_groups.html) Sₙ | Permutations, card shuffling |
;; | [Product groups](groups_and_structure.html) G₁ × G₂ | 2D DFT, Klein four-group |
;;
;; | Tool | Purpose |
;; |:-----|:--------|
;; | [Character tables](character_theory.html) | Fourier-analytic backbone |
;; | [Fourier transform](dft_as_group_fourier.html) | Decompose functions on groups |
;; | [Group actions](counting_necklaces.html) | Orbits, Burnside counting, Pólya |
;; | [Representation matrices](representation_matrices.html) | Explicit matrix irreps via Young tableaux |
;;
;; ## Where to start
;;
;; - **[Groups and Structure](groups_and_structure.html)** — what groups
;;   are and the group families in the library
;; - **[The DFT as Group Fourier Transform](dft_as_group_fourier.html)** —
;;   the connection between the DFT and cyclic groups
;; - **[Random Walks and Convolution](random_walks.html)** — convergence
;;   to uniform via Fourier eigenvalues
;; - **[Symmetry Sketchpad](symmetry_sketchpad.html)** — draw rosette
;;   patterns with dihedral groups
;; - **[Counting Necklaces](counting_necklaces.html)** — Burnside's lemma
;;   and Pólya enumeration
;; - **[Chord Geometry](chord_geometry.html)** — music theory as group action
;; - **[Hearing Symmetry](hearing_symmetry.html)** — the Klein four-group
;;   in melodic transformations
;; - **[Random Transpositions](random_transpositions.html)** — the cutoff
;;   phenomenon in card shuffling
;;
;; For a look at the internal data structures and algorithms, see
;; [Under the Hood](data_representations.html).
