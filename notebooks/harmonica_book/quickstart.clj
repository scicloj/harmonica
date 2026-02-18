;; # Quickstart
;;
;; [harmonica](https://github.com/scicloj/harmonica) is a Clojure library
;; for computational [group theory](https://en.wikipedia.org/wiki/Group_theory)
;; and [representation theory](https://en.wikipedia.org/wiki/Representation_theory).
;;
;; **Groups are the mathematics of symmetry.** When a problem has symmetry,
;; group theory turns brute force into elegant formulas. This page shows
;; three examples — each solved in a few lines.

(ns harmonica-book.quickstart
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
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
  (fn [g x] (mod (+ (long x) (long g)) n)))

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
      :r (mod (+ (long x) (long k)) n)
      :s (mod (- (long k) (long x)) n))))

(let [G (hm/dihedral-group 8)
      ci (hm/cycle-index G (dihedral-action 8) (range 8))]
  (hm/polya-count ci 3))

(kind/test-last [= 462])

;; 834 necklaces, 462 bracelets — from the group's structure alone.
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

(def f-hat (hm/fourier-transform ct (cx/complex-tensor-real temperatures)))

;; The dominant component is $k = 2$ — two cycles in 24 months, the
;; annual cycle. The DC component ($k = 0$) is the sum of all values:

(cx/re (f-hat 0))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 320.0)) 1e-10))])

;; Inverse transform recovers the original signal exactly:

(let [recovered (hm/inverse-fourier-transform ct f-hat)]
  (every? #(< (Math/abs (double %)) 1e-10)
          (map - (vec (cx/re recovered)) temperatures)))

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

;; ## The library at a glance
;;
;; harmonica provides:
;;
;; | Concept | Groups |
;; |:--------|:-------|
;; | [Cyclic groups](groups_and_structure.html) $\mathbb{Z}/n\mathbb{Z}$ | Clock arithmetic, DFT |
;; | [Dihedral groups](groups_and_structure.html) $D_n$ | Polygon symmetries, necklaces |
;; | [Symmetric groups](symmetric_groups.html) $S_n$ | Permutations, card shuffling |
;; | [Product groups](groups_and_structure.html) $G_1 \times G_2$ | 2D DFT, Klein four-group |
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
;;   are and the four families in the library
;; - **[The DFT as Group Fourier Transform](dft_as_group_fourier.html)** —
;;   the connection between the DFT and cyclic groups
;; - **[Symmetry Sketchpad](symmetry_sketchpad.html)** — draw rosette
;;   patterns with dihedral groups
;; - **[Counting Necklaces](counting_necklaces.html)** — Burnside's lemma
;;   and Pólya enumeration
;; - **[Chord Geometry](chord_geometry.html)** — music theory as group action
;; - **[Random Transpositions](random_transpositions.html)** — the cutoff
;;   phenomenon in card shuffling
