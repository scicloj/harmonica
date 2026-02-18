;; # Quickstart
;;
;; A minimal introduction to [harmonica](https://github.com/scicloj/harmonica) — a library for computational [group
;; theory](https://en.wikipedia.org/wiki/Group_theory) and [representation theory](https://en.wikipedia.org/wiki/Representation_theory) in Clojure.
;;
;; The central idea: the [Discrete Fourier Transform](https://en.wikipedia.org/wiki/Discrete_Fourier_transform) that every programmer
;; knows is secretly the Fourier transform on the [cyclic group](https://en.wikipedia.org/wiki/Cyclic_group). This library
;; makes that connection explicit and generalizes it.
;;
;; For the full story, see [The DFT as Group Fourier Transform](dft_as_group_fourier.html).

(ns harmonica-book.quickstart
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.complex :as cx]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Create a group

;; The cyclic group $\mathbb{Z}/24\mathbb{Z}$ — integers 0 through 23 with addition mod 24.

(def G (hm/cyclic-group 24))

(hm/order G)

(kind/test-last
 [= 24])

(hm/elements G)

(kind/test-last
 [= (range 24)])

;; Group operations work on plain integers.

(hm/op G 15 9)

(kind/test-last
 [= 0])

(hm/inv G 15)

(kind/test-last
 [= 9])

;; ## Character table

;; The character table of $\mathbb{Z}/n\mathbb{Z}$ is the DFT matrix — each row is a character
;; (irreducible representation), each column is a group element.

(def ct (hm/character-table G))

;; The first row is the trivial character: all ones.

(every? #(< (Math/abs (- (cx/re %) 1.0)) 1e-10) (seq ((:table ct) 0)))

(kind/test-last
 [true?])

;; Characters are orthonormal.

(let [chi-0 ((:table ct) 0)
      chi-1 ((:table ct) 1)
      sizes (:class-sizes ct)
      n 24]
  (cx/cabs (hm/character-inner-product chi-0 chi-1 sizes n)))

(kind/test-last
 [(fn [v] (< v 1e-10))])

;; ## Fourier transform

;; Apply the Fourier transform to a signal — a function on the group.
;; These 24 values represent monthly temperatures (°C) over two years.

(def temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3
   3 4 8 13 18 23 26 25 20 14 8 4])

(def f-hat (hm/fourier-transform ct (cx/complex-tensor-real temperatures)))

;; The DC component ($k = 0$) is the sum of all values.

(cx/re (f-hat 0))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 320.0)) 1e-10))])

;; Round-trip: inverse transform recovers the original signal.

(every? #(< (Math/abs (double %)) 1e-10)
        (map - (vec (cx/re (hm/inverse-fourier-transform ct f-hat)))
             temperatures))

(kind/test-last
 [true?])

;; ## Convolution

;; Convolution in the group domain equals pointwise multiplication
;; in the Fourier domain.

(let [f (cx/complex-tensor-real
              [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])
      h (cx/complex-tensor-real
              [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])]
  (mapv #(Math/round %) (vec (cx/re (hm/convolve ct f h)))))

(kind/test-last
 [= [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

;; ## What Else?
;;
;; This quickstart showed cyclic groups and abelian Fourier transforms.
;; The library also provides:
;;
;; - **[Symmetric groups](https://en.wikipedia.org/wiki/Symmetric_group)** ($S_n$) with [character tables](https://en.wikipedia.org/wiki/Character_table) via the [Murnaghan-Nakayama rule](https://en.wikipedia.org/wiki/Murnaghan%E2%80%93Nakayama_rule)
;; - **[Dihedral groups](https://en.wikipedia.org/wiki/Dihedral_group)** ($D_n$) — symmetries of regular polygons
;; - **Product groups** ($G_1 \times G_2$)
;; - **[Irreducible representations](https://en.wikipedia.org/wiki/Irreducible_representation)** via [Young's orthogonal form](https://en.wikipedia.org/wiki/Young%27s_orthogonal_representation)
;; - **[Group actions](https://en.wikipedia.org/wiki/Group_action)**, [Burnside's lemma](https://en.wikipedia.org/wiki/Burnside%27s_lemma), and [Pólya enumeration](https://en.wikipedia.org/wiki/P%C3%B3lya_enumeration_theorem)
;; - **Matrix-valued Fourier transforms** for non-abelian groups
;;
;; See the other notebooks for applications:
;; [Symmetric Groups](symmetric_groups.html),
;; [Random Transpositions](random_transpositions.html),
;; [Riffle Shuffles](riffle_shuffle.html),
;; [Counting Necklaces](counting_necklaces.html),
;; [Chord Geometry](chord_geometry.html),
;; [Hearing Symmetry](hearing_symmetry.html),
;; [Symmetry Sketchpad](symmetry_sketchpad.html).
