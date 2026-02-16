;; # Quickstart
;;
;; A minimal introduction to reel — a library for computational group
;; theory and representation theory in Clojure.
;;
;; The central idea: the Discrete Fourier Transform that every programmer
;; knows is secretly the Fourier transform on the cyclic group. This library
;; makes that connection explicit and generalizes it.
;;
;; For the full story, see [The DFT as Group Fourier Transform](dft_as_group_fourier.html).

(ns reel-book.quickstart
  (:require
   [scicloj.reel.core :as reel]
   [fastmath.complex :as c]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Create a group

;; The cyclic group Z/24Z — integers 0 through 23 with addition mod 24.

(def G (reel/cyclic-group 24))

(reel/order G)

(kind/test-last
 [= 24])

(reel/elements G)

(kind/test-last
 [= (range 24)])

;; Group operations work on plain integers.

(reel/op G 15 9)

(kind/test-last
 [= 0])

(reel/inv G 15)

(kind/test-last
 [= 9])

;; ## Character table

;; The character table of Z/nZ is the DFT matrix — each row is a character
;; (irreducible representation), each column is a group element.

(def ct (reel/character-table G))

;; The first row is the trivial character: all ones.

(every? #(< (Math/abs (- (c/re %) 1.0)) 1e-10) ((:table ct) 0))

(kind/test-last
 [true?])

;; Characters are orthonormal.

(let [chi-0 ((:table ct) 0)
      chi-1 ((:table ct) 1)
      sizes (:class-sizes ct)
      n 24]
  (c/abs (reel/character-inner-product chi-0 chi-1 sizes n)))

(kind/test-last
 [(fn [v] (< v 1e-10))])

;; ## Fourier transform

;; Apply the Fourier transform to a signal — a function on the group.
;; These 24 values represent monthly temperatures (°C) over two years.

(def temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3
   3 4 8 13 18 23 26 25 20 14 8 4])

(def signal (mapv #(c/complex (double %)) temperatures))

(def f-hat (reel/fourier-transform ct signal))

;; The DC component (k=0) is the sum of all values.

(c/re (f-hat 0))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 320.0)) 1e-10))])

;; Round-trip: inverse transform recovers the original signal.

(def reconstructed (reel/inverse-fourier-transform ct f-hat))

(every? #(< (Math/abs (double %)) 1e-10)
        (map - (mapv c/re reconstructed) temperatures))

(kind/test-last
 [true?])

;; ## Convolution

;; Convolution in the group domain equals pointwise multiplication
;; in the Fourier domain.

(def f (mapv #(c/complex (double %))
             [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))
(def h (mapv #(c/complex (double %))
             [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))

(def convolved (reel/convolve ct f h))

(mapv #(Math/round (c/re %)) convolved)

(kind/test-last
 [= [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])
