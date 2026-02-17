;; # API Reference
;;
;; Complete reference for `scicloj.reel.core` — the public API for
;; computational group theory and representation theory.

^{:kindly/hide-code true
  :kindly/options {:kinds-that-hide-code #{:kind/doc}}}
(ns reel-book.api-reference
  (:require
   [scicloj.reel.core :as reel]
   [scicloj.reel.representations :as rep]
   [fastmath.complex :as c]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Group Constructors

(kind/doc #'reel/cyclic-group)

(reel/cyclic-group 5)

(kind/test-last [(fn [v] (= (reel/order v) 5))])

(kind/doc #'reel/symmetric-group)

(reel/symmetric-group 3)

(kind/test-last [(fn [v] (= (reel/order v) 6))])

(kind/doc #'reel/dihedral-group)

(reel/dihedral-group 4)

(kind/test-last [(fn [v] (= (reel/order v) 8))])

(kind/doc #'reel/product-group)

(reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 3))

(kind/test-last [(fn [v] (= (reel/order v) 6))])

;; ## Group Operations

(kind/doc #'reel/op)

(reel/op (reel/cyclic-group 7) 3 5)

(kind/test-last [= 1])

(reel/op (reel/symmetric-group 3) [1 2 0] [0 2 1])

(kind/test-last [= [1 0 2]])

(kind/doc #'reel/inv)

(reel/inv (reel/cyclic-group 7) 3)

(kind/test-last [= 4])

(reel/inv (reel/symmetric-group 3) [1 2 0])

(kind/test-last [= [2 0 1]])

(kind/doc #'reel/id)

(reel/id (reel/cyclic-group 5))

(kind/test-last [= 0])

(reel/id (reel/symmetric-group 3))

(kind/test-last [= [0 1 2]])

(reel/id (reel/dihedral-group 4))

(kind/test-last [= [:r 0]])

(kind/doc #'reel/elements)

(vec (reel/elements (reel/cyclic-group 4)))

(kind/test-last [= [0 1 2 3]])

(kind/doc #'reel/order)

(reel/order (reel/symmetric-group 4))

(kind/test-last [= 24])

(kind/doc #'reel/conjugacy-classes)

(let [classes (reel/conjugacy-classes (reel/symmetric-group 3))]
  (mapv :size classes))

(kind/test-last [= [2 3 1]])

;; ## Permutation Utilities

(kind/doc #'reel/cycles)

(reel/cycles [1 2 3 0])

(kind/test-last [= [[0 1 2 3]]])

(reel/cycles [1 0 3 2])

(kind/test-last [= [[0 1] [2 3]]])

(kind/doc #'reel/cycle-type)

(reel/cycle-type [1 0 3 2])

(kind/test-last [= [2 2]])

(kind/doc #'reel/sign)

(reel/sign [1 0 2 3])

(kind/test-last [= -1])

(reel/sign [0 1 2 3])

(kind/test-last [= 1])

(kind/doc #'reel/identity-perm)

(reel/identity-perm 4)

(kind/test-last [= [0 1 2 3]])

(kind/doc #'reel/transposition)

(reel/transposition 5 1 3)

(kind/test-last [= [0 3 2 1 4]])

(kind/doc #'reel/adjacent-transposition-decomposition)

(reel/adjacent-transposition-decomposition [2 0 1])

(kind/test-last [vector?])

;; ## Partitions

(kind/doc #'reel/partitions)

(reel/partitions 4)

(kind/test-last [= [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]])

(kind/doc #'reel/partition-conjugate)

(reel/partition-conjugate [4 2 1])

(kind/test-last [= [3 2 1 1]])

;; ## Young Tableaux

(kind/doc #'reel/standard-young-tableaux)

(reel/standard-young-tableaux [2 1])

(kind/test-last [= [[[1 2] [3]] [[1 3] [2]]]])

(kind/doc #'reel/hook-length-dimension)

(reel/hook-length-dimension [3 2])

(kind/test-last [= 5])

(reel/hook-length-dimension [2 2 1])

(kind/test-last [= 5])

;; ## Characters

(kind/doc #'reel/character-table)

(let [ct (reel/character-table (reel/cyclic-group 3))]
  (count (:table ct)))

(kind/test-last [= 3])

(let [ct (reel/character-table (reel/symmetric-group 3))
      re-table (mapv (fn [row] (mapv #(long (Math/round (c/re %))) row))
                     (:table ct))]
  re-table)

(kind/test-last [= [[1 1 1] [2 0 -1] [1 -1 1]]])

(kind/doc #'reel/character-inner-product)

(let [ct (reel/character-table (reel/symmetric-group 3))
      {:keys [table class-sizes]} ct
      order (reel/order (:group ct))]
  (c/re (reel/character-inner-product (nth table 0) (nth table 1) class-sizes order)))

(kind/test-last [(fn [v] (< (Math/abs v) 1e-10))])

;; ## Representations

(kind/doc #'reel/irrep)

(let [ir (reel/irrep [2 1])]
  (reel/rep-dimension ir))

(kind/test-last [= 2])

(kind/doc #'reel/rep-matrix)

(let [ir (reel/irrep [2 1])]
  (fm/nrow (reel/rep-matrix ir [1 0 2])))

(kind/test-last [= 2])

(kind/doc #'reel/rep-dimension)

(reel/rep-dimension (reel/irrep [3 1]))

(kind/test-last [= 3])

(kind/doc #'reel/rep-character)

(let [ir (reel/irrep [2 1])]
  (reel/rep-character ir [0 1 2]))

(kind/test-last [(fn [v] (< (Math/abs (- v 2.0)) 1e-10))])

(kind/doc #'reel/rep-generators)

(let [ir (reel/irrep [2 1])]
  (count (reel/rep-generators ir)))

(kind/test-last [= 2])

(kind/doc #'reel/tensor-product)

(let [ir1 (reel/irrep [2 1])
      ir2 (reel/irrep [2 1])
      tp (reel/tensor-product ir1 ir2)]
  (reel/rep-dimension tp))

(kind/test-last [= 4])

(kind/doc #'reel/direct-sum)

(let [ir1 (reel/irrep [2 1])
      ir2 (reel/irrep [1 1 1])
      ds (reel/direct-sum ir1 ir2)]
  (reel/rep-dimension ds))

(kind/test-last [= 3])

(kind/doc #'reel/frobenius-norm-sq)

(let [M (fm/rows->mat [[1.0 0.0] [0.0 1.0]])]
  (reel/frobenius-norm-sq M))

(kind/test-last [(fn [v] (< (Math/abs (- v 2.0)) 1e-10))])

(kind/doc #'reel/frobenius-norm)

(let [M (fm/rows->mat [[3.0 0.0] [0.0 4.0]])]
  (reel/frobenius-norm M))

(kind/test-last [(fn [v] (< (Math/abs (- v 5.0)) 1e-10))])

(kind/doc #'reel/matrix-fourier-transform)

(let [G (reel/symmetric-group 3)
      ir (reel/irrep [2 1])
      f (zipmap (reel/elements G) (repeat 1.0))
      fhat (reel/matrix-fourier-transform ir G f)]
  (fm/nrow fhat))

(kind/test-last [= 2])

;; ## Riffle Shuffles

(kind/doc #'reel/rising-sequences)

(reel/rising-sequences [0 1 2 3])

(kind/test-last [= 1])

(reel/rising-sequences [3 2 1 0])

(kind/test-last [= 4])

(kind/doc #'reel/gsr-probability)

(let [p (reel/gsr-probability [0 1 2 3] 1)]
  (> p 0.0))

(kind/test-last [true?])

;; ## Group Actions

(kind/doc #'reel/orbit)

(let [G (reel/cyclic-group 4)
      act (fn [g x] (mod (+ (long g) (long x)) 4))]
  (reel/orbit G act 0))

(kind/test-last [= #{0 1 2 3}])

(kind/doc #'reel/orbits)

(let [G (reel/cyclic-group 3)
      act (fn [g x] (mod (+ (long g) (long x)) 3))]
  (count (reel/orbits G act (range 3))))

(kind/test-last [= 1])

(kind/doc #'reel/fixed-points)

(let [act (fn [g x] (mod (+ (long g) (long x)) 5))]
  (reel/fixed-points act 0 (range 5)))

(kind/test-last [= #{0 1 2 3 4}])

(kind/doc #'reel/stabilizer)

(let [G (reel/cyclic-group 4)
      act (fn [g x] (mod (+ (long g) (long x)) 4))]
  (reel/stabilizer G act 0))

(kind/test-last [= #{0}])

(kind/doc #'reel/burnside-count)

(let [G (reel/cyclic-group 4)
      act (fn [g coloring] (mapv #(coloring (mod (+ % (long g)) 4)) (range 4)))
      domain [[0 0 0 0] [0 0 0 1] [0 0 1 0] [0 0 1 1]
              [0 1 0 0] [0 1 0 1] [0 1 1 0] [0 1 1 1]
              [1 0 0 0] [1 0 0 1] [1 0 1 0] [1 0 1 1]
              [1 1 0 0] [1 1 0 1] [1 1 1 0] [1 1 1 1]]]
  (reel/burnside-count G act domain))

(kind/test-last [= 6])

(kind/doc #'reel/cycle-index)

(let [G (reel/cyclic-group 3)
      act (fn [g x] (mod (+ (long g) (long x)) 3))
      ci (reel/cycle-index G act (range 3))]
  (= 1 (reduce + (vals ci))))

(kind/test-last [true?])

(kind/doc #'reel/polya-count)

(let [G (reel/cyclic-group 4)
      act (fn [g x] (mod (+ (long g) (long x)) 4))
      ci (reel/cycle-index G act (range 4))]
  (reel/polya-count ci 2))

(kind/test-last [= 6])

(kind/doc #'reel/subset-action)

(let [perm-act (fn [sigma x] (sigma x))
      {:keys [domain]} (reel/subset-action perm-act (range 4) 2)]
  (count domain))

(kind/test-last [= 6])

;; ## Fourier Analysis

(kind/doc #'reel/fourier-transform)

(let [ct (reel/character-table (reel/cyclic-group 4))
      f (mapv #(c/complex (double %) 0.0) [1 0 0 0])
      fhat (reel/fourier-transform ct f)]
  (count fhat))

(kind/test-last [= 4])

(kind/doc #'reel/inverse-fourier-transform)

(let [ct (reel/character-table (reel/cyclic-group 4))
      f (mapv #(c/complex (double %) 0.0) [1 2 3 4])
      fhat (reel/fourier-transform ct f)
      f-back (reel/inverse-fourier-transform ct fhat)
      max-err (apply max (map #(c/abs (c/sub %1 %2)) f f-back))]
  (< max-err 1e-10))

(kind/test-last [true?])

(kind/doc #'reel/convolve)

(let [ct (reel/character-table (reel/cyclic-group 4))
      f (mapv #(c/complex (double %) 0.0) [1 0 0 0])
      g (mapv #(c/complex (double %) 0.0) [0 1 0 0])
      conv (reel/convolve ct f g)]
  (long (Math/round (c/re (nth conv 1)))))

(kind/test-last [= 1])

(kind/doc #'reel/total-variation-distance)

(reel/total-variation-distance [0.5 0.5 0.0 0.0] [0.25 0.25 0.25 0.25])

(kind/test-last [(fn [v] (< (Math/abs (- v 0.5)) 1e-10))])

