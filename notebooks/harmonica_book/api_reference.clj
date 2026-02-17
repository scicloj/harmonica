;; # API Reference
;;
;; Complete reference for `scicloj.harmonica.core` — the public API for
;; computational group theory and representation theory.

^{:kindly/hide-code true
  :kindly/options {:kinds-that-hide-code #{:kind/doc}}}
(ns harmonica-book.api-reference
  (:require
   [scicloj.harmonica.core :as hm]
   [scicloj.harmonica.representations :as rep]
   [fastmath.complex :as c]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Group Constructors

(kind/doc #'hm/cyclic-group)

(hm/cyclic-group 5)

(kind/test-last [(fn [v] (= (hm/order v) 5))])

(kind/doc #'hm/symmetric-group)

(hm/symmetric-group 3)

(kind/test-last [(fn [v] (= (hm/order v) 6))])

(kind/doc #'hm/dihedral-group)

(hm/dihedral-group 4)

(kind/test-last [(fn [v] (= (hm/order v) 8))])

(kind/doc #'hm/product-group)

(hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3))

(kind/test-last [(fn [v] (= (hm/order v) 6))])

;; ## Group Operations

(kind/doc #'hm/op)

(hm/op (hm/cyclic-group 7) 3 5)

(kind/test-last [= 1])

(hm/op (hm/symmetric-group 3) [1 2 0] [0 2 1])

(kind/test-last [= [1 0 2]])

(kind/doc #'hm/inv)

(hm/inv (hm/cyclic-group 7) 3)

(kind/test-last [= 4])

(hm/inv (hm/symmetric-group 3) [1 2 0])

(kind/test-last [= [2 0 1]])

(kind/doc #'hm/id)

(hm/id (hm/cyclic-group 5))

(kind/test-last [= 0])

(hm/id (hm/symmetric-group 3))

(kind/test-last [= [0 1 2]])

(hm/id (hm/dihedral-group 4))

(kind/test-last [= [:r 0]])

(kind/doc #'hm/elements)

(vec (hm/elements (hm/cyclic-group 4)))

(kind/test-last [= [0 1 2 3]])

(kind/doc #'hm/order)

(hm/order (hm/symmetric-group 4))

(kind/test-last [= 24])

(kind/doc #'hm/conjugacy-classes)

(let [classes (hm/conjugacy-classes (hm/symmetric-group 3))]
  (mapv :size classes))

(kind/test-last [= [2 3 1]])

;; ## Permutation Utilities

(kind/doc #'hm/cycles)

(hm/cycles [1 2 3 0])

(kind/test-last [= [[0 1 2 3]]])

(hm/cycles [1 0 3 2])

(kind/test-last [= [[0 1] [2 3]]])

(kind/doc #'hm/cycle-type)

(hm/cycle-type [1 0 3 2])

(kind/test-last [= [2 2]])

(kind/doc #'hm/sign)

(hm/sign [1 0 2 3])

(kind/test-last [= -1])

(hm/sign [0 1 2 3])

(kind/test-last [= 1])

(kind/doc #'hm/identity-perm)

(hm/identity-perm 4)

(kind/test-last [= [0 1 2 3]])

(kind/doc #'hm/transposition)

(hm/transposition 5 1 3)

(kind/test-last [= [0 3 2 1 4]])

(kind/doc #'hm/adjacent-transposition-decomposition)

(hm/adjacent-transposition-decomposition [2 0 1])

(kind/test-last [vector?])

;; ## Partitions

(kind/doc #'hm/partitions)

(hm/partitions 4)

(kind/test-last [= [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]])

(kind/doc #'hm/partition-conjugate)

(hm/partition-conjugate [4 2 1])

(kind/test-last [= [3 2 1 1]])

;; ## Young Tableaux

(kind/doc #'hm/standard-young-tableaux)

(hm/standard-young-tableaux [2 1])

(kind/test-last [= [[[1 2] [3]] [[1 3] [2]]]])

(kind/doc #'hm/hook-length-dimension)

(hm/hook-length-dimension [3 2])

(kind/test-last [= 5])

(hm/hook-length-dimension [2 2 1])

(kind/test-last [= 5])

;; ## Characters

(kind/doc #'hm/character-table)

(let [ct (hm/character-table (hm/cyclic-group 3))]
  (count (:table ct)))

(kind/test-last [= 3])

(let [ct (hm/character-table (hm/symmetric-group 3))
      re-table (mapv (fn [row] (mapv #(long (Math/round (c/re %))) row))
                     (:table ct))]
  re-table)

(kind/test-last [= [[1 1 1] [2 0 -1] [1 -1 1]]])

(kind/doc #'hm/character-inner-product)

(let [ct (hm/character-table (hm/symmetric-group 3))
      {:keys [table class-sizes]} ct
      order (hm/order (:group ct))]
  (c/re (hm/character-inner-product (nth table 0) (nth table 1) class-sizes order)))

(kind/test-last [(fn [v] (< (Math/abs v) 1e-10))])

;; ## Representations

(kind/doc #'hm/irrep)

(let [ir (hm/irrep [2 1])]
  (hm/rep-dimension ir))

(kind/test-last [= 2])

(kind/doc #'hm/rep-matrix)

(let [ir (hm/irrep [2 1])]
  (fm/nrow (hm/rep-matrix ir [1 0 2])))

(kind/test-last [= 2])

(kind/doc #'hm/rep-dimension)

(hm/rep-dimension (hm/irrep [3 1]))

(kind/test-last [= 3])

(kind/doc #'hm/rep-character)

(let [ir (hm/irrep [2 1])]
  (hm/rep-character ir [0 1 2]))

(kind/test-last [(fn [v] (< (Math/abs (- v 2.0)) 1e-10))])

(kind/doc #'hm/rep-generators)

(let [ir (hm/irrep [2 1])]
  (count (hm/rep-generators ir)))

(kind/test-last [= 2])

(kind/doc #'hm/tensor-product)

(let [ir1 (hm/irrep [2 1])
      ir2 (hm/irrep [2 1])
      tp (hm/tensor-product ir1 ir2)]
  (hm/rep-dimension tp))

(kind/test-last [= 4])

(kind/doc #'hm/direct-sum)

(let [ir1 (hm/irrep [2 1])
      ir2 (hm/irrep [1 1 1])
      ds (hm/direct-sum ir1 ir2)]
  (hm/rep-dimension ds))

(kind/test-last [= 3])

(kind/doc #'hm/frobenius-norm-sq)

(let [M (fm/rows->mat [[1.0 0.0] [0.0 1.0]])]
  (hm/frobenius-norm-sq M))

(kind/test-last [(fn [v] (< (Math/abs (- v 2.0)) 1e-10))])

(kind/doc #'hm/frobenius-norm)

(let [M (fm/rows->mat [[3.0 0.0] [0.0 4.0]])]
  (hm/frobenius-norm M))

(kind/test-last [(fn [v] (< (Math/abs (- v 5.0)) 1e-10))])

(kind/doc #'hm/matrix-fourier-transform)

(let [G (hm/symmetric-group 3)
      ir (hm/irrep [2 1])
      f (zipmap (hm/elements G) (repeat 1.0))
      fhat (hm/matrix-fourier-transform ir G f)]
  (fm/nrow fhat))

(kind/test-last [= 2])

;; ## Riffle Shuffles

(kind/doc #'hm/rising-sequences)

(hm/rising-sequences [0 1 2 3])

(kind/test-last [= 1])

(hm/rising-sequences [3 2 1 0])

(kind/test-last [= 4])

(kind/doc #'hm/gsr-probability)

(let [p (hm/gsr-probability [0 1 2 3] 1)]
  (> p 0.0))

(kind/test-last [true?])

;; ## Group Actions

(kind/doc #'hm/orbit)

(let [G (hm/cyclic-group 4)
      act (fn [g x] (mod (+ (long g) (long x)) 4))]
  (hm/orbit G act 0))

(kind/test-last [= #{0 1 2 3}])

(kind/doc #'hm/orbits)

(let [G (hm/cyclic-group 3)
      act (fn [g x] (mod (+ (long g) (long x)) 3))]
  (count (hm/orbits G act (range 3))))

(kind/test-last [= 1])

(kind/doc #'hm/fixed-points)

(let [act (fn [g x] (mod (+ (long g) (long x)) 5))]
  (hm/fixed-points act 0 (range 5)))

(kind/test-last [= #{0 1 2 3 4}])

(kind/doc #'hm/stabilizer)

(let [G (hm/cyclic-group 4)
      act (fn [g x] (mod (+ (long g) (long x)) 4))]
  (hm/stabilizer G act 0))

(kind/test-last [= #{0}])

(kind/doc #'hm/burnside-count)

(let [G (hm/cyclic-group 4)
      act (fn [g coloring] (mapv #(coloring (mod (+ % (long g)) 4)) (range 4)))
      domain [[0 0 0 0] [0 0 0 1] [0 0 1 0] [0 0 1 1]
              [0 1 0 0] [0 1 0 1] [0 1 1 0] [0 1 1 1]
              [1 0 0 0] [1 0 0 1] [1 0 1 0] [1 0 1 1]
              [1 1 0 0] [1 1 0 1] [1 1 1 0] [1 1 1 1]]]
  (hm/burnside-count G act domain))

(kind/test-last [= 6])

(kind/doc #'hm/cycle-index)

(let [G (hm/cyclic-group 3)
      act (fn [g x] (mod (+ (long g) (long x)) 3))
      ci (hm/cycle-index G act (range 3))]
  (= 1 (reduce + (vals ci))))

(kind/test-last [true?])

(kind/doc #'hm/polya-count)

(let [G (hm/cyclic-group 4)
      act (fn [g x] (mod (+ (long g) (long x)) 4))
      ci (hm/cycle-index G act (range 4))]
  (hm/polya-count ci 2))

(kind/test-last [= 6])

(kind/doc #'hm/subset-action)

(let [perm-act (fn [sigma x] (sigma x))
      {:keys [domain]} (hm/subset-action perm-act (range 4) 2)]
  (count domain))

(kind/test-last [= 6])

;; ## Fourier Analysis

(kind/doc #'hm/fourier-transform)

(let [ct (hm/character-table (hm/cyclic-group 4))
      f (mapv #(c/complex (double %) 0.0) [1 0 0 0])
      fhat (hm/fourier-transform ct f)]
  (count fhat))

(kind/test-last [= 4])

(kind/doc #'hm/inverse-fourier-transform)

(let [ct (hm/character-table (hm/cyclic-group 4))
      f (mapv #(c/complex (double %) 0.0) [1 2 3 4])
      fhat (hm/fourier-transform ct f)
      f-back (hm/inverse-fourier-transform ct fhat)
      max-err (apply max (map #(c/abs (c/sub %1 %2)) f f-back))]
  (< max-err 1e-10))

(kind/test-last [true?])

(kind/doc #'hm/convolve)

(let [ct (hm/character-table (hm/cyclic-group 4))
      f (mapv #(c/complex (double %) 0.0) [1 0 0 0])
      g (mapv #(c/complex (double %) 0.0) [0 1 0 0])
      conv (hm/convolve ct f g)]
  (long (Math/round (c/re (nth conv 1)))))

(kind/test-last [= 1])

(kind/doc #'hm/total-variation-distance)

(hm/total-variation-distance [0.5 0.5 0.0 0.0] [0.25 0.25 0.25 0.25])

(kind/test-last [(fn [v] (< (Math/abs (- v 0.5)) 1e-10))])


;; ## SVG Visualizations

(kind/doc #'hm/young-diagram-svg)

(kind/hiccup (hm/young-diagram-svg [4 2 1]))

(kind/doc #'hm/young-hooks-svg)

(kind/hiccup (hm/young-hooks-svg [4 2 1]))

(kind/doc #'hm/syt-svg)

(kind/hiccup (hm/syt-svg [[1 2 3 4] [5 6] [7]]))

(kind/doc #'hm/cycle-diagram-svg)

(kind/hiccup (hm/cycle-diagram-svg [1 2 3 0]))

(kind/doc #'hm/cayley-table-svg)

(kind/hiccup (hm/cayley-table-svg (hm/cyclic-group 4)))
