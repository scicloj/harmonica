;; # Symmetric Groups
;;
;; The **symmetric group** $S_n$ is the group of all permutations of $n$ objects.
;; It has $n!$ elements — the most classical object in group theory.
;;
;; The structure of $S_n$ is richer than cyclic groups: it has multiple
;; conjugacy classes (indexed by **partitions**), and its representation
;; theory governs problems from card shuffling to quantum chemistry.
;;
;; This notebook introduces $S_n$ using reel's permutation and partition
;; machinery.

(ns reel-book.symmetric-groups
  (:require
   [scicloj.reel.core :as reel]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Permutations as vectors

;; A permutation of $\{0, 1, \ldots, n{-}1\}$ is stored as a vector in **one-line
;; notation**: the entry at position $i$ is the image of $i$ under the
;; permutation.

(def G (reel/symmetric-group 4))

(reel/order G)

(kind/test-last
 [= 24])

;; The identity permutation maps every element to itself.

(reel/id G)

(kind/test-last
 [= [0 1 2 3]])

;; A transposition swaps two elements.

(reel/transposition 4 1 3)

(kind/test-last
 [= [0 3 2 1]])

;; Composition follows the standard mathematical convention (right-to-left):
;; $(\sigma\circ\tau)(i) = \sigma(\tau(i))$. Since permutations are vectors,
;; this is simply `(mapv sigma tau)`.

(reel/op G [1 0 2 3] [0 1 3 2])

(kind/test-last
 [= [1 0 3 2]])

;; Every permutation has an inverse.

(reel/inv G [1 2 3 0])

(kind/test-last
 [= [3 0 1 2]])

(reel/op G [1 2 3 0] (reel/inv G [1 2 3 0]))

(kind/test-last
 [= [0 1 2 3]])

;; ## Cycle notation

;; Cycle notation reveals the structure of a permutation more clearly
;; than one-line notation. The permutation `[1 2 3 0]` sends
;; $0 \to 1 \to 2 \to 3 \to 0$, which is the single 4-cycle $(0\;1\;2\;3)$.

(reel/cycles [1 2 3 0])

(kind/test-last
 [= [[0 1 2 3]]])

;; A transposition is a 2-cycle.

(reel/cycles [0 3 2 1])

(kind/test-last
 [= [[1 3]]])

;; The identity has no non-trivial cycles.

(reel/cycles [0 1 2 3])

(kind/test-last
 [= []])

;; A permutation with two disjoint cycles:

(reel/cycles [1 0 3 2])

(kind/test-last
 [= [[0 1] [2 3]]])

;; ## Cycle type and the sign of a permutation

;; The **cycle type** records the lengths of all cycles (including
;; fixed points) as a partition — a descending sequence of positive
;; integers summing to $n$.

(reel/cycle-type [1 2 3 0])

(kind/test-last
 [= [4]])

(reel/cycle-type [1 0 3 2])

(kind/test-last
 [= [2 2]])

(reel/cycle-type [1 0 2 3])

(kind/test-last
 [= [2 1 1]])

;; The **sign** of a permutation is $+1$ (even) or $-1$ (odd), determined
;; by the parity of the number of transpositions needed to express it.

(reel/sign [0 1 2 3])

(kind/test-last
 [= 1])

(reel/sign [1 0 2 3])

(kind/test-last
 [= -1])

;; The sign is multiplicative:
;; $\operatorname{sign}(\sigma\circ\tau) = \operatorname{sign}(\sigma)\cdot\operatorname{sign}(\tau)$.

(let [sigma [1 2 0 3]
      tau [0 1 3 2]]
  (* (reel/sign sigma) (reel/sign tau)))

(kind/test-last
 [= (reel/sign (reel/op G [1 2 0 3] [0 1 3 2]))])

;; ## Partitions

;; A **partition** of $n$ is a way to write $n$ as a sum of positive integers
;; in descending order. Partitions index the conjugacy classes of $S_n$
;; and also index the irreducible representations.

(reel/partitions 4)

(kind/test-last
 [= [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]])

;; The number of partitions grows quickly.

(kind/table
 {:column-names ["$n$" "$p(n)$"]
  :row-vectors (mapv (fn [n] [n (count (reel/partitions n))])
                     (range 1 11))})

;; ## Conjugacy classes

;; Two permutations are **conjugate** if one can be obtained from the
;; other by relabeling elements. The conjugacy class of a permutation
;; is determined entirely by its cycle type (partition).

(def classes (reel/conjugacy-classes G))

(kind/table
 {:column-names ["Cycle type" "Class size"]
  :row-vectors (mapv (fn [c] [(:cycle-type c) (:size c)]) classes)})

;; The class sizes sum to $|S_n|$.

(reduce + (map :size classes))

(kind/test-last
 [= 24])

;; The number of conjugacy classes equals the number of partitions.

(count classes)

(kind/test-last
 [= (count (reel/partitions 4))])

;; ## Growth of $S_n$

;; The symmetric group grows factorially — $S_5$ already has 120 elements.
;; But the number of conjugacy classes (= number of partitions) grows
;; much more slowly, which is what makes representation theory tractable.

(kind/table
 {:column-names ["$n$" "$|S_n|$" "# classes"]
  :row-vectors (mapv (fn [n]
                       (let [G (reel/symmetric-group n)]
                         [n (reel/order G)
                          (count (reel/conjugacy-classes G))]))
                     (range 1 9))})

;; ## What comes next
;;
;; With the symmetric group in hand, the next step is computing its
;; **character table** — a square matrix indexed by partitions, whose
;; entries are given by the Murnaghan-Nakayama rule. This will enable
;; Fourier analysis on $S_n$, with applications to card shuffling
;; (Diaconis's random transpositions analysis).
