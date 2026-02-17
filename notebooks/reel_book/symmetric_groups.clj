;; # Symmetric Groups
;;
;; The **symmetric group** $S_n$ is the group of all permutations of $n$ objects.
;; It has $n!$ elements — the most classical object in group theory.
;;
;; Where cyclic groups are abelian and predictable, $S_n$ is wild: for $n \geq 3$
;; it is non-abelian, meaning the order in which you compose permutations
;; matters. This non-commutativity is precisely what makes $S_n$ rich enough to
;; model shuffling, symmetry breaking, and the combinatorics of partitions.
;;
;; This notebook introduces $S_n$ through reel's permutation and partition
;; machinery. For deeper exploration of permutation algebra, see
;; [Permutations and Partitions](permutations_and_partitions.html). For
;; character theory and representations, see
;; [Character Theory](character_theory.html) and
;; [Representation Matrices](representation_matrices.html).

(ns reel-book.symmetric-groups
  (:require
   [scicloj.reel.core :as reel]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Permutations as vectors
;;
;; A permutation of $\{0, 1, \ldots, n{-}1\}$ is stored as a vector in **one-line
;; notation**: the entry at position $i$ is the image of $i$ under the
;; permutation. This representation makes composition natural — it's just
;; function composition on vectors.

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

;; ## Non-commutativity
;;
;; Unlike cyclic groups, $S_n$ is **not abelian** for $n \geq 3$. The order
;; of composition matters — this is the source of its richness.

(let [sigma [1 2 0 3]
      tau [0 1 3 2]]
  [(reel/op G sigma tau)
   (reel/op G tau sigma)])

(kind/test-last
 [(fn [v] (not= (first v) (second v)))])

;; ## Cycle notation
;;
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
;;
;; The **cycle type** records the lengths of all cycles (including
;; fixed points) as a partition — a descending sequence of positive
;; integers summing to $n$. Two permutations are conjugate in $S_n$
;; if and only if they share the same cycle type.

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
;; The sign is a group homomorphism: $\operatorname{sign}(\sigma\tau) =
;; \operatorname{sign}(\sigma) \cdot \operatorname{sign}(\tau)$.

(reel/sign [0 1 2 3])

(kind/test-last
 [= 1])

(reel/sign [1 0 2 3])

(kind/test-last
 [= -1])

;; Verify the homomorphism property:

(let [sigma [1 2 0 3]
      tau [0 1 3 2]]
  (* (reel/sign sigma) (reel/sign tau)))

(kind/test-last
 [= (reel/sign (reel/op G [1 2 0 3] [0 1 3 2]))])

;; ## Partitions
;;
;; A **partition** of $n$ is a way to write $n$ as a sum of positive integers
;; in descending order. Partitions play a double role in $S_n$: they index
;; both the **conjugacy classes** (via cycle type) and the **irreducible
;; representations** (via Young diagrams). This duality is at the heart of
;; the representation theory of symmetric groups.

(reel/partitions 4)

(kind/test-last
 [= [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]])

;; The number of partitions grows quickly — but much slower than $n!$.

(kind/table
 {:column-names ["$n$" "$p(n)$"]
  :row-vectors (mapv (fn [n] [n (count (reel/partitions n))])
                     (range 1 11))})

;; ## Conjugacy classes
;;
;; Two permutations are **conjugate** if one can be obtained from the
;; other by relabeling elements: $\sigma \sim \tau$ iff there exists $g$
;; with $g\sigma g^{-1} = \tau$. The conjugacy class of a permutation
;; is determined entirely by its cycle type.

(def classes (reel/conjugacy-classes G))

(kind/table
 {:column-names ["Cycle type" "Class size"]
  :row-vectors (mapv (fn [c] [(:cycle-type c) (:size c)]) classes)})

;; The class sizes sum to $|S_n|$.

(reduce + (map :size classes))

(kind/test-last
 [= 24])

;; The number of conjugacy classes equals the number of partitions —
;; this is the key identity that makes the character table square.

(count classes)

(kind/test-last
 [= (count (reel/partitions 4))])

;; ## Growth of $S_n$
;;
;; The symmetric group grows factorially — $S_5$ already has 120 elements.
;; But the number of conjugacy classes (= number of partitions = number of
;; irreps) grows much more slowly, which is what makes Fourier analysis on
;; $S_n$ tractable: instead of working with $n!$ elements, we work with
;; $p(n)$ representations.

(kind/table
 {:column-names ["$n$" "$|S_n|$" "# classes"]
  :row-vectors (mapv (fn [n]
                       (let [G (reel/symmetric-group n)]
                         [n (reel/order G)
                          (count (reel/conjugacy-classes G))]))
                     (range 1 9))})

;; For $n = 8$, the group has 40,320 elements but only 22 conjugacy classes
;; — and therefore only 22 irreducible representations.

(-> (tc/dataset
     {:n (range 1 9)
      :log-order (mapv (fn [n] (Math/log10 (double (reel/order (reel/symmetric-group n)))))
                       (range 1 9))
      :num-classes (mapv (fn [n] (count (reel/conjugacy-classes (reel/symmetric-group n))))
                         (range 1 9))})
    (plotly/base {:=x :n :=y :log-order
                  :=x-title "n"
                  :=y-title "log₁₀(|Sₙ|)"
                  :=title "Factorial growth of Sₙ"})
    (plotly/layer-line)
    (plotly/layer-point {:=mark-size 8})
    plotly/plot)

;; ## What comes next
;;
;; With the symmetric group in hand, the next steps are:
;;
;; - **Character tables** — computed by the Murnaghan-Nakayama rule, these
;;   encode all irreducible representations as a square matrix.
;;   See [Character Theory](character_theory.html).
;;
;; - **Fourier analysis on $S_n$** — class functions can be transformed
;;   using just the character table; general functions need matrix
;;   representations. See [Random Transpositions](random_transpositions.html)
;;   and [Riffle Shuffles](riffle_shuffle.html).
;;
;; - **Representation matrices** — Young's orthogonal form gives explicit
;;   matrices for each irrep. See [Representation Matrices](representation_matrices.html).

