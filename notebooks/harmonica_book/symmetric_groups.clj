
;; # Symmetric Groups
;;
;; The previous chapters used cyclic and dihedral groups to analyze
;; melodies, necklaces, chords, and sound. Now we turn to a larger
;; family: the **[symmetric group](https://en.wikipedia.org/wiki/Symmetric_group)** $S_n$, the group of all [permutations](https://en.wikipedia.org/wiki/Permutation) of $n$ objects.
;; It has $n!$ elements — the most classical object in group theory.
;;
;; Where cyclic groups are [abelian](https://en.wikipedia.org/wiki/Abelian_group) and predictable, $S_n$ is wild: for $n \geq 3$
;; it is non-abelian, meaning the order in which you compose permutations
;; matters. This non-commutativity is precisely what makes $S_n$ rich enough to
;; model shuffling, symmetry breaking, and the combinatorics of partitions.
;;
;; For the general group concept and other group families, see
;; [Groups and Structure](groups_and_structure.html).

(ns harmonica-book.symmetric-groups
  (:require
   [scicloj.harmonica :as hm]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Permutations as vectors
;;
;; A permutation of $\{0, 1, \ldots, n{-}1\}$ is stored as a vector in **[one-line
;; notation](https://en.wikipedia.org/wiki/Permutation#One-line_notation)**: the entry at position $i$ is the image of $i$ under the
;; permutation. This representation makes composition natural — it's just
;; function composition on vectors.

(def G (hm/symmetric-group 4))

G

(hm/order G)

(kind/test-last
 [= 24])

;; The identity permutation maps every element to itself.

(hm/id G)

(kind/test-last
 [= [0 1 2 3]])

;; A transposition swaps two elements.

(hm/transposition 4 1 3)

(kind/test-last
 [= [0 3 2 1]])

;; Composition follows the standard mathematical convention (right-to-left):
;; $(\sigma\circ\tau)(i) = \sigma(\tau(i))$. Since permutations are vectors,
;; this is simply `(mapv sigma tau)`.

(hm/op G [1 0 2 3] [0 1 3 2])

(kind/test-last
 [= [1 0 3 2]])

;; Every permutation has an inverse.

(hm/inv G [1 2 3 0])

(kind/test-last
 [= [3 0 1 2]])

(hm/op G [1 2 3 0] (hm/inv G [1 2 3 0]))

(kind/test-last
 [= [0 1 2 3]])

;; ## Non-commutativity
;;
;; Unlike cyclic groups, $S_n$ is **not abelian** for $n \geq 3$. The order
;; of composition matters — this is the source of its richness.

(let [sigma [1 2 0 3]
      tau [0 1 3 2]]
  [(hm/op G sigma tau)
   (hm/op G tau sigma)])

(kind/test-last
 [(fn [v] (not= (first v) (second v)))])

;; ## Cycle notation
;;
;; [Cycle notation](https://en.wikipedia.org/wiki/Permutation#Cycle_notation) reveals the structure of a permutation more clearly
;; than one-line notation. The permutation `[1 2 3 0]` sends
;; $0 \to 1 \to 2 \to 3 \to 0$, which is the single 4-cycle $(0\;1\;2\;3)$.

(hm/cycles [1 2 3 0])

(kind/test-last
 [= [[0 1 2 3]]])

;; A transposition is a 2-cycle.

(hm/cycles [0 3 2 1])

(kind/test-last
 [= [[1 3]]])

;; The identity has no non-trivial cycles.

(hm/cycles [0 1 2 3])

(kind/test-last
 [= []])

;; A permutation with two disjoint cycles:

(hm/cycles [1 0 3 2])

(kind/test-last
 [= [[0 1] [2 3]]])

;; ### Cycle diagrams
;;
;; A cycle diagram draws elements around a circle with arrows showing
;; where each element maps. Here is the 4-cycle $(0\;1\;2\;3)$:

(kind/hiccup (hm/cycle-diagram-svg [1 2 3 0]))

;; Two disjoint 2-cycles $(0\;1)(2\;3)$:

(kind/hiccup (hm/cycle-diagram-svg [1 0 3 2]))

;; A permutation with a fixed point — $(0\;2\;4)(1\;3)$, where $5$ is fixed:

(kind/hiccup (hm/cycle-diagram-svg [2 3 4 1 0 5]))

;; ## Cycle type and the sign of a permutation
;;
;; The **[cycle type](https://en.wikipedia.org/wiki/Cycle_type)** records the lengths of all cycles (including
;; fixed points) as a partition — a descending sequence of positive
;; integers summing to $n$. Two permutations are conjugate in $S_n$
;; if and only if they share the same cycle type.

(hm/cycle-type [1 2 3 0])

(kind/test-last
 [= [4]])

(hm/cycle-type [1 0 3 2])

(kind/test-last
 [= [2 2]])

(hm/cycle-type [1 0 2 3])

(kind/test-last
 [= [2 1 1]])

;; The **[sign](https://en.wikipedia.org/wiki/Parity_of_a_permutation)** of a permutation is $+1$ (even) or $-1$ (odd), determined
;; by the parity of the number of transpositions needed to express it.
;; The sign is a group homomorphism: $\operatorname{sign}(\sigma\tau) =
;; \operatorname{sign}(\sigma) \cdot \operatorname{sign}(\tau)$.

(hm/sign [0 1 2 3])

(kind/test-last
 [= 1])

(hm/sign [1 0 2 3])

(kind/test-last
 [= -1])

;; Verify the homomorphism property:

(let [sigma [1 2 0 3]
      tau [0 1 3 2]]
  (* (hm/sign sigma) (hm/sign tau)))

(kind/test-last
 [= (hm/sign (hm/op G [1 2 0 3] [0 1 3 2]))])

;; ## Partitions
;;
;; A **[partition](https://en.wikipedia.org/wiki/Partition_(number_theory))** of $n$ is a way to write $n$ as a sum of positive integers
;; in descending order. Partitions play a double role in $S_n$: they index
;; both the **conjugacy classes** (via cycle type) and the **irreducible
;; representations** (via [Young diagrams](https://en.wikipedia.org/wiki/Young_diagram)). This duality is at the heart of
;; the representation theory of symmetric groups.

(hm/partitions 4)

(kind/test-last
 [= [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]])

;; The number of partitions grows quickly — but much slower than $n!$.

(kind/table
 {:column-names ["$n$" "$p(n)$"]
  :row-vectors (mapv (fn [n] [n (count (hm/partitions n))])
                     (range 1 11))})

;; ## Young diagrams
;;
;; A partition is visualized as a **[Young diagram](https://en.wikipedia.org/wiki/Young_diagram)**: a left-justified array
;; of boxes where row $i$ has $\lambda_i$ boxes.

;; The partitions of 5 and their Young diagrams:

(kind/hiccup
 (into [:div {:style "display: flex; flex-wrap: wrap; gap: 24px; align-items: flex-end;"}]
       (for [p (hm/partitions 5)]
         [:div {:style "text-align: center;"}
          (hm/young-diagram-svg p)
          [:div {:style "margin-top: 4px; font-family: monospace; font-size: 13px;"}
           (str p)]])))

;; ### Conjugate partition
;;
;; The **conjugate** $\lambda'$ of a partition $\lambda$ is obtained by
;; reflecting the Young diagram along the main diagonal — swapping rows
;; and columns.

(hm/partition-conjugate [4 2 1])

(kind/test-last [= [3 2 1 1]])

(hm/partition-conjugate [3 3])

(kind/test-last [= [2 2 2]])

;; A partition and its conjugate, side by side:

(kind/hiccup
 (let [p [4 2 1]
       pc (hm/partition-conjugate p)]
   [:div {:style "display: flex; gap: 40px; align-items: flex-end;"}
    [:div {:style "text-align: center;"}
     (hm/young-diagram-svg p)
     [:div {:style "margin-top: 4px; font-family: monospace;"} (str "λ = " p)]]
    [:div {:style "text-align: center; font-size: 24px; align-self: center;"} "↔"]
    [:div {:style "text-align: center;"}
     (hm/young-diagram-svg pc :fill "#e67e22")
     [:div {:style "margin-top: 4px; font-family: monospace;"} (str "λ' = " pc)]]]))

;; Conjugation is an involution: $(\lambda')' = \lambda$.

(= [4 2 1] (hm/partition-conjugate (hm/partition-conjugate [4 2 1])))

(kind/test-last [true?])

;; ## Hook lengths and the hook-length formula
;;
;; For each cell $(i, j)$ in a Young diagram, the **[hook length](https://en.wikipedia.org/wiki/Hook_length_formula)** $h(i,j)$
;; counts the cells directly to the right and directly below, plus the cell
;; itself.
;;
;; The **[hook-length formula](https://en.wikipedia.org/wiki/Hook_length_formula)** gives the number of standard Young tableaux
;; of shape $\lambda$:
;;
;; $$f^\lambda = \frac{n!}{\prod_{(i,j) \in \lambda} h(i,j)}$$
;;
;; This equals the dimension of the irreducible representation of $S_n$
;; indexed by $\lambda$.

;; Hook lengths for $[4, 2, 1]$:

(kind/hiccup (hm/young-hooks-svg [4 2 1]))

;; Hook lengths for $[3, 2, 2]$:

(kind/hiccup (hm/young-hooks-svg [3 2 2]))

;; Verify: the hook-length dimension matches explicit tableau enumeration.

(let [results
      (for [lambda (hm/partitions 5)]
        (= (hm/hook-length-dimension lambda)
           (count (hm/standard-young-tableaux lambda))))]
  (every? true? results))

(kind/test-last [true?])

;; Dimension table for partitions of 5:

(kind/table
 {:column-names ["$\\lambda$" "$f^\\lambda$ (hook-length)" "# SYT (enumerated)"]
  :row-vectors
  (mapv (fn [lambda]
          [(str lambda)
           (hm/hook-length-dimension lambda)
           (count (hm/standard-young-tableaux lambda))])
        (hm/partitions 5))})

;; ## Standard Young Tableaux
;;
;; A **[standard Young tableau](https://en.wikipedia.org/wiki/Young_tableau#Standard_Young_tableaux)** (SYT) of shape $\lambda$ is a filling of the
;; Young diagram with $1, 2, \ldots, n$ such that entries increase along each
;; row and down each column. SYTs form the basis for
;; [Young's orthogonal representation](https://en.wikipedia.org/wiki/Young%27s_orthogonal_representation).

(hm/standard-young-tableaux [2 1])

(kind/test-last [= [[[1 2] [3]] [[1 3] [2]]]])

(count (hm/standard-young-tableaux [3 2]))

(kind/test-last [= 5])

;; The 5 standard Young tableaux of shape $[3, 2]$:

(kind/hiccup
 (into [:div {:style "display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start;"}]
       (for [t (hm/standard-young-tableaux [3 2])]
         [:div {:style "text-align: center;"}
          (hm/syt-svg t)])))

;; ## Conjugacy classes
;;
;; Two permutations are **[conjugate](https://en.wikipedia.org/wiki/Conjugacy_class)** if one can be obtained from the
;; other by relabeling elements: $\sigma \sim \tau$ iff there exists $g$
;; with $g\sigma g^{-1} = \tau$. The conjugacy class of a permutation
;; is determined entirely by its cycle type.

(def classes (hm/conjugacy-classes G))

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
 [= (count (hm/partitions 4))])

;; ## The dimension-squared sum
;;
;; A beautiful identity: the sum of squared dimensions of all irreps
;; equals $|S_n| = n!$:
;;
;; $$\sum_{\lambda \vdash n} (f^\lambda)^2 = n!$$

(let [n 5
      dims (mapv hm/hook-length-dimension (hm/partitions n))]
  (reduce + (map #(* % %) dims)))

(kind/test-last [= 120])

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
                       (let [G (hm/symmetric-group n)]
                         [n (hm/order G)
                          (count (hm/conjugacy-classes G))]))
                     (range 1 9))})

;; For $n = 8$, the group has 40,320 elements but only 22 conjugacy classes
;; — and therefore only 22 irreducible representations. The character table
;; is a 22×22 matrix, but it captures the complete Fourier-analytic structure
;; of a group with over 40,000 elements.

(-> (tc/dataset
     {:n (range 1 9)
      :log-order (mapv (fn [n] (Math/log10 (double (hm/order (hm/symmetric-group n)))))
                       (range 1 9))
      :num-classes (mapv (fn [n] (count (hm/conjugacy-classes (hm/symmetric-group n))))
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
;; With permutations, partitions, and Young diagrams in hand:
;;
;; - **[Character Theory](character_theory.html)** — the character table
;;   encodes all irreducible representations. For $S_n$ it is computed by
;;   the Murnaghan-Nakayama rule.
;; - **[Representation Matrices](representation_matrices.html)** — Young's
;;   orthogonal form gives explicit matrices for each irrep.
;; - **[Random Transpositions](random_transpositions.html)** and
;;   **[Riffle Shuffles](riffle_shuffle.html)** — Fourier analysis on $S_n$
;;   applied to card shuffling.
