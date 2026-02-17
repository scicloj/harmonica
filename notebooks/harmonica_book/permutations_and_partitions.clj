;; # Permutations and Partitions
;;
;; This notebook is a deep dive into the algebraic properties of
;; **permutations** and **partitions** — the two foundational data types
;; in the representation theory of symmetric groups. We verify identities
;; systematically and visualize partitions as Young diagrams.
;;
;; For a narrative introduction to permutations, cycles, and the symmetric
;; group, see [Symmetric Groups](symmetric_groups.html).

(ns harmonica-book.permutations-and-partitions
  (:require
   [scicloj.harmonica.core :as hm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Permutation Identities
;;
;; The following verifications cover the fundamental algebraic properties
;; of permutations in $S_n$. Each identity is tested exhaustively on
;; small symmetric groups.

;; ### Sign is a homomorphism
;;
;; $\operatorname{sign}(\sigma \tau) =
;; \operatorname{sign}(\sigma) \cdot \operatorname{sign}(\tau)$.
;; We verify for all pairs in $S_5$.

(let [G (hm/symmetric-group 5)
      elts (vec (hm/elements G))]
  (every? (fn [[s t]]
            (= (hm/sign (hm/op G s t))
               (* (hm/sign s) (hm/sign t))))
          (for [a elts b elts] [a b])))

(kind/test-last [true?])

;; ### Inverse round-trip
;;
;; $\sigma \circ \sigma^{-1} = \sigma^{-1} \circ \sigma = e$ for all
;; permutations in $S_n$.

(let [results
      (for [n (range 1 7)]
        (let [G (hm/symmetric-group n)
              e (hm/id G)]
          (every? (fn [sigma]
                    (let [si (hm/inv G sigma)]
                      (and (= (hm/op G sigma si) e)
                           (= (hm/op G si sigma) e))))
                  (hm/elements G))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Adjacent transposition decomposition
;;
;; Every permutation can be written as a product of adjacent transpositions
;; $s_i = (i, i{+}1)$. We verify the decomposition reconstructs the
;; original for all of $S_6$.

(let [results
      (for [n (range 1 7)]
        (let [G (hm/symmetric-group n)
              id-perm (hm/id G)
              make-swap (fn [i]
                          (let [v (vec (range n))]
                            (assoc v i (inc i) (inc i) i)))]
          (every? (fn [sigma]
                    (let [swaps (hm/adjacent-transposition-decomposition sigma)
                          reconstructed (reduce (fn [p i] (hm/op G p (make-swap i)))
                                                id-perm swaps)]
                      (= sigma reconstructed)))
                  (hm/elements G))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Cycle type determines conjugacy class
;;
;; Two permutations are conjugate in $S_n$ if and only if they have the
;; same cycle type. We verify this for $S_3$ through $S_6$.

(let [results
      (for [n (range 3 7)]
        (let [G (hm/symmetric-group n)
              classes (hm/conjugacy-classes G)]
          (every? (fn [cls]
                    (= 1 (count (set (map hm/cycle-type (:elements cls))))))
                  classes)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Composition is associative
;;
;; $(\sigma \circ \tau) \circ \rho = \sigma \circ (\tau \circ \rho)$,
;; verified exhaustively for $S_4$.

(let [G (hm/symmetric-group 4)
      elts (vec (hm/elements G))]
  (every? (fn [[a b c]]
            (= (hm/op G (hm/op G a b) c)
               (hm/op G a (hm/op G b c))))
          (for [a elts b elts c elts] [a b c])))

(kind/test-last [true?])

;; ### Order of a permutation
;;
;; The **order** of $\sigma$ is the smallest positive $k$ such that
;; $\sigma^k = e$. It equals the LCM of the cycle lengths.

(defn perm-order
  "Compute the order of a permutation by repeated composition."
  [G sigma]
  (let [e (hm/id G)]
    (loop [k 1
           current sigma]
      (if (= current e)
        k
        (recur (inc k) (hm/op G current sigma))))))

;; Verify: order = lcm of cycle lengths, for all of $S_5$.

(let [G (hm/symmetric-group 5)]
  (every? (fn [sigma]
            (let [ct (hm/cycle-type sigma)
                  expected (reduce (fn [a b] (/ (* a b) (biginteger (.gcd (biginteger a) (biginteger b)))))
                                   (map biginteger ct))]
              (= (perm-order G sigma) (long expected))))
          (hm/elements G)))

(kind/test-last [true?])

;; ## Partitions
;;
;; A **partition** of $n$ is a way to write $n$ as a sum of positive integers
;; in descending order. Partitions are central to the representation theory
;; of $S_n$: they index both conjugacy classes and irreducible representations.

;; ### Enumeration

(hm/partitions 1)

(kind/test-last [= [[1]]])

(hm/partitions 4)

(kind/test-last [= [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]])

(hm/partitions 5)

(kind/test-last [= [[5] [4 1] [3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]]])

;; The number of partitions $p(n)$ grows sub-exponentially.

(kind/table
 {:column-names ["$n$" "$p(n)$"]
  :row-vectors (mapv (fn [n] [n (count (hm/partitions n))]) (range 1 16))})

;; All parts are positive and sum to $n$.

(let [results
      (for [n (range 1 13)]
        (every? (fn [p]
                  (and (every? pos-int? p)
                       (apply >= p)
                       (= n (reduce + p))))
                (hm/partitions n)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Young diagrams as SVG
;;
;; A partition is visualized as a **Young diagram**: a left-justified array
;; of boxes where row $i$ has $\lambda_i$ boxes.

;; The library provides `hm/young-diagram-svg` to render these:

;; The partitions of 5 and their Young diagrams:

(kind/hiccup
 (into [:div {:style "display: flex; flex-wrap: wrap; gap: 24px; align-items: flex-end;"}]
       (for [p (hm/partitions 5)]
         [:div {:style "text-align: center;"}
          (hm/young-diagram-svg p)
          [:div {:style "margin-top: 4px; font-family: monospace; font-size: 13px;"}
           (str p)]])))

;; ### Conjugate (transpose) partition
;;
;; The **conjugate** $\lambda'$ of a partition $\lambda$ is obtained by
;; reflecting the Young diagram along the main diagonal — swapping rows
;; and columns.

(hm/partition-conjugate [4 2 1])

(kind/test-last [= [3 2 1 1]])

(hm/partition-conjugate [3 3])

(kind/test-last [= [2 2 2]])

(hm/partition-conjugate [5])

(kind/test-last [= [1 1 1 1 1]])

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

;; Conjugate is an involution: $(\lambda')' = \lambda$.

(let [results
      (for [n (range 1 11)
            p (hm/partitions n)]
        (= p (hm/partition-conjugate (hm/partition-conjugate p))))]
  (every? true? results))

(kind/test-last [true?])

;; Conjugate preserves the sum: $|\lambda'| = |\lambda|$.

(let [results
      (for [n (range 1 11)
            p (hm/partitions n)]
        (= (reduce + p) (reduce + (hm/partition-conjugate p))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Hook lengths and the hook-length formula
;;
;; For each cell $(i, j)$ in a Young diagram, the **hook length** $h(i,j)$
;; counts the cells directly to the right and directly below, plus the cell
;; itself.
;;
;; The **hook-length formula** gives the number of standard Young tableaux
;; (SYT) of shape $\lambda$:
;;
;; $$f^\lambda = \frac{n!}{\prod_{(i,j) \in \lambda} h(i,j)}$$
;;
;; This equals the dimension of the irreducible representation of $S_n$
;; indexed by $\lambda$.

(defn hook-lengths
  "Compute all hook lengths for a partition."
  [lambda]
  (let [conj (hm/partition-conjugate lambda)]
    (for [i (range (count lambda))
          j (range (nth lambda i))]
      (+ (- (nth lambda i) j)
         (- (nth conj j) i)
         -1))))

;; Visualize hook lengths on a Young diagram:

;; The library provides `hm/young-hooks-svg`:

;; Hook lengths for $[4, 2, 1]$:

(kind/hiccup (hm/young-hooks-svg [4 2 1]))

;; Hook lengths for $[3, 2, 2]$:

(kind/hiccup (hm/young-hooks-svg [3 2 2]))

;; ### Hook-length formula verification
;;
;; The product of hook lengths divides $n!$ to give the number of SYT,
;; which equals the count from explicit enumeration.

(defn factorial [n] (reduce *' (range 1 (inc n))))

(let [results
      (for [n (range 1 8)
            lambda (hm/partitions n)]
        (let [hooks (hook-lengths lambda)
              hook-product (reduce *' hooks)
              formula-dim (/ (factorial n) hook-product)
              enum-dim (count (hm/standard-young-tableaux lambda))
              lib-dim (hm/hook-length-dimension lambda)]
          (and (= formula-dim enum-dim)
               (= formula-dim lib-dim))))]
  (every? true? results))

(kind/test-last [true?])

;; Dimension table for partitions of 5:

(kind/table
 {:column-names ["Partition λ" "Hook product" "f^λ = 5!/hooks" "# SYT (enumerated)"]
  :row-vectors
  (mapv (fn [lambda]
          (let [hooks (hook-lengths lambda)
                hp (reduce *' hooks)
                dim (/ (factorial 5) hp)
                enum (count (hm/standard-young-tableaux lambda))]
            [(str lambda) (str hp) (str dim) (str enum)]))
        (hm/partitions 5))})

;; ### Dimension-squared sum
;;
;; The sum of squared dimensions of all irreps equals $|S_n| = n!$:
;;
;; $$\sum_{\lambda \vdash n} (f^\lambda)^2 = n!$$

(let [results
      (for [n (range 1 9)]
        (let [dims (map hm/hook-length-dimension (hm/partitions n))
              sum-sq (reduce + (map #(* % %) dims))]
          (= sum-sq (factorial n))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Standard Young Tableaux
;;
;; A **standard Young tableau** (SYT) of shape $\lambda$ is a filling of the
;; Young diagram with $1, 2, \ldots, n$ such that entries increase along each
;; row and down each column. SYTs form the basis for Young's orthogonal
;; representation.

(hm/standard-young-tableaux [2 1])

(kind/test-last [= [[[1 2] [3]] [[1 3] [2]]]])

(count (hm/standard-young-tableaux [3 2]))

(kind/test-last [= 5])

;; Visualize all SYTs of shape $[3, 2]$:

;; The library provides `hm/syt-svg`:

(kind/hiccup
 (into [:div {:style "display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start;"}]
       (for [t (hm/standard-young-tableaux [3 2])]
         [:div {:style "text-align: center;"}
          (hm/syt-svg t)])))

;; ### Conjugacy class sizes
;;
;; The size of the conjugacy class in $S_n$ with cycle type $\mu$ is
;;
;; $$|C_\mu| = \frac{n!}{\prod_k k^{a_k} \cdot a_k!}$$
;;
;; where $a_k$ is the number of parts equal to $k$.

(defn class-size-formula
  "Compute conjugacy class size from the partition formula."
  [n mu]
  (let [freq (frequencies mu)]
    (/ (factorial n)
       (reduce *' (map (fn [[k ak]]
                         (*' (reduce *' (repeat ak k))
                             (factorial ak)))
                       freq)))))

;; Verify: the formula matches the actual class sizes from enumeration,
;; and all sizes sum to $n!$.

(let [results
      (for [n (range 2 8)]
        (let [G (hm/symmetric-group n)
              classes (hm/conjugacy-classes G)]
          (and
           ;; Sizes match formula
           (every? (fn [cls]
                     (let [ct (hm/cycle-type (:representative cls))]
                       (= (:size cls) (class-size-formula n ct))))
                   classes)
           ;; Sizes sum to n!
           (= (reduce + (map :size classes)) (factorial n)))))]
  (every? true? results))

(kind/test-last [true?])

;; Table of conjugacy classes for $S_5$:

(let [G (hm/symmetric-group 5)
      classes (hm/conjugacy-classes G)]
  (kind/table
   {:column-names ["Cycle type" "Class size" "Formula"]
    :row-vectors (mapv (fn [cls]
                         (let [ct (hm/cycle-type (:representative cls))]
                           [(str ct)
                            (:size cls)
                            (class-size-formula 5 ct)]))
                       classes)}))

;; ## Permutation cycle diagram
;;
;; A cycle diagram draws elements around a circle with arrows showing
;; where each element maps.

;; The library provides `hm/cycle-diagram-svg`:

;; The 4-cycle $(0\;1\;2\;3)$:

(kind/hiccup (hm/cycle-diagram-svg [1 2 3 0]))

;; Two disjoint 2-cycles $(0\;1)(2\;3)$:

(kind/hiccup (hm/cycle-diagram-svg [1 0 3 2]))

;; A permutation with a fixed point: $(0\;2\;4)(1\;3)$, 5 is fixed.

(kind/hiccup (hm/cycle-diagram-svg [2 3 4 1 0 5]))

;; ## Summary of verified identities
;;
;; This notebook verified:
;;
;; - Sign is a group homomorphism ($S_5$, all $120^2$ pairs)
;; - Inverse round-trip for $S_1$ through $S_6$ (all elements)
;; - Adjacent transposition decomposition reconstructs original ($S_1$ through $S_6$)
;; - Cycle type determines conjugacy class ($S_3$ through $S_6$)
;; - Associativity exhaustively verified for $S_4$ (all $24^3$ triples)
;; - Permutation order equals LCM of cycle lengths ($S_5$, all elements)
;; - Partitions are valid (positive, descending, correct sum) for $n = 1, \ldots, 12$
;; - Conjugate is an involution for $n = 1, \ldots, 10$
;; - Conjugate preserves the sum for $n = 1, \ldots, 10$
;; - Hook-length formula matches SYT enumeration for $n = 1, \ldots, 7$
;; - Dimension-squared sum equals $n!$ for $n = 1, \ldots, 8$
;; - Conjugacy class size formula matches enumeration for $S_2$ through $S_7$

