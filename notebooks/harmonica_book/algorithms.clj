;; # Algorithms
;;
;; This chapter walks through the key algorithms behind harmonica,
;; with worked examples. Each section states the mathematical rule,
;; shows the implementation strategy, and runs a concrete computation
;; so you can see inputs and outputs.

(ns harmonica-book.algorithms
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
   [scicloj.harmonica.protocols :as p]
   [scicloj.harmonica.combinatorics.permutation :as perm]
   [scicloj.harmonica.combinatorics.partition :as part]
   [scicloj.harmonica.combinatorics.young-tableaux :as yt]
   [scicloj.harmonica.combinatorics.murnaghan-nakayama :as mn]
   [scicloj.harmonica.combinatorics.young-orthogonal :as yo]
   [scicloj.harmonica.combinatorics.riffle :as riffle]
   [scicloj.harmonica.analysis.representations :as rep]
   [tech.v3.datatype.functional :as dfn]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Murnaghan-Nakayama rule
;;
;; The character value $\chi_\lambda(\mu)$ — the trace of the irrep
;; matrix for partition $\lambda$ at conjugacy class $\mu$ — can be
;; computed without constructing any matrices. The
;; [Murnaghan-Nakayama rule](https://en.wikipedia.org/wiki/Murnaghan%E2%80%93Nakayama_rule)
;; is a recursive formula:
;;
;; $$\chi_\lambda(\mu) = \sum_\xi (-1)^{\text{ht}(\xi)} \; \chi_{\lambda \setminus \xi}(\mu')$$
;;
;; where the sum is over all **rim hooks** (border strips) $\xi$ of
;; length $\mu_1$ that can be removed from $\lambda$, $\text{ht}(\xi)$
;; is the height (number of rows minus 1), and $\mu'$ is $\mu$ with
;; $\mu_1$ removed.
;;
;; ### Partition sequence encoding
;;
;; The implementation encodes a Young diagram's boundary as a
;; **partition sequence** — a boolean array tracing the lower contour:
;; `true` for a horizontal step, `false` for a vertical step.
;;
;; For $\lambda = [3, 2]$:
;;
;; ```
;; □ □ □
;; □ □
;; ```
;;
;; Reading the boundary bottom-to-top, left-to-right:
;; 2 horizontal → 1 vertical → 1 horizontal → 1 vertical,
;; giving `[true true false true false]`.

(vec (mn/partition-seq [3 2]))

(kind/test-last [= [true true false true false]])

;; A partition sequence for $[4, 2, 1]$:

(vec (mn/partition-seq [4 2 1]))

(kind/test-last [= [true false true false true true false]])

;; ### Rim hook removal as bit-swapping
;;
;; A rim hook of length $r$ starting at position $i$ in the partition
;; sequence corresponds to `R[i] = true` and `R[i+r] = false`.
;; Removing the hook is a simple swap: set `R[i] = false`,
;; `R[i+r] = true`.
;;
;; The sign is $(-1)^{\text{ht}}$ where the height equals the number
;; of `false` entries (vertical steps) in `R[i..i+r-1]`.
;;
;; ### Worked example
;;
;; Compute $\chi_{[3,2]}([2,2,1])$ — the character of the irrep
;; labeled `[3,2]` at the class with cycle type `[2,2,1]`:

(mn/chi [3 2] [2 2 1])

(kind/test-last [= 1])

;; We can verify this against the character table:

(let [ct (hm/character-table (hm/symmetric-group 5))
      ;; Find the row for irrep [3,2] and column for class [2,2,1]
      row-idx (.indexOf ^clojure.lang.PersistentVector (:irrep-labels ct) [3 2])
      col-idx (.indexOf ^clojure.lang.PersistentVector (:classes ct) [2 2 1])]
  (cx/re (((:table ct) row-idx) col-idx)))

(kind/test-last [= 1.0])

;; The MN rule builds the entire character table of $S_n$ without
;; constructing any representation matrices. For $S_{20}$
;; (which has 627 conjugacy classes), computing a single character
;; value is nearly instant:

(mn/chi [10 5 3 2] [4 4 4 3 3 2])

(kind/test-last [(fn [v] (integer? v))])

;; ## Young's orthogonal form
;;
;; To build actual representation **matrices** (not just traces),
;; harmonica uses
;; [Young's orthogonal form](https://en.wikipedia.org/wiki/Young%27s_orthogonal_form).
;; The idea: define the irrep on **generators** (adjacent
;; transpositions $s_i = (i\;i{+}1)$), then extend to arbitrary
;; permutations by decomposition.
;;
;; ### The SYT basis
;;
;; The basis vectors of the irrep $\rho_\lambda$ are the standard
;; Young tableaux of shape $\lambda$. For $\lambda = [3, 1]$:

(yt/standard-young-tableaux [3 1])

(kind/test-last [(fn [syts] (= 3 (count syts)))])

;; ### Axial distance
;;
;; The key quantity is the **axial distance** between values $i$ and
;; $i{+}1$ in a tableau $T$:
;;
;; $$\rho(T, i, i{+}1) = c(\text{pos}(i{+}1)) - c(\text{pos}(i))$$
;;
;; where $c(r, c) = c - r$ is the **content** of cell $(r, c)$.

;; In the tableau `[[1 2 4] [3]]`, values 2 and 3 sit at
;; positions $(0,1)$ and $(1,0)$:

(yt/axial-distance [[1 2 4] [3]] 2 3)

(kind/test-last [= -2])

;; ### Generator matrices
;;
;; For adjacent transposition $s_i$, the matrix in the SYT basis is:
;;
;; - If $i$ and $i{+}1$ are in the **same row** ($\rho = 1$):
;;   diagonal entry $1/\rho = 1$
;; - If $i$ and $i{+}1$ are in the **same column** ($\rho = -1$):
;;   diagonal entry $1/\rho = -1$
;; - Otherwise: a $2 \times 2$ block pairing tableau $T$ with
;;   the tableau $T'$ obtained by swapping $i$ and $i{+}1$:
;;
;; $$\begin{pmatrix} 1/\rho & \sqrt{1 - 1/\rho^2} \\ \sqrt{1 - 1/\rho^2} & -1/\rho \end{pmatrix}$$
;;
;; Here is the generator $\rho(s_1)$ for the irrep $[3, 1]$
;; — swapping values 1 and 2:

(let [ir (hm/irrep [3 1])]
  (first (:generators ir)))

;; ### From generators to arbitrary permutations
;;
;; Any permutation can be decomposed into adjacent transpositions.
;; Harmonica uses **bubble sort**: sort the permutation array,
;; recording each swap. The recorded indices (reversed) give the
;; decomposition.

(perm/adjacent-transposition-decomposition [2 0 1 3])

;; Verify: composing the transpositions (left to right) reconstructs
;; the original permutation.

(let [decomp (perm/adjacent-transposition-decomposition [2 0 1 3])]
  (reduce (fn [acc i]
            (perm/compose acc (perm/from-cycles 4 [[i (inc i)]])))
          (perm/identity-perm 4)
          decomp))

(kind/test-last [= [2 0 1 3]])

;; The representation matrix $\rho(\sigma)$ is then the product
;; of the corresponding generator matrices:

(let [ir (hm/irrep [3 1])
      sigma [2 0 1 3]
      M (hm/rep-matrix ir sigma)]
  {:trace (fm/trace M)
   :is-orthogonal (< (fm/trace
                       (fm/sub (fm/mulm M (fm/transpose M))
                               (fm/eye 3 true)))
                     1e-10)})

(kind/test-last [(fn [m] (and (number? (:trace m))
                              (:is-orthogonal m)))])

;; Why bubble sort? It's $O(n^2)$ in swaps, which is fine for
;; $S_n$ with $n \le 10$ or so. The matrices themselves are the
;; expensive part — each multiply costs $O(d^3)$ where $d$ is
;; the irrep dimension.

;; ## Conjugacy classes of $S_n$
;;
;; Two permutations in $S_n$ are conjugate if and only if they have
;; the same **cycle type**. So the conjugacy classes of $S_n$ are
;; in bijection with the partitions of $n$.
;;
;; The implementation uses two strategies:
;;
;; - **$n \le 8$**: Enumerate all $n!$ permutations, group by cycle
;;   type, store the full element sets.
;; - **$n > 8$**: Don't enumerate (too expensive). Instead, for each
;;   partition, build a **canonical representative** by laying out
;;   consecutive cycles: partition `[3 2 1]` on $n = 6$ gives the
;;   permutation with cycles $(0\;1\;2)(3\;4)(5)$. The class size
;;   is computed by the formula:
;;
;; $$|C_\mu| = \frac{n!}{\prod_k k^{a_k} \cdot a_k!}$$
;;
;; where $a_k$ is the number of parts equal to $k$.

;; Small group — full element sets available:

(let [cls (first (hm/conjugacy-classes (hm/symmetric-group 4)))]
  {:cycle-type (:cycle-type cls)
   :size (:size cls)
   :has-elements (some? (:elements cls))})

(kind/test-last [(fn [m] (and (= [4] (:cycle-type m))
                              (= 6 (:size m))
                              (:has-elements m)))])

;; Large group — no element enumeration:

(let [cls (first (hm/conjugacy-classes (hm/symmetric-group 12)))]
  {:cycle-type (:cycle-type cls)
   :size (:size cls)
   :has-elements (some? (:elements cls))})

(kind/test-last [(fn [m] (and (= [12] (:cycle-type m))
                              (not (:has-elements m))))])

;; The class size formula at work:

(part/partition-class-size 6 [3 2 1])

(kind/test-last [= 120])

;; Verify: $6! / (3^1 \cdot 1! \cdot 2^1 \cdot 1! \cdot 1^1 \cdot 1!) = 720 / 6 = 120$. Correct.

;; ## Burnside's lemma and cycle index
;;
;; [Burnside's lemma](https://en.wikipedia.org/wiki/Burnside%27s_lemma)
;; counts orbits by averaging fixed-point counts:
;;
;; $$|\text{orbits}| = \frac{1}{|G|} \sum_{g \in G} |\text{Fix}(g)|$$
;;
;; The implementation is direct — loop over group elements, count
;; fixed points of each, divide by $|G|$.
;;
;; The **cycle index** generalizes this. For each $g$, compute the
;; cycle type of $g$'s action on the domain, then accumulate
;; frequencies. [Pólya's theorem](https://en.wikipedia.org/wiki/Pólya_enumeration_theorem)
;; evaluates the cycle index by substituting $p_i \to k$ (number of
;; colors).

;; Necklaces with 6 beads and 3 colors. The cycle index of the
;; rotation action captures the cycle structure of each group element:

(let [G (hm/cyclic-group 6)
      act (fn [g x] (mod (+ g x) 6))]
  (hm/cycle-index G act (range 6)))

(kind/test-last [(fn [ci] (= 4 (count ci)))])

;; Pólya's theorem evaluates the cycle index to count distinct
;; necklaces — substitute $p_i \to k$ (number of colors):

(let [G (hm/cyclic-group 6)
      act (fn [g x] (mod (+ g x) 6))
      ci (hm/cycle-index G act (range 6))]
  (hm/polya-count ci 3))

(kind/test-last [= 130])

;; Burnside's lemma can compute the same answer directly, but it
;; needs the action on the full set of $3^6 = 729$ colorings.
;; `coloring-action` lifts the point action to an action on coloring
;; vectors:

(let [G (hm/cyclic-group 6)
      act (fn [g x] (mod (+ g x) 6))
      {:keys [act domain]} (hm/coloring-action act 6 3)]
  (hm/burnside-count G act domain))

(kind/test-last [= 130])

;; Pólya enumeration gives the same result without enumerating
;; all 729 colorings — the cycle index distills the group's structure
;; into a compact polynomial.

;; ## GSR riffle shuffle
;;
;; The [Gilbert-Shannon-Reeds model](https://en.wikipedia.org/wiki/Gilbert%E2%80%93Shannon%E2%80%93Reeds_model)
;; assigns a probability to each permutation after $k$ riffle shuffles
;; of an $n$-card deck:
;;
;; $$Q_a(\sigma) = \frac{\binom{a + n - r(\sigma)}{n}}{a^n}$$
;;
;; where $a = 2^k$ and $r(\sigma)$ is the number of **rising sequences**
;; of $\sigma$.
;;
;; ### Rising sequences
;;
;; $r(\sigma) = 1 + \text{descents}(\sigma^{-1})$ — compute the
;; inverse, then count positions where the value decreases:

(let [sigma [1 3 0 2]]
  {:inverse (perm/inverse sigma)
   :descents (riffle/descents (perm/inverse sigma))
   :rising-seqs (riffle/rising-sequences sigma)})

(kind/test-last [(fn [m] (= 3 (:rising-seqs m)))])

;; ### The probability formula
;;
;; After $k = 1$ shuffle of $n = 4$ cards, the identity has the
;; highest probability (it has $r = 1$, meaning $a + n - 1 = 5$ choose
;; $4 = 5$, divided by $2^4 = 16$):

(riffle/gsr-probability [0 1 2 3] 1)

(kind/test-last [(fn [p] (< (Math/abs (- p (/ 5.0 16))) 1e-10))])

;; ### Connection to Fourier analysis
;;
;; The power of this formula is that $Q_a$ is a **class function** —
;; it depends only on the number of rising sequences, which is
;; determined by the cycle type of $\sigma^{-1}$. This means we can
;; analyze the convergence to uniformity using the (scalar) Fourier
;; transform on $S_n$ via characters, and the (matrix) Fourier
;; transform via irreps gives the **upper bound lemma** used in
;; the "seven shuffles" proof.
;;
;; Total variation distance from uniform after $k$ shuffles of 52 cards:

(let [G (hm/symmetric-group 4)
      elts (vec (hm/elements G))
      uniform (/ 1.0 (hm/order G))]
  (mapv (fn [k]
          (let [probs (mapv #(riffle/gsr-probability % k) elts)]
            (double (* 0.5 (dfn/sum (dfn/abs (dfn/- probs uniform)))))))
        (range 1 8)))

(kind/test-last [(fn [tvs] (and (> (first tvs) 0.4)
                                (< (last tvs) 0.01)))])

;; The TV distance drops sharply — this is the **cutoff phenomenon**.

;; ## Restriction, induction, and branching
;;
;; ### Standard embedding $S_k \hookrightarrow S_n$
;;
;; The subgroup $S_k$ sits inside $S_n$ by fixing points $\ge k$.
;; To **restrict** a representation $\rho$ of $S_n$ to $S_k$,
;; simply compose with the embedding:

(let [ir (hm/irrep [3 1])
      restricted (hm/restrict-rep ir 4 3)
      ;; Apply to an element of S_3
      M (hm/rep-matrix restricted [1 2 0])]
  (fm/trace M))

(kind/test-last [(fn [t] (number? t))])

;; ### Induction
;;
;; **Induction** goes the other direction: given a representation
;; $\rho$ of $S_k$, build a representation of $S_n$ with dimension
;; $[S_n : S_k] \cdot d = (n!/k!) \cdot d$.
;;
;; The algorithm:
;;
;; 1. Compute left coset representatives $t_1, \ldots, t_m$ of $S_k$ in $S_n$
;; 2. For each $g \in S_n$, build an $m \times m$ block matrix where
;;    block $(s, t)$ is $\rho(s^{-1} g t)$ if $s^{-1} g t \in S_k$,
;;    and zero otherwise

(let [ir-triv {:dimension 1
               :matrix-fn (fn [_] (fm/eye 1 true))}
      induced (hm/induce-rep ir-triv 2 4)]
  {:dimension (:dimension induced)
   :expected (/ 24 2)})

(kind/test-last [(fn [m] (= (:dimension m) (:expected m)))])

;; ### Branching rule
;;
;; The [branching rule](https://en.wikipedia.org/wiki/Branching_rule)
;; for $S_n \downarrow S_{n-1}$ says: the restricted irrep $V_\lambda$
;; decomposes into the direct sum of $V_\mu$ where $\mu$ is obtained
;; by removing one box from $\lambda$.
;;
;; Harmonica verifies this computationally: restrict, then decompose
;; via character inner products.

(hm/branching-rule [3 1])

(kind/test-last [(fn [m] (= m {[3] 1, [2 1] 1}))])

;; Removing one box from `[3,1]` can give `[3]` (remove from row 2)
;; or `[2,1]` (remove from row 1) — each with multiplicity 1.
;; The branching rule confirms this.

;; ## What comes next
;;
;; For the mathematical theory behind these algorithms, see the main
;; book chapters — [Character Theory](character_theory.html) for
;; Murnaghan-Nakayama, [Representation Matrices](representation_matrices.html)
;; for Young's orthogonal form, and [Riffle Shuffle](riffle_shuffle.html)
;; for the GSR model. The [API Reference](api_reference.html) lists
;; every public function with examples.
