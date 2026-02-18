;; # Riffle Shuffles and the "Seven Shuffles" Theorem
;;
;; How many times should you [riffle-shuffle](https://en.wikipedia.org/wiki/Riffle_shuffle_permutation) a deck of cards?
;;
;; In 1992, **Bayer and Diaconis** proved a remarkable result: for a standard
;; 52-card deck, the distribution after $k$ riffle shuffles stays far from
;; uniform until $k \approx 7$, then rapidly converges. This sharp transition
;; is the **cutoff phenomenon**.
;;
;; Unlike random transpositions (which are a class function), the riffle
;; shuffle distribution is **not** a class function — it depends on the
;; specific permutation, not just its cycle type. Analyzing it requires
;; the full machinery of **matrix representations** and the
;; **[matrix-valued Fourier transform](https://en.wikipedia.org/wiki/Fourier_analysis_on_finite_groups)**.
;;
;; In this notebook we:
;;
;; - Build irreducible representations of $S_n$ via Young's orthogonal form
;; - Compute the Gilbert-Shannon-Reeds (GSR) riffle shuffle distribution
;; - Use the matrix Fourier transform to analyze convergence
;; - Reproduce the cutoff phenomenon for small groups

(ns harmonica-book.riffle-shuffle
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.representations :as rep]
   [scicloj.harmonica.protocols :as p]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [scicloj.kindly.v4.kind :as kind]
   [fastmath.matrix :as fm]))

;; ## Young's Orthogonal Form
;;
;; Every irreducible representation of $S_n$ can be realized as an orthogonal
;; matrix representation, indexed by a **partition** $\lambda$ of $n$.
;; The basis vectors are **standard Young tableaux** (SYTs) — fillings of
;; the Young diagram of $\lambda$ with $1, \ldots, n$ that increase along
;; rows and columns.

;; For $S_4$ with partition $[3\;1]$, the SYTs are:

(hm/standard-young-tableaux [3 1])

;; The dimension is $d_{[3,1]} = 3$, matching the hook-length formula:

(hm/hook-length-dimension [3 1])

;; The representation matrices for the generators $s_1, s_2, s_3$ (adjacent
;; transpositions swapping values $i$ and $i+1$):

(let [ir (hm/irrep [3 1])
      gens (hm/rep-generators ir)]
  (kind/table
   {:column-names ["Generator" "Matrix"]
    :row-vectors (mapv (fn [i g]
                         [(str "$s_" (inc i) "$") (str g)])
                       (range) gens)}))

;; These matrices are **orthogonal** ($\rho(\sigma)^T \rho(\sigma) = I$)
;; and satisfy the **homomorphism property** ($\rho(\sigma\tau) = \rho(\sigma)\rho(\tau)$).

;; ## Dimension Table for $S_5$
;;
;; Each partition $\lambda$ of $n$ gives an irrep of dimension $d_\lambda$.
;; The sum $\sum_\lambda d_\lambda^2 = n!$ (a consequence of Maschke's theorem).

(let [n 5
      parts (hm/partitions n)]
  (kind/table
   {:column-names ["$\\lambda$" "$d_\\lambda$" "$d_\\lambda^2$"]
    :row-vectors (conj (mapv (fn [lam]
                               (let [d (hm/hook-length-dimension lam)]
                                 [(str lam) d (* d d)]))
                             parts)
                       ["**Total**" ""
                        (reduce + (map #(let [d (hm/hook-length-dimension %)]
                                          (* d d))
                                       parts))])}))

(kind/test-last
 [(fn [_]
   (let [parts (hm/partitions 5)]
     (= 120 (reduce + (map #(let [d (hm/hook-length-dimension %)]
                               (* d d))
                            parts)))))])
;; ## The [Gilbert-Shannon-Reeds](https://en.wikipedia.org/wiki/Gilbert%E2%80%93Shannon%E2%80%93Reeds_model) Riffle Shuffle
;;
;; The GSR model for a single riffle shuffle:
;;
;; 1. Cut the deck into two packets according to a binomial distribution
;; 2. Interleave the packets, preserving relative order within each
;;
;; After $k$ shuffles (equivalently, an $a$-shuffle with $a = 2^k$),
;; the probability of permutation $\sigma$ is:
;;
;; $$Q_a(\sigma) = \frac{\binom{a + n - r(\sigma)}{n}}{a^n}$$
;;
;; where $r(\sigma)$ is the number of **rising sequences** of $\sigma$ —
;; maximal consecutive increasing runs. Equivalently,
;; $r(\sigma) = 1 + \text{descents}(\sigma^{-1})$.

;; For example, $[2\;0\;3\;1]$ has $\sigma^{-1} = [1\;3\;0\;2]$, with
;; a descent at position 1 (3 > 0), so $r = 1 + 1 = 2$ rising sequences:

(hm/rising-sequences [2 0 3 1])

(kind/test-last [= 2])

;; The identity permutation always has $r = 1$ (one rising sequence),
;; making it the most likely outcome:

(hm/gsr-probability (hm/identity-perm 4) 1)

;; ## Total Variation Distance: Exact Computation
;;
;; For small $n$, we can compute the exact total variation distance
;; between the $k$-shuffle distribution and uniform:
;;
;; $$\|Q^{*k} - U\|_{TV} = \frac{1}{2}\sum_{\sigma \in S_n} \left|Q_a(\sigma) - \frac{1}{n!}\right|$$

(let [n 5
      G (hm/symmetric-group n)
      elts (vec (p/elements G))
      n-elts (count elts)
      uniform (/ 1.0 n-elts)
      tv-data (mapv (fn [k]
                      (let [probs (hm/gsr-distribution-vec elts k)
                            tv (* 0.5 (reduce + (map #(Math/abs (- (aget ^doubles probs (int %)) uniform))
                                                     (range n-elts))))]
                        {:k k :tv tv}))
                    (range 1 15))]
  (-> (tc/dataset tv-data)
      (plotly/base {:=x :k :=y :tv})
      (plotly/layer-line {:=mark-color "steelblue"})
      (plotly/layer-point {:=mark-color "steelblue" :=mark-size 8})
      (plotly/update-data
       (fn [d] (assoc d
                      :=layout {:title "TV distance from uniform after k riffle shuffles (S₅)"
                                :xaxis {:title "k (number of shuffles)"}
                                :yaxis {:title "Total variation distance"
                                        :range [0 1.05]}})))
      plotly/plot))

;; The cutoff is at $k \approx \tfrac{3}{2}\log_2(n)$. For $n = 5$,
;; that is $\approx 3.5$ shuffles.

;; ## Matrix Fourier Transform of the GSR Distribution
;;
;; We compute $\hat{Q}(\rho_\lambda) = \sum_\sigma Q(\sigma) \cdot \rho_\lambda(\sigma)$
;; for all irreps of $S_4$.

(let [n 4
      G (hm/symmetric-group n)
      parts (hm/partitions n)
      irreps (mapv hm/irrep parts)
      k 2
      f (fn [sigma] (hm/gsr-probability sigma k))
      f-hats (hm/matrix-fourier-transform-all G f irreps)]
  (kind/table
   {:column-names ["$\\lambda$" "$d_\\lambda$"
                    "$\\|\\hat{Q}(\\rho_\\lambda)\\|_F$"]
    :row-vectors (mapv (fn [lam ir fh]
                         [(str lam)
                          (:dimension ir)
                          (format "%.6f" (hm/frobenius-norm fh))])
                       parts irreps f-hats)}))

;; ## Plancherel Identity Verification
;;
;; The Plancherel identity states:
;;
;; $$\sum_{\sigma \in G} |f(\sigma)|^2 = \frac{1}{|G|} \sum_\lambda d_\lambda \|\hat{f}(\rho_\lambda)\|_F^2$$

(let [n 4
      G (hm/symmetric-group n)
      parts (hm/partitions n)
      irreps (mapv hm/irrep parts)
      f (fn [sigma] (hm/gsr-probability sigma 2))
      f-hats (hm/matrix-fourier-transform-all G f irreps)
      lhs (rep/plancherel-lhs G f)
      rhs (rep/plancherel-rhs G f-hats irreps)]
  {:lhs lhs :rhs rhs :difference (Math/abs (- lhs rhs))})

(kind/test-last
 [(fn [result] (< (:difference result) 1e-10))])

;; ## Convergence Rates: Comparing $S_4$, $S_5$, $S_6$
;;
;; The cutoff at $k \approx \tfrac{3}{2}\log_2(n)$ means larger groups
;; need more shuffles. Let's compare.

(let [tv-data (vec
               (for [n [4 5 6]
                     k (range 1 15)]
                 (let [G (hm/symmetric-group n)
                       elts (vec (p/elements G))
                       n-elts (count elts)
                       uniform (/ 1.0 n-elts)
                       probs (hm/gsr-distribution-vec elts k)
                       tv (* 0.5 (reduce + (map #(Math/abs (- (aget ^doubles probs (int %)) uniform))
                                                (range n-elts))))]
                   {:k k :tv tv :n (str "n=" n)})))]
  (-> (tc/dataset tv-data)
      (plotly/base {:=x :k :=y :tv :=color :n})
      (plotly/layer-line)
      (plotly/layer-point {:=mark-size 6})
      (plotly/update-data
       (fn [d] (assoc d
                      :=layout {:title "Riffle shuffle cutoff phenomenon"
                                :xaxis {:title "k (number of shuffles)"}
                                :yaxis {:title "Total variation distance"
                                        :range [0 1.05]}})))
      plotly/plot))

;; ## Toward $n = 52$: The "Seven Shuffles" Result
;;
;; For a standard 52-card deck, the exact computation is infeasible
;; ($52! \approx 8 \times 10^{67}$ permutations). However, Bayer and Diaconis
;; computed the exact total variation distance using the formula for
;; $a$-shuffles. Their result:
;;
;; | $k$ | $\|Q^{*k} - U\|_{TV}$ |
;; |:---:|:----------------------:|
;; | 1   | 1.000                  |
;; | 3   | 1.000                  |
;; | 5   | 0.924                  |
;; | 6   | 0.614                  |
;; | 7   | 0.334                  |
;; | 8   | 0.167                  |
;; | 10  | 0.039                  |
;; | 12  | 0.009                  |
;;
;; The cutoff at $k \approx \tfrac{3}{2}\log_2(52) \approx 8.5$ is clearly
;; visible. After 7 shuffles the deck is "nearly random" (TV $\approx 1/3$),
;; while 6 shuffles leave it decidedly non-random (TV $> 0.6$).

;; ## Summary
;;
;; This notebook demonstrated:
;;
;; - **Standard Young Tableaux** as the basis for irreducible representations
;; - **Young's orthogonal form**: explicit orthogonal matrices for generators
;; - The **GSR riffle shuffle** distribution $Q_a(\sigma) = \binom{a+n-r}{n}/a^n$
;; - **Matrix Fourier transform** for non-class functions
;; - The **Plancherel identity** as a verification tool
;; - The **cutoff phenomenon**: a sharp transition from "far from random" to
;;   "nearly uniform" at $k \approx \tfrac{3}{2}\log_2(n)$
