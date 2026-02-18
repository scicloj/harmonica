;; # Random Transpositions and the Cutoff Phenomenon
;;
;; How many random transpositions does it take to mix up a deck of cards?
;;
;; At each step, we pick a random transposition $(i\; j)$ uniformly — including
;; the "identity transposition" where $i = j$ — and apply it. After $k$ steps,
;; how close is the resulting distribution to uniform?
;;
;; **Diaconis and Shahshahani (1981)** proved a remarkable result: the
;; distribution stays far from uniform until approximately $\tfrac{1}{2}n\ln n$
;; steps, then rapidly converges. This sharp transition is called the
;; **[cutoff phenomenon](https://en.wikipedia.org/wiki/Cutoff_(Markov_chains))**.
;;
;; The analysis uses **[Fourier analysis on the symmetric group](https://en.wikipedia.org/wiki/Fourier_analysis_on_finite_groups)** — exactly
;; the character tables we computed via the Murnaghan-Nakayama rule.

(ns harmonica-book.random-transpositions
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
   [harmonica-book.book-helpers :refer [allclose?]]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Character table of $S_5$

;; Let's start by looking at the character table for a small symmetric group.
;; Rows are indexed by **partitions** (labeling irreducible representations),
;; columns by **conjugacy classes** (also indexed by partitions = cycle types).

(let [ct (hm/character-table (hm/symmetric-group 5))]
  (kind/table
   {:column-names (into ["Irrep $\\lambda$"]
                        (map #(str %) (:classes ct)))
    :row-vectors (mapv (fn [label row]
                         (into [(str label)]
                               (map #(long (cx/re %)) row)))
                       (:irrep-labels ct) (:table ct))}))

;; Each entry $\chi_\lambda(\mu)$ is an integer. The first column (identity
;; class) gives the **dimension** $d_\lambda$ of each irrep.

(count (:irrep-labels (hm/character-table (hm/symmetric-group 5))))

(kind/test-last [= 7])

;; ## Row orthogonality

;; The character table satisfies a fundamental orthogonality relation:
;;
;; $$\langle \chi_\lambda, \chi_\mu \rangle
;;   = \frac{1}{|G|} \sum_C |C|\;\chi_\lambda(C)\;\overline{\chi_\mu(C)}
;;   = \delta_{\lambda\mu}$$

(let [ct (hm/character-table (hm/symmetric-group 5))
      table (:table ct)
      sizes (:class-sizes ct)
      order 120]
  (kind/table
   {:column-names (into [""] (map str (:irrep-labels ct)))
    :row-vectors (mapv (fn [i]
                         (into [(str ((:irrep-labels ct) i))]
                               (map (fn [j]
                                      (let [v (hm/character-inner-product
                                               (table i) (table j) sizes order)]
                                        (format "%.0f" (cx/re v))))
                                    (range (count table)))))
                       (range (count table)))}))

;; The result is the identity matrix — each irrep is orthonormal.

(let [ct (hm/character-table (hm/symmetric-group 5))
      table (:table ct)
      sizes (:class-sizes ct)
      k (count (:irrep-labels ct))
      errors (double-array
              (for [i (range k) j (range k)]
                (- (cx/re (hm/character-inner-product
                           (table i) (table j) sizes 120))
                   (if (= i j) 1.0 0.0))))]
  (allclose? errors 0.0))

(kind/test-last [true?])

;; ## The random transposition walk

;; A **random transposition** on $S_n$ picks an ordered pair $(i, j)$
;; uniformly from $\{0,\ldots,n{-}1\}^2$. If $i \neq j$, apply the
;; transposition $(i\; j)$. If $i = j$, do nothing.
;;
;; There are $\binom{n}{2}$ distinct transpositions and 1 identity, giving
;; $M = \binom{n}{2} + 1$ equally weighted outcomes. Since the distribution
;; depends only on cycle type, it is a **class function** — its Fourier
;; transform gives a scalar for each irrep.

;; ## Fourier coefficients via characters

;; For a class function $f$, the Fourier coefficient at irrep $\lambda$ is:
;;
;; $$\hat{f}(\lambda) = \frac{1}{d_\lambda}\sum_\mu |C_\mu|\;f(\mu)\;\chi_\lambda(\mu)$$
;;
;; For random transpositions, only the identity class and the transposition
;; class $[2,1^{n-2}]$ contribute:
;;
;; $$\beta_\lambda = \frac{1 + \binom{n}{2}\;\chi_\lambda([2,1^{n-2}])/d_\lambda}{M}$$

;; ## Closed-form eigenvalues

;; A key result gives the ratio $\chi_\lambda([2,1^{n-2}])/d_\lambda$ in
;; closed form:
;;
;; $$\frac{\chi_\lambda([2,1^{n-2}])}{d_\lambda} = \frac{n(\lambda) - n(\lambda')}{\binom{n}{2}}$$
;;
;; where $n(\lambda) = \sum_i \binom{\lambda_i}{2}$ and $\lambda'$ is the
;; conjugate partition. The eigenvalue simplifies to:
;;
;; $$\beta_\lambda = \frac{1 + n(\lambda) - n(\lambda')}{\binom{n}{2} + 1}$$
;;
;; This formula only needs partition shapes — no character table — so it
;; scales to arbitrarily large $n$.

(defn n-stat
  "n(lambda) = sum_i C(lambda_i, 2) = sum_i lambda_i*(lambda_i-1)/2."
  [lambda]
  (reduce + (map (fn [p] (/ (* p (dec p)) 2)) lambda)))

(defn eigenvalue
  "Eigenvalue of the random transposition operator on irrep lambda of S_n."
  [n lambda]
  (let [M (inc (/ (* n (dec n)) 2))]
    (/ (double (+ 1 (n-stat lambda) (- (n-stat (hm/partition-conjugate lambda)))))
       (double M))))

(defn hook-length-dim
  "Dimension of irrep lambda via the hook-length formula: n! / prod h(i,j)."
  [lambda]
  (if (empty? lambda)
    1
    (let [n (reduce + lambda)
          conj (hm/partition-conjugate lambda)]
      (/ (reduce *' (range 1 (inc n)))
         (reduce *' (for [i (range (count lambda))
                          j (range (lambda i))]
                      (+ (- (lambda i) j)
                         (- (conj j) i)
                         -1)))))))

;; Let's verify the closed-form eigenvalues against the character table
;; for $S_5$.

(let [n 5
      ct (hm/character-table (hm/symmetric-group n))
      table (:table ct)
      M (inc (/ (* n (dec n)) 2))
      trans-idx (.indexOf ^clojure.lang.PersistentVector (:classes ct)
                          (into [2] (repeat (- n 2) 1)))]
  (kind/table
   {:column-names ["$\\lambda$" "$d_\\lambda$" "$\\beta$ (table)" "$\\beta$ (closed)"]
    :row-vectors
    (mapv (fn [i]
            (let [lam ((:irrep-labels ct) i)
                  d (long (cx/re ((table i) 0)))
                  chi-t (long (cx/re ((table i) trans-idx)))
                  from-table (/ (+ 1.0 (* (/ (* n (dec n)) 2) (/ (double chi-t) d))) M)]
              [(str lam) d (format "%.4f" from-table) (format "%.4f" (eigenvalue n lam))]))
          (range (count table)))}))

(kind/test-last
 [(fn [_]
    (let [n 5
          ct (hm/character-table (hm/symmetric-group n))
          table (:table ct)
          M (inc (/ (* n (dec n)) 2))
          trans-idx (.indexOf ^clojure.lang.PersistentVector
                     (:classes ct)
                              (into [2] (repeat (- n 2) 1)))]
      (every?
       (fn [i]
         (let [lam ((:irrep-labels ct) i)
               d (cx/re ((table i) 0))
               chi-t (cx/re ((table i) trans-idx))
               from-table (/ (+ 1.0 (* (/ (* n (dec n)) 2) (/ chi-t d))) M)]
           (< (Math/abs (- from-table (eigenvalue n lam))) 1e-10)))
       (range (count table)))))])

;; The two columns match exactly.

;; The trivial representation $[n]$ always has eigenvalue 1:

(eigenvalue 5 [5])

(kind/test-last [= 1.0])

;; The sign representation $[1,1,1,1,1]$ has the smallest eigenvalue:

(eigenvalue 5 [1 1 1 1 1])

(kind/test-last [(fn [v] (< (Math/abs (- v (/ -9.0 11.0))) 1e-10))])

;; ## The Upper Bound Lemma

;; **Diaconis's Upper Bound Lemma** bounds the total variation distance
;; between $Q^{*k}$ (the distribution after $k$ steps) and the uniform
;; distribution $U$:
;;
;; $$4\,\|Q^{*k} - U\|_{\mathrm{TV}}^2
;;   \;\le\; \sum_{\lambda \neq \mathrm{trivial}} d_\lambda^2\;|\beta_\lambda|^{2k}$$
;;
;; The trivial representation (partition $[n]$) is excluded because
;; $\beta = 1$.

(defn tv-upper-bound
  "Upper bound on ||Q^{*k} - U||_TV via Diaconis's Upper Bound Lemma."
  [partitions-data k]
  (let [ub (reduce + (map (fn [{:keys [dim eigenvalue]}]
                            (* dim dim (Math/pow (Math/abs (double eigenvalue)) (* 2 k))))
                          partitions-data))]
    (min 1.0 (* 0.5 (Math/sqrt ub)))))

;; ## The cutoff phenomenon

;; The predicted cutoff is at $k \approx \tfrac{1}{2}n\ln n$. Let's compute
;; the upper bound for several deck sizes and observe the sharp transition.

(let [ns-to-plot [10 20 30 40]
      rows (vec (for [n ns-to-plot
                      :let [parts (hm/partitions n)
                            data (mapv (fn [p] {:dim (double (hook-length-dim p))
                                                :eigenvalue (eigenvalue n p)})
                                       (rest parts))
                            cutoff (* 0.5 n (Math/log n))
                            k-max (long (* 3 cutoff))]
                      k (range 0 (inc k-max) (max 1 (long (/ k-max 100))))]
                  {:n (str "n=" n) :k k :tv-bound (tv-upper-bound data k)}))]
  (-> (tc/dataset rows)
      (plotly/base {:=x :k :=y :tv-bound :=color :n})
      (plotly/layer-line)
      (plotly/update-data
       (fn [d] (assoc d
                      :=layout {:title "Random Transpositions: TV Distance Upper Bound"
                                :xaxis {:title "Steps (k)"}
                                :yaxis {:title "TV upper bound" :range [0 1.05]}})))
      plotly/plot))

;; All curves show the cutoff shape: near 1 for a while, then a sharp
;; drop. The larger the deck, the sharper the transition.

;; Verify: after 1 step, the walk is far from uniform (TV bound > 0.5):

(let [n 10
      parts (hm/partitions n)
      data (mapv (fn [p] {:dim (double (hook-length-dim p))
                          :eigenvalue (eigenvalue n p)})
                 (rest parts))]
  (> (tv-upper-bound data 1) 0.5))

(kind/test-last [true?])

;; After many steps, the bound approaches 0:

(let [n 10
      parts (hm/partitions n)
      data (mapv (fn [p] {:dim (double (hook-length-dim p))
                          :eigenvalue (eigenvalue n p)})
                 (rest parts))]
  (< (tv-upper-bound data 100) 0.01))

(kind/test-last [true?])

;; ## Cutoff in normalized time

;; To confirm the $\tfrac{1}{2}n\ln n$ scaling, we normalize the x-axis by
;; dividing $k$ by $\tfrac{1}{2}n\ln n$. All curves should drop near $x = 1$.

(let [ns-to-plot [10 20 30 40]
      rows (vec (for [n ns-to-plot
                      :let [parts (hm/partitions n)
                            data (mapv (fn [p] {:dim (double (hook-length-dim p))
                                                :eigenvalue (eigenvalue n p)})
                                       (rest parts))
                            cutoff (* 0.5 n (Math/log n))
                            k-max (long (* 3 cutoff))]
                      k (range 0 (inc k-max) (max 1 (long (/ k-max 100))))]
                  {:n (str "n=" n) :k-normalized (/ (double k) cutoff)
                   :tv-bound (tv-upper-bound data k)}))]
  (-> (tc/dataset rows)
      (plotly/base {:=x :k-normalized :=y :tv-bound :=color :n})
      (plotly/layer-line)
      (plotly/update-data
       (fn [d] (assoc d
                      :=layout {:title "Cutoff at k = (1/2) n ln(n)"
                                :xaxis {:title "k / ((1/2) n ln n)"}
                                :yaxis {:title "TV upper bound" :range [0 1.05]}})))
      plotly/plot))

;; All curves drop around $x \approx 1$, confirming the $\tfrac{1}{2}n\ln n$
;; scaling law.

;; ## Summary

(kind/table
 {:column-names ["$n$" "$|S_n|$" "$\\tfrac{1}{2}n\\ln n$" "# partitions"]
  :row-vectors (mapv (fn [n]
                       [n
                        (str (reduce *' (range 1 (inc n))))
                        (format "%.1f" (* 0.5 n (Math/log n)))
                        (count (hm/partitions n))])
                     [5 10 15 20 30 40 52])})

;; For a standard 52-card deck, the cutoff is at about
;; $\tfrac{1}{2} \cdot 52 \cdot \ln 52 \approx 103$ random transpositions.

;; ## What drives the cutoff
;;
;; The Fourier transform decomposes the random walk into independent
;; components — one per irreducible representation. Each component decays
;; at rate $|\beta_\lambda|^{2k}$. The **slowest-decaying** non-trivial
;; component controls the mixing time.
;;
;; For random transpositions, the largest non-trivial eigenvalue is
;; $\beta_{[n-1,1]} = (n-2)/(n+1)$, which is close to 1 for large $n$.
;; But because the sum includes $d_\lambda^2$ factors (which grow
;; combinatorially with the partition), the overall mixing time is
;; $\tfrac{1}{2}n\ln n$ — governed by the interplay between eigenvalue
;; decay and multiplicity.

;; ## Connection to coupon collecting
;;
;; The $\tfrac{1}{2}n\ln n$ threshold has an intuitive explanation via
;; the **coupon collector problem**. For the permutation to be fully
;; mixed, every position $\{0,\ldots,n{-}1\}$ must be "touched" by at
;; least one transposition. The expected number of random events to cover
;; all $n$ positions is $n H_n \sim n\ln n$, where $H_n$ is the $n$-th
;; harmonic number. Since each transposition touches two positions, we
;; divide by 2, yielding the $\tfrac{1}{2}n\ln n$ cutoff.
;;
;; The Fourier analysis makes this precise and quantitative.

;; ## References
;;
;; - Diaconis, P. & Shahshahani, M. (1981). "Generating a random
;;   permutation with random transpositions." *Z. Wahrscheinlichkeitstheorie*
;;   **57**, 159–179.
;;
;; - Diaconis, P. (1988). *Group Representations in Probability and
;;   Statistics*. IMS Lecture Notes.
;;
;; - Sagan, B. (2001). *The Symmetric Group*. Springer GTM 203.

;; ## Where to go next
;;
;; - For **non-class-function** analysis (riffle shuffles require
;;   matrix-valued Fourier transforms): [Riffle Shuffles](riffle_shuffle.html)
;; - For the **character theory** behind these computations:
;;   [Character Theory](character_theory.html)
;; - For the **1D DFT** on cyclic groups:
;;   [The DFT as Group Fourier Transform](dft_as_group_fourier.html)

