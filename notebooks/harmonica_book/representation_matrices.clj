;; # Representation Matrices
;;
;; An irreducible representation $\rho_\lambda : S_n \to [GL(d_\lambda)](https://en.wikipedia.org/wiki/General_linear_group)$
;; assigns a matrix to each permutation. For the symmetric group, harmonica
;; uses **[Young's orthogonal form](https://en.wikipedia.org/wiki/Young%27s_orthogonal_representation)** — an explicit construction based on
;; standard Young tableaux.
;;
;; This notebook verifies the deep algebraic properties of these matrices
;; and explores tensor products and direct sums.

(ns harmonica-book.representation-matrices
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.representations :as rep]
   [scicloj.harmonica.complex :as cx]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Building an irrep
;;
;; An irreducible representation is constructed from a partition $\lambda$.
;; The dimension $d_\lambda$ equals the number of [standard Young tableaux](https://en.wikipedia.org/wiki/Young_tableau#Standard_Young_tableaux)
;; of shape $\lambda$.

(def ir-21 (hm/irrep [2 1]))

(hm/rep-dimension ir-21)

(kind/test-last [= 2])

(def ir-31 (hm/irrep [3 1]))

(hm/rep-dimension ir-31)

(kind/test-last [= 3])

;; The dimension matches the hook-length formula.

(let [results
      (for [n (range 2 8)
            lambda (hm/partitions n)]
        (= (hm/rep-dimension (hm/irrep lambda))
           (hm/hook-length-dimension lambda)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Homomorphism property
;;
;; The fundamental requirement: $\rho(\sigma \tau) = \rho(\sigma) \rho(\tau)$.
;; This must hold for every pair of permutations.

(let [results
      (for [n [3 4 5]
            lambda (hm/partitions n)]
        (let [G (hm/symmetric-group n)
              ir (hm/irrep lambda)
              elts (vec (hm/elements G))
              pairs (if (<= (count elts) 24)
                      (for [a elts b elts] [a b])
                      (let [rng (java.util.Random. 42)]
                        (repeatedly 300
                                    (fn []
                                      [(elts (.nextInt rng (count elts)))
                                       (elts (.nextInt rng (count elts)))]))))]
          (every? (fn [[s t]]
                    (let [st (hm/op G s t)
                          rho-st (hm/rep-matrix ir st)
                          rho-s-t (fm/mulm (hm/rep-matrix ir s)
                                           (hm/rep-matrix ir t))
                          diff (fm/sub rho-st rho-s-t)
                          err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                      (< err 1e-10)))
                  pairs)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Orthogonality of representation matrices
;;
;; Young's orthogonal form produces **orthogonal** matrices:
;; $\rho(\sigma)^T \rho(\sigma) = I$ for all $\sigma$.

(defn identity-matrix
  "Identity matrix as fastmath RealMatrix."
  [d]
  (fm/rows->mat (mapv (fn [i] (mapv (fn [j] (if (= i j) 1.0 0.0)) (range d)))
                      (range d))))

(let [results
      (for [n [3 4 5]
            lambda (hm/partitions n)]
        (let [G (hm/symmetric-group n)
              ir (hm/irrep lambda)
              d (hm/rep-dimension ir)
              I (identity-matrix d)]
          (every? (fn [sigma]
                    (let [M (hm/rep-matrix ir sigma)
                          MtM (fm/mulm (fm/transpose M) M)
                          diff (fm/sub MtM I)
                          err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                      (< err 1e-10)))
                  (hm/elements G))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Trace equals character
;;
;; $\text{tr}(\rho_\lambda(\sigma)) = \chi_\lambda(\text{cycle-type}(\sigma))$
;;
;; The trace of the representation matrix equals the character table
;; entry for the appropriate conjugacy class.

(let [results
      (for [n [3 4 5 6]]
        (let [G (hm/symmetric-group n)
              ct (hm/character-table G)
              classes (:classes ct)
              class-idx (into {} (map-indexed (fn [i c] [c i]) classes))
              labels (:irrep-labels ct)]
          (every? identity
                  (for [lambda (hm/partitions n)]
                    (let [ir (hm/irrep lambda)
                          row-idx (.indexOf labels lambda)
                          row (nth (:table ct) row-idx)]
                      (every? (fn [sigma]
                                (let [ct-idx (class-idx (hm/cycle-type sigma))
                                      chi-val (cx/re (row ct-idx))
                                      trace-val (hm/rep-character ir sigma)]
                                  (< (Math/abs (- chi-val trace-val)) 1e-8)))
                              (hm/elements G)))))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Identity maps to identity matrix
;;
;; $\rho(e) = I_{d \times d}$

(let [results
      (for [n [3 4 5 6]
            lambda (hm/partitions n)]
        (let [ir (hm/irrep lambda)
              d (hm/rep-dimension ir)
              I (identity-matrix d)
              rho-e (hm/rep-matrix ir (hm/identity-perm n))
              diff (fm/sub rho-e I)
              err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
          (< err 1e-10)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Inverse maps to transpose
;;
;; Since $\rho$ is orthogonal: $\rho(\sigma^{-1}) = \rho(\sigma)^T$.

(let [results
      (for [n [3 4 5]
            lambda (hm/partitions n)]
        (let [G (hm/symmetric-group n)
              ir (hm/irrep lambda)]
          (every? (fn [sigma]
                    (let [rho-inv (hm/rep-matrix ir (hm/inv G sigma))
                          rho-t (fm/transpose (hm/rep-matrix ir sigma))
                          diff (fm/sub rho-inv rho-t)
                          err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                      (< err 1e-10)))
                  (hm/elements G))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Generator matrices
;;
;; The generators $\rho(s_1), \ldots, \rho(s_{n-1})$ for adjacent
;; transpositions determine the entire representation. We verify that
;; generators satisfy the **Coxeter relations**:
;;
;; - $s_i^2 = e$ (involution)
;; - $s_i s_{i+1} s_i = s_{i+1} s_i s_{i+1}$ (braid relation)
;; - $s_i s_j = s_j s_i$ for $|i - j| \geq 2$ (commutativity)

(let [results
      (for [n [3 4 5 6]
            lambda (hm/partitions n)]
        (let [ir (hm/irrep lambda)
              gens (hm/rep-generators ir)
              d (hm/rep-dimension ir)
              I (identity-matrix d)
              mat-err (fn [A B]
                        (let [diff (fm/sub A B)]
                          (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))))]
          (and
           ;; s_i^2 = I
           (every? (fn [gi]
                     (< (mat-err (fm/mulm gi gi) I) 1e-10))
                   gens)
           ;; Braid relation: s_i s_{i+1} s_i = s_{i+1} s_i s_{i+1}
           (every? (fn [i]
                     (let [si (gens i)
                           sj (gens (inc i))
                           lhs (fm/mulm si (fm/mulm sj si))
                           rhs (fm/mulm sj (fm/mulm si sj))]
                       (< (mat-err lhs rhs) 1e-10)))
                   (range (- (count gens) 1)))
           ;; Far commutativity: s_i s_j = s_j s_i for |i-j| >= 2
           (every? (fn [[i j]]
                     (let [si (gens i)
                           sj (gens j)
                           lhs (fm/mulm si sj)
                           rhs (fm/mulm sj si)]
                       (< (mat-err lhs rhs) 1e-10)))
                   (for [i (range (count gens))
                         j (range (count gens))
                         :when (>= (Math/abs (- i j)) 2)]
                     [i j])))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Plancherel identity
;;
;; $$\sum_{\sigma \in G} |f(\sigma)|^2 = \frac{1}{|G|} \sum_\rho d_\rho \, \|\hat{f}(\rho)\|_F^2$$
;;
;; We test with several different functions on $S_3$ and $S_4$.

(let [results
      (for [n [3 4]
            seed [42 123 7]]
        (let [G (hm/symmetric-group n)
              parts (hm/partitions n)
              irreps (mapv hm/irrep parts)
              elts (vec (hm/elements G))
              rng (java.util.Random. seed)
              f (into {} (map (fn [sigma]
                                [sigma (.nextGaussian rng)])
                              elts))
              lhs (rep/plancherel-lhs G f)
              f-hats (rep/matrix-fourier-transform-all G f irreps)
              rhs (rep/plancherel-rhs G f-hats irreps)]
          (< (Math/abs (- lhs rhs)) 1e-8)))]
  (every? true? results))

(kind/test-last [true?])

;; ## [Tensor product](https://en.wikipedia.org/wiki/Tensor_product_of_representations)
;;
;; The tensor product $\rho_1 \otimes \rho_2$ has dimension
;; $d_1 \cdot d_2$ and character $\chi_1(g) \cdot \chi_2(g)$.

;; ### Dimension

(let [results
      (for [n [3 4 5]
            l1 (hm/partitions n)
            l2 (hm/partitions n)]
        (let [ir1 (hm/irrep l1)
              ir2 (hm/irrep l2)
              tp (hm/tensor-product ir1 ir2)]
          (= (hm/rep-dimension tp)
             (* (hm/rep-dimension ir1) (hm/rep-dimension ir2)))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Character multiplicativity
;;
;; $\chi_{\rho_1 \otimes \rho_2}(g) = \chi_{\rho_1}(g) \cdot \chi_{\rho_2}(g)$

(let [results
      (for [n [3 4]
            l1 (hm/partitions n)
            l2 (hm/partitions n)]
        (let [G (hm/symmetric-group n)
              ir1 (hm/irrep l1)
              ir2 (hm/irrep l2)
              tp (hm/tensor-product ir1 ir2)]
          (every? (fn [sigma]
                    (let [c1 (hm/rep-character ir1 sigma)
                          c2 (hm/rep-character ir2 sigma)
                          c-tp (fm/trace (hm/rep-matrix tp sigma))]
                      (< (Math/abs (- c-tp (* c1 c2))) 1e-8)))
                  (hm/elements G))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Homomorphism

(let [results
      (for [n [3 4]
            [l1 l2] (take 5 (for [a (hm/partitions n)
                                   b (hm/partitions n)
                                   :when (not= a b)]
                               [a b]))]
        (let [G (hm/symmetric-group n)
              ir1 (hm/irrep l1)
              ir2 (hm/irrep l2)
              tp (hm/tensor-product ir1 ir2)
              elts (vec (hm/elements G))]
          (every? (fn [[s t]]
                    (let [st (hm/op G s t)
                          rho-st (hm/rep-matrix tp st)
                          rho-s-t (fm/mulm (hm/rep-matrix tp s)
                                           (hm/rep-matrix tp t))
                          diff (fm/sub rho-st rho-s-t)
                          err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                      (< err 1e-10)))
                  (for [a elts b elts] [a b]))))]
  (every? true? results))

(kind/test-last [true?])

;; ## [Direct sum](https://en.wikipedia.org/wiki/Direct_sum_of_modules)
;;
;; The direct sum $\rho_1 \oplus \rho_2$ has dimension $d_1 + d_2$ and
;; character $\chi_1(g) + \chi_2(g)$.

;; ### Dimension

(let [results
      (for [n [3 4 5]
            l1 (hm/partitions n)
            l2 (hm/partitions n)]
        (let [ir1 (hm/irrep l1)
              ir2 (hm/irrep l2)
              ds (hm/direct-sum ir1 ir2)]
          (= (hm/rep-dimension ds)
             (+ (hm/rep-dimension ir1) (hm/rep-dimension ir2)))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Character additivity
;;
;; $\chi_{\rho_1 \oplus \rho_2}(g) = \chi_{\rho_1}(g) + \chi_{\rho_2}(g)$

(let [results
      (for [n [3 4]
            l1 (hm/partitions n)
            l2 (hm/partitions n)]
        (let [G (hm/symmetric-group n)
              ir1 (hm/irrep l1)
              ir2 (hm/irrep l2)
              ds (hm/direct-sum ir1 ir2)]
          (every? (fn [sigma]
                    (let [c1 (hm/rep-character ir1 sigma)
                          c2 (hm/rep-character ir2 sigma)
                          c-ds (fm/trace (hm/rep-matrix ds sigma))]
                      (< (Math/abs (- c-ds (+ c1 c2))) 1e-8)))
                  (hm/elements G))))]
  (every? true? results))

(kind/test-last [true?])

;; ## [Schur orthogonality relations](https://en.wikipedia.org/wiki/Schur_orthogonality_relations) (matrix entries)
;;
;; For distinct irreps $\rho$ and $\rho'$:
;;
;; $$\frac{1}{|G|} \sum_{\sigma \in G} \rho(\sigma)_{ij} \, \rho'(\sigma)_{kl} = 0$$
;;
;; For the same irrep:
;;
;; $$\frac{1}{|G|} \sum_{\sigma \in G} \rho(\sigma)_{ij} \, \rho(\sigma)_{kl} = \frac{\delta_{ik} \delta_{jl}}{d_\rho}$$
;;
;; We verify for $S_4$.

(let [G (hm/symmetric-group 4)
      order (hm/order G)
      elts (vec (hm/elements G))
      parts (hm/partitions 4)
      irreps (mapv hm/irrep parts)
      ;; Cross-irrep: average of product of entries should be 0
      cross-ok?
      (every? true?
              (for [a (range (count irreps))
                    b (range (count irreps))
                    :when (< a b)
                    :let [ira (irreps a)
                          irb (irreps b)
                          da (hm/rep-dimension ira)
                          db (hm/rep-dimension irb)]
                    i (range da) j (range da)
                    k (range db) l (range db)]
                (let [avg (/ (reduce + (map (fn [sigma]
                                              (let [Ma (hm/rep-matrix ira sigma)
                                                    Mb (hm/rep-matrix irb sigma)]
                                                (* (fm/entry Ma i j)
                                                   (fm/entry Mb k l))))
                                            elts))
                             (double order))]
                  (< (Math/abs avg) 1e-8))))
      ;; Same-irrep: verify diagonal pattern
      same-ok?
      (every? true?
              (for [a (range (count irreps))
                    :let [ira (irreps a)
                          da (hm/rep-dimension ira)]
                    i (range da) j (range da)
                    k (range da) l (range da)]
                (let [avg (/ (reduce + (map (fn [sigma]
                                              (let [M (hm/rep-matrix ira sigma)]
                                                (* (fm/entry M i j)
                                                   (fm/entry M k l))))
                                            elts))
                             (double order))
                      expected (if (and (= i k) (= j l))
                                 (/ 1.0 (double da))
                                 0.0)]
                  (< (Math/abs (- avg expected)) 1e-8))))]
  (and cross-ok? same-ok?))

(kind/test-last [true?])

;; ## Summary of verified identities
;;
;; This notebook verified:
;;
;; - **Dimension** matches hook-length formula for $n = 2, \ldots, 7$
;; - **Homomorphism** $\rho(\sigma\tau) = \rho(\sigma)\rho(\tau)$ for $S_3, S_4, S_5$
;; - **Orthogonal matrices** $\rho(\sigma)^T\rho(\sigma) = I$ for $S_3, S_4, S_5$
;; - **Trace = character** for $S_3, S_4, S_5, S_6$
;; - **Identity maps to $I$** for $S_3, S_4, S_5, S_6$
;; - **Inverse maps to transpose** for $S_3, S_4, S_5$
;; - **Coxeter relations** (involution, braid, commutativity) for $S_3$ through $S_6$
;; - **Plancherel identity** for $S_3, S_4$ with multiple random functions
;; - **Tensor product**: dimension, character multiplicativity, homomorphism
;; - **Direct sum**: dimension, character additivity
;; - **Schur orthogonality** (matrix entry level) for $S_4$


;; For an application of matrix representations to card shuffling, see
;; [Riffle Shuffles](riffle_shuffle.html).
