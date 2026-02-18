;; # Representation Matrices
;;
;; An irreducible representation $\rho_\lambda : S_n \to GL(d_\lambda)$
;; assigns a matrix to each permutation. For the symmetric group, harmonica
;; uses **[Young's orthogonal form](https://en.wikipedia.org/wiki/Young%27s_orthogonal_representation)** — an explicit construction based on
;; standard Young tableaux.
;;
;; This notebook explores these matrices and their algebraic properties:
;; the homomorphism property, orthogonality, Coxeter relations,
;; tensor products, and the deep Schur orthogonality relations at the
;; matrix-entry level.

(ns harmonica-book.representation-matrices
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.analysis.representations :as rep]
   [scicloj.harmonica.linalg.complex :as cx]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Building an Irrep
;;
;; An irreducible representation is constructed from a partition $\lambda$.
;; The dimension $d_\lambda$ equals the number of standard Young tableaux
;; of shape $\lambda$ — which also equals the hook-length formula.

;; The partition $[3\;1]$ of $4$ gives a 3-dimensional irrep:

(def ir-31 (hm/irrep [3 1]))

(hm/rep-dimension ir-31)

(kind/test-last [= 3])

;; This matches the hook-length formula:

(hm/hook-length-dimension [3 1])

(kind/test-last [= 3])

;; The generator matrices $\rho(s_1), \rho(s_2), \rho(s_3)$ for adjacent
;; transpositions:

(let [gens (hm/rep-generators ir-31)]
  (kind/table
   {:column-names ["Generator" "Matrix"]
    :row-vectors (mapv (fn [i g]
                         [(str "$s_" (inc i) "$") (str g)])
                       (range) gens)}))

;; ## The Homomorphism Property
;;
;; The fundamental requirement of a representation:
;;
;; $$\rho(\sigma \tau) = \rho(\sigma) \, \rho(\tau)$$
;;
;; This must hold for every pair of permutations. Let's verify
;; for $\rho_{[3,1]}$ on $S_4$:

(defn mat-err
  "Frobenius norm of the difference of two matrices."
  [A B]
  (let [diff (fm/sub A B)]
    (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))))

(let [G (hm/symmetric-group 4)
      elts (vec (hm/elements G))]
  (every? (fn [[s t]]
            (let [st (hm/op G s t)
                  rho-st (hm/rep-matrix ir-31 st)
                  rho-s-t (fm/mulm (hm/rep-matrix ir-31 s)
                                   (hm/rep-matrix ir-31 t))]
              (< (mat-err rho-st rho-s-t) 1e-10)))
          (for [a elts b elts] [a b])))

(kind/test-last [true?])

;; ## Orthogonal Matrices
;;
;; Young's orthogonal form produces **orthogonal** matrices:
;; $\rho(\sigma)^T \rho(\sigma) = I$ for all $\sigma$.

(defn identity-matrix [d]
  (fm/rows->mat (mapv (fn [i] (mapv (fn [j] (if (= i j) 1.0 0.0)) (range d)))
                      (range d))))

(let [G (hm/symmetric-group 4)
      d (hm/rep-dimension ir-31)
      I (identity-matrix d)]
  (every? (fn [sigma]
            (let [M (hm/rep-matrix ir-31 sigma)
                  MtM (fm/mulm (fm/transpose M) M)]
              (< (mat-err MtM I) 1e-10)))
          (hm/elements G)))

(kind/test-last [true?])

;; ## Trace Equals Character
;;
;; The trace of a representation matrix equals the character value:
;;
;; $$\text{tr}(\rho_\lambda(\sigma)) = \chi_\lambda(\text{cycle-type}(\sigma))$$
;;
;; For example, the transposition $[1\;0\;2\;3]$ has cycle type $[2\;1\;1]$.
;; Its character under $\rho_{[3,1]}$:

(let [sigma [1 0 2 3]]
  {:trace (fm/trace (hm/rep-matrix ir-31 sigma))
   :character (hm/rep-character ir-31 sigma)})

;; Both should be $1.0$. Let's verify for all of $S_4$:

(let [G (hm/symmetric-group 4)
      ct (hm/character-table G)
      classes (:classes ct)
      class-idx (into {} (map-indexed (fn [i c] [c i]) classes))
      row-idx (.indexOf (:irrep-labels ct) [3 1])
      row (nth (:table ct) row-idx)]
  (every? (fn [sigma]
            (let [chi-val (cx/re (row (class-idx (hm/cycle-type sigma))))
                  trace-val (hm/rep-character ir-31 sigma)]
              (< (Math/abs (- chi-val trace-val)) 1e-8)))
          (hm/elements G)))

(kind/test-last [true?])

;; ## Identity and Inverse
;;
;; Two immediate consequences of the homomorphism property:
;;
;; - $\rho(e) = I$ (identity maps to identity matrix)
;; - $\rho(\sigma^{-1}) = \rho(\sigma)^T$ (inverse maps to transpose, since $\rho$ is orthogonal)

(let [d (hm/rep-dimension ir-31)
      I (identity-matrix d)
      rho-e (hm/rep-matrix ir-31 (hm/identity-perm 4))]
  (< (mat-err rho-e I) 1e-10))

(kind/test-last [true?])

(let [G (hm/symmetric-group 4)
      sigma [2 0 3 1]]
  (< (mat-err (hm/rep-matrix ir-31 (hm/inv G sigma))
              (fm/transpose (hm/rep-matrix ir-31 sigma)))
     1e-10))

(kind/test-last [true?])

;; ## Coxeter Relations
;;
;; The adjacent transpositions $s_1, \ldots, s_{n-1}$ generate $S_n$ and
;; satisfy the **[Coxeter relations](https://en.wikipedia.org/wiki/Coxeter_group)**:
;;
;; - **Involution**: $s_i^2 = e$
;; - **Braid relation**: $s_i s_{i+1} s_i = s_{i+1} s_i s_{i+1}$
;; - **Far commutativity**: $s_i s_j = s_j s_i$ for $|i - j| \geq 2$
;;
;; Any valid representation must respect these. Let's verify for $\rho_{[3,1]}$
;; of $S_4$ (generators $s_1, s_2, s_3$):

(let [gens (hm/rep-generators ir-31)
      d (hm/rep-dimension ir-31)
      I (identity-matrix d)
      ;; s_i^2 = I
      involution-ok?
      (every? (fn [gi]
                (< (mat-err (fm/mulm gi gi) I) 1e-10))
              gens)
      ;; s_i s_{i+1} s_i = s_{i+1} s_i s_{i+1}
      braid-ok?
      (every? (fn [i]
                (let [si (gens i)
                      sj (gens (inc i))
                      lhs (fm/mulm si (fm/mulm sj si))
                      rhs (fm/mulm sj (fm/mulm si sj))]
                  (< (mat-err lhs rhs) 1e-10)))
              (range (- (count gens) 1)))
      ;; s_i s_j = s_j s_i for |i-j| >= 2
      far-comm-ok?
      (every? (fn [[i j]]
                (let [si (gens i) sj (gens j)]
                  (< (mat-err (fm/mulm si sj) (fm/mulm sj si)) 1e-10)))
              (for [i (range (count gens))
                    j (range (count gens))
                    :when (>= (Math/abs (- i j)) 2)]
                [i j]))]
  {:involution involution-ok?
   :braid braid-ok?
   :far-commutativity far-comm-ok?})

(kind/test-last
 [(fn [result]
    (every? true? (vals result)))])

;; ## Plancherel Identity
;;
;; The [Plancherel identity](https://en.wikipedia.org/wiki/Plancherel_theorem)
;; relates the $\ell^2$ norm of a function to the Frobenius norms
;; of its Fourier transforms:
;;
;; $$\sum_{\sigma \in G} |f(\sigma)|^2 = \frac{1}{|G|} \sum_\rho d_\rho \, \|\hat{f}(\rho)\|_F^2$$
;;
;; This is the non-abelian analogue of Parseval's theorem. We verify with
;; a random function on $S_3$:

(let [G (hm/symmetric-group 3)
      parts (hm/partitions 3)
      irreps (mapv hm/irrep parts)
      elts (vec (hm/elements G))
      rng (java.util.Random. 42)
      f (into {} (map (fn [sigma]
                        [sigma (.nextGaussian rng)])
                      elts))
      lhs (rep/plancherel-lhs G f)
      f-hats (rep/matrix-fourier-transform-all G f irreps)
      rhs (rep/plancherel-rhs G f-hats irreps)]
  {:lhs (format "%.6f" lhs)
   :rhs (format "%.6f" rhs)
   :difference (Math/abs (- lhs rhs))})

(kind/test-last
 [(fn [result] (< (:difference result) 1e-10))])

;; ## Tensor Product
;;
;; The [tensor product](https://en.wikipedia.org/wiki/Tensor_product_of_representations)
;; $\rho_1 \otimes \rho_2$ has dimension $d_1 \cdot d_2$ and character
;; $\chi_1(g) \cdot \chi_2(g)$.

(let [ir1 (hm/irrep [2 1])
      ir2 (hm/irrep [2 1])
      tp (hm/tensor-product ir1 ir2)]
  {:dim-ir1 (hm/rep-dimension ir1)
   :dim-ir2 (hm/rep-dimension ir2)
   :dim-tensor (hm/rep-dimension tp)})

(kind/test-last
 [(fn [{:keys [dim-ir1 dim-ir2 dim-tensor]}]
    (= dim-tensor (* dim-ir1 dim-ir2)))])

;; Character multiplicativity: $\chi_{\rho_1 \otimes \rho_2}(\sigma) = \chi_{\rho_1}(\sigma) \cdot \chi_{\rho_2}(\sigma)$

(let [G (hm/symmetric-group 3)
      ir1 (hm/irrep [2 1])
      ir2 (hm/irrep [2 1])
      tp (hm/tensor-product ir1 ir2)]
  (every? (fn [sigma]
            (let [c1 (hm/rep-character ir1 sigma)
                  c2 (hm/rep-character ir2 sigma)
                  c-tp (fm/trace (hm/rep-matrix tp sigma))]
              (< (Math/abs (- c-tp (* c1 c2))) 1e-8)))
          (hm/elements G)))

(kind/test-last [true?])

;; ## Direct Sum
;;
;; The [direct sum](https://en.wikipedia.org/wiki/Direct_sum_of_modules)
;; $\rho_1 \oplus \rho_2$ has dimension $d_1 + d_2$ and character
;; $\chi_1(g) + \chi_2(g)$.

(let [ir1 (hm/irrep [2 1])
      ir2 (hm/irrep [1 1 1])
      ds (hm/direct-sum ir1 ir2)]
  {:dim-ir1 (hm/rep-dimension ir1)
   :dim-ir2 (hm/rep-dimension ir2)
   :dim-sum (hm/rep-dimension ds)})

(kind/test-last
 [(fn [{:keys [dim-ir1 dim-ir2 dim-sum]}]
    (= dim-sum (+ dim-ir1 dim-ir2)))])

;; Character additivity: $\chi_{\rho_1 \oplus \rho_2}(\sigma) = \chi_{\rho_1}(\sigma) + \chi_{\rho_2}(\sigma)$

(let [G (hm/symmetric-group 3)
      ir1 (hm/irrep [2 1])
      ir2 (hm/irrep [1 1 1])
      ds (hm/direct-sum ir1 ir2)]
  (every? (fn [sigma]
            (let [c1 (hm/rep-character ir1 sigma)
                  c2 (hm/rep-character ir2 sigma)
                  c-ds (fm/trace (hm/rep-matrix ds sigma))]
              (< (Math/abs (- c-ds (+ c1 c2))) 1e-8)))
          (hm/elements G)))

(kind/test-last [true?])

;; ## Schur Orthogonality (Matrix Entries)
;;
;; The [Schur orthogonality relations](https://en.wikipedia.org/wiki/Schur_orthogonality_relations)
;; are the deepest form of orthogonality for representations. They hold at
;; the level of **individual matrix entries**:
;;
;; For distinct irreps $\rho$ and $\rho'$:
;;
;; $$\frac{1}{|G|} \sum_{\sigma \in G} \rho(\sigma)_{ij} \, \rho'(\sigma)_{kl} = 0$$
;;
;; For the same irrep:
;;
;; $$\frac{1}{|G|} \sum_{\sigma \in G} \rho(\sigma)_{ij} \, \rho(\sigma)_{kl} = \frac{\delta_{ik} \delta_{jl}}{d_\rho}$$
;;
;; The character orthogonality relations (from the
;; [character theory](character_theory.html) notebook) follow as a corollary
;; by setting $i=j$ and $k=l$ and summing over diagonal entries.
;;
;; We verify for all irreps of $S_4$:

(let [G (hm/symmetric-group 4)
      order (hm/order G)
      elts (vec (hm/elements G))
      parts (hm/partitions 4)
      irreps (mapv hm/irrep parts)
      ;; Cross-irrep: average of product of entries should be 0
      cross-ok?
      (every? true?
              (for [a (range (count irreps))
                    b (range (inc a) (count irreps))
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
      ;; Same-irrep: verify the Kronecker delta pattern
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
  {:cross-irrep cross-ok? :same-irrep same-ok?})

(kind/test-last
 [(fn [result] (every? true? (vals result)))])

;; ## Summary
;;
;; This notebook demonstrated:
;;
;; - **Young's orthogonal form**: explicit orthogonal matrices for $S_n$ irreps
;; - **Homomorphism**: $\rho(\sigma\tau) = \rho(\sigma)\rho(\tau)$
;; - **Orthogonality**: $\rho(\sigma)^T\rho(\sigma) = I$
;; - **Trace = character**: connecting matrices back to the character table
;; - **Coxeter relations**: generator matrices satisfy the defining relations of $S_n$
;; - **Plancherel identity**: the non-abelian Parseval theorem
;; - **Tensor product** and **direct sum**: building new representations from old
;; - **Schur orthogonality**: entry-level orthogonality, the deepest form
;;
;; For bulk verification of these properties across many groups, see
;; [Algebraic Identities](algebraic_identities.html).
;;
;; For an application of matrix representations to card shuffling, see
;; [Riffle Shuffles](riffle_shuffle.html).

