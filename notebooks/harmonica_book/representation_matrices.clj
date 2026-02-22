;; # Representation Matrices
;;
;; An irreducible representation $\rho_\lambda : S_n \to GL(d_\lambda)$
;; assigns a matrix to each permutation. For the symmetric group, harmonica
;; uses **[Young's orthogonal form](https://en.wikipedia.org/wiki/Young%27s_orthogonal_representation)** — an explicit construction based on
;; standard Young tableaux.
;;
;; Where the [character table](character_theory.html) records only the
;; trace of each matrix, this notebook shows the matrices themselves:
;; how they look, how they compose, and what algebraic properties they
;; satisfy.

(ns harmonica-book.representation-matrices
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.analysis.representations :as rep]
   [scicloj.harmonica.linalg.complex :as cx]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Building an irrep
;;
;; An irreducible representation is constructed from a partition $\lambda$.
;; The dimension $d_\lambda$ equals the number of standard Young tableaux
;; of shape $\lambda$ — which also equals the hook-length formula.

;; The partition $[3\;1]$ of $4$ gives a 3-dimensional irrep:

(def ir-31 (hm/irrep [3 1]))

ir-31

(hm/rep-dimension ir-31)

(kind/test-last [= 3])

;; This matches the hook-length formula:

(hm/hook-length-dimension [3 1])

(kind/test-last [= 3])

;; ## What do the matrices look like?
;;
;; Before checking algebraic properties, let's see the actual matrices.
;; Here is $\rho_{[3,1]}$ evaluated at several permutations of $S_4$:

(let [perms [[0 1 2 3] [1 0 2 3] [1 2 0 3] [1 2 3 0]]]
  (kind/table
   {:column-names ["\u03c3" "Cycle type" "tr(\u03c1(\u03c3))" "\u03c1(\u03c3)"]
    :row-vectors (mapv (fn [sigma]
                         [(str sigma)
                          (str (hm/cycle-type sigma))
                          (format "%.0f" (fm/trace (hm/rep-matrix ir-31 sigma)))
                          (str (hm/rep-matrix ir-31 sigma))])
                       perms)}))

;; Notice:
;;
;; - The identity $[0\;1\;2\;3]$ maps to the identity matrix (trace $= 3 = d_\lambda$)
;; - The transposition $[1\;0\;2\;3]$ maps to a matrix with trace $1$
;; - The 3-cycle $[1\;2\;0\;3]$ maps to a matrix with trace $0$
;; - The 4-cycle $[1\;2\;3\;0]$ maps to a matrix with trace $-1$
;;
;; These traces are the character values from the [character table](character_theory.html)!

;; ## Generator matrices
;;
;; The generators $\rho(s_1), \rho(s_2), \rho(s_3)$ for adjacent
;; transpositions:

(let [gens (hm/rep-generators ir-31)]
  (kind/table
   {:column-names ["Generator" "Matrix"]
    :row-vectors (mapv (fn [i g]
                         [(str "$s_" (inc i) "$") (str g)])
                       (range) gens)}))

;; ## The homomorphism property
;;
;; The fundamental requirement of a representation:
;;
;; $$\rho(\sigma \tau) = \rho(\sigma) \, \rho(\tau)$$
;;
;; This must hold for every pair of permutations.

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

;; ## Orthogonal matrices
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

;; Two immediate consequences:
;;
;; - $\rho(e) = I$
;; - $\rho(\sigma^{-1}) = \rho(\sigma)^T$ (inverse = transpose for orthogonal matrices)

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

;; ## Coxeter relations
;;
;; The adjacent transpositions $s_1, \ldots, s_{n-1}$ generate $S_n$ and
;; satisfy the **[Coxeter relations](https://en.wikipedia.org/wiki/Coxeter_group)**:
;;
;; - **Involution**: $s_i^2 = e$
;; - **Braid relation**: $s_i s_{i+1} s_i = s_{i+1} s_i s_{i+1}$
;; - **Far commutativity**: $s_i s_j = s_j s_i$ for $|i - j| \geq 2$
;;
;; Any valid representation must respect these.

(let [gens (hm/rep-generators ir-31)
      d (hm/rep-dimension ir-31)
      I (identity-matrix d)
      involution-ok?
      (every? (fn [gi]
                (< (mat-err (fm/mulm gi gi) I) 1e-10))
              gens)
      braid-ok?
      (every? (fn [i]
                (let [si (gens i)
                      sj (gens (inc i))
                      lhs (fm/mulm si (fm/mulm sj si))
                      rhs (fm/mulm sj (fm/mulm si sj))]
                  (< (mat-err lhs rhs) 1e-10)))
              (range (- (count gens) 1)))
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

;; ## Plancherel identity
;;
;; The [Plancherel identity](https://en.wikipedia.org/wiki/Plancherel_theorem)
;; is the non-abelian Parseval theorem. It relates the $\ell^2$ norm of a
;; function to the Frobenius norms of its matrix Fourier transforms:
;;
;; $$\sum_{\sigma \in G} |f(\sigma)|^2 = \frac{1}{|G|} \sum_\rho d_\rho \, \|\hat{f}(\rho)\|_F^2$$

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

;; ## Tensor product and direct sum
;;
;; New representations from old:
;;
;; - The [tensor product](https://en.wikipedia.org/wiki/Tensor_product_of_representations)
;;   $\rho_1 \otimes \rho_2$ has dimension $d_1 \cdot d_2$ and character
;;   $\chi_1(g) \cdot \chi_2(g)$.
;; - The [direct sum](https://en.wikipedia.org/wiki/Direct_sum_of_modules)
;;   $\rho_1 \oplus \rho_2$ has dimension $d_1 + d_2$ and character
;;   $\chi_1(g) + \chi_2(g)$.

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

;; Direct sum:

(let [ir1 (hm/irrep [2 1])
      ir2 (hm/irrep [1 1 1])
      ds (hm/direct-sum ir1 ir2)
      G (hm/symmetric-group 3)]
  (every? (fn [sigma]
            (let [c1 (hm/rep-character ir1 sigma)
                  c2 (hm/rep-character ir2 sigma)
                  c-ds (fm/trace (hm/rep-matrix ds sigma))]
              (< (Math/abs (- c-ds (+ c1 c2))) 1e-8)))
          (hm/elements G)))

(kind/test-last [true?])

;; ## Schur orthogonality (matrix entries)
;;
;; The deepest orthogonality relations hold at the level of individual
;; matrix entries:
;;
;; $$\frac{1}{|G|} \sum_{\sigma \in G} \rho(\sigma)_{ij} \, \rho'(\sigma)_{kl} = 0 \quad (\rho \neq \rho')$$
;;
;; $$\frac{1}{|G|} \sum_{\sigma \in G} \rho(\sigma)_{ij} \, \rho(\sigma)_{kl} = \frac{\delta_{ik} \delta_{jl}}{d_\rho}$$
;;
;; The character orthogonality relations from
;; [Character Theory](character_theory.html) follow as a corollary by
;; taking $i = j$ and $k = l$ and summing over diagonal entries.
;;
;; We verify for all irreps of $S_4$:

(let [G (hm/symmetric-group 4)
      order (hm/order G)
      elts (vec (hm/elements G))
      parts (hm/partitions 4)
      irreps (mapv hm/irrep parts)
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

;; ## What comes next
;;
;; These matrices are the building blocks for non-abelian Fourier analysis:
;;
;; - **[Riffle Shuffles](riffle_shuffle.html)** — the matrix Fourier
;;   transform applied to card shuffling, where characters alone are
;;   not enough
;;
;; For applications that need only characters (not full matrices), see
;; [Random Transpositions](random_transpositions.html).
