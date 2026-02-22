;; # Character Theory
;;
;; The **[character table](https://en.wikipedia.org/wiki/Character_table)** of a finite group is a square matrix
;; that encodes every [irreducible representation](https://en.wikipedia.org/wiki/Irreducible_representation) as a single row of
;; complex numbers. It is the Fourier-analytic backbone of the group: just
;; as the DFT matrix decomposes signals into frequencies, the character table
;; decomposes functions on the group into irreducible components.
;;
;; For a programmer, the character table is a precomputed lookup table.
;; Once you have it, Fourier transforms, inner products, and decompositions
;; reduce to matrix-vector operations.

(ns harmonica-book.character-theory
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
   [harmonica-book.book-helpers :refer [allclose?]]
   [tech.v3.datatype.functional :as dfn]
   [scicloj.kindly.v4.kind :as kind]))

;; ## What is a character?
;;
;; A **representation** $\rho : G \to GL(V)$ realizes each group element
;; as an invertible matrix, with group multiplication becoming matrix
;; multiplication. The **character** is the trace of this matrix:
;;
;; $$\chi_\rho(g) = \text{tr}(\rho(g))$$
;;
;; Why is the trace — a single number — enough to capture a matrix?
;; Three reasons:
;;
;; 1. **Class function**: The trace is invariant under conjugation
;;    ($\text{tr}(ABA^{-1}) = \text{tr}(B)$), so conjugate group elements
;;    have the same character value. The character depends only on the
;;    conjugacy class, not the individual element.
;;
;; 2. **Determines the representation**: Two irreducible representations
;;    with the same character are equivalent. The trace loses no
;;    information that matters.
;;
;; 3. **Orthogonality**: The irreducible characters form an orthonormal
;;    basis for the space of class functions — exactly what you need
;;    for Fourier analysis.
;;
;; The **character table** collects all irreducible characters into one
;; matrix. Rows are irreducible representations, columns are conjugacy
;; classes.

;; ## Cyclic groups — the DFT matrix
;;
;; For $\mathbb{Z}/n\mathbb{Z}$, the character table is the DFT matrix:
;; $\chi_k(g) = \omega^{kg}$ where $\omega = e^{2\pi i/n}$.
;; All entries lie on the unit circle. We saw this in detail in
;; [The DFT as Group Fourier Transform](dft_as_group_fourier.html).

(let [ct (hm/character-table (hm/cyclic-group 6))]
  (count (:table ct)))

(kind/test-last [= 6])

(hm/show-character-table (hm/character-table (hm/cyclic-group 6)))

;; Every entry has magnitude 1:

(allclose? (cx/cabs (:table (hm/character-table (hm/cyclic-group 8)))) 1.0)

(kind/test-last [true?])

;; ## Symmetric groups — integer entries
;;
;; For $S_n$, the character table is computed by the [Murnaghan-Nakayama
;; rule](https://en.wikipedia.org/wiki/Murnaghan%E2%80%93Nakayama_rule) —
;; a combinatorial algorithm based on border-strip tableaux.
;; A remarkable fact: all entries are **real integers**.

(let [table (:table (hm/character-table (hm/symmetric-group 4)))
      re-vals (cx/re table)]
  (and (allclose? (cx/im table) 0.0)
       (allclose? re-vals (dfn/rint re-vals))))

(kind/test-last [true?])

;; ## Known character tables
;;
;; ### $S_3$
;;
;; The character table of $S_3$ has three rows (one per partition of 3):
;; the trivial representation $[3]$, the standard representation $[2,1]$,
;; and the sign representation $[1,1,1]$.

(hm/show-character-table (hm/character-table (hm/symmetric-group 3)))

(let [ct (hm/character-table (hm/symmetric-group 3))
      re-table (mapv (fn [row] (mapv #(Math/round (cx/re %)) row))
                     (:table ct))]
  re-table)

(kind/test-last [= [[1 1 1] [2 0 -1] [1 -1 1]]])

;; The first column is the identity class — these entries are the
;; **dimensions** of each irrep ($1, 2, 1$). The dimension-squared
;; sum equals $|S_3| = 6$: $1^2 + 2^2 + 1^2 = 6$.

;; ### $S_4$

(hm/show-character-table (hm/character-table (hm/symmetric-group 4)))

(let [ct (hm/character-table (hm/symmetric-group 4))
      re-table (mapv (fn [row] (mapv #(Math/round (cx/re %)) row))
                     (:table ct))]
  re-table)

(kind/test-last
 [= [[1 1 1 1 1]
     [3 1 -1 0 -1]
     [2 0 2 -1 0]
     [3 -1 -1 0 1]
     [1 -1 1 1 -1]]])

;; ## Row orthogonality
;;
;; The central identity of character theory
;; ([Schur orthogonality](https://en.wikipedia.org/wiki/Schur_orthogonality_relations)):
;; different irreps are orthogonal as class functions.
;;
;; $$\frac{1}{|G|}\sum_C |C| \, \chi_i(C) \, \overline{\chi_j(C)} = \delta_{ij}$$
;;
;; This is the **Fourier inversion formula** for class functions. It
;; guarantees that the character table is invertible — that we can
;; decompose any class function into irreducible components.
;;
;; Let's verify for $S_4$ — a $5 \times 5$ character table.

(let [ct (hm/character-table (hm/symmetric-group 4))
      {:keys [table class-sizes]} ct
      order (hm/order (:group ct))
      n (count table)
      inner (fn [i j]
              (reduce + (map-indexed
                         (fn [k sz]
                           (let [ci ((table i) k) cj ((table j) k)]
                             (* (double sz)
                                (+ (* (cx/re ci) (cx/re cj))
                                   (* (cx/im ci) (cx/im cj))))))
                         class-sizes)))
      max-err (apply max
                     (for [i (range n) j (range n)]
                       (Math/abs (- (inner i j)
                                    (if (= i j) (double order) 0.0)))))]
  (< max-err 1e-8))

(kind/test-last [true?])

;; The inner product matrix is $|G| \cdot I$: row $i$ dotted with row $j$
;; (weighted by class sizes) gives $|G|$ on the diagonal and 0 off it.

;; ## Column orthogonality
;;
;; The dual identity: different conjugacy classes are orthogonal across irreps.
;;
;; $$\sum_\rho \chi_\rho(C_i) \, \overline{\chi_\rho(C_j)} = \frac{|G|}{|C_i|} \, \delta_{ij}$$

(let [ct (hm/character-table (hm/symmetric-group 4))
      {:keys [table class-sizes]} ct
      order (hm/order (:group ct))
      n (count class-sizes)
      max-err (apply max
                     (for [i (range n) j (range n)]
                       (let [ip (reduce + (map (fn [row]
                                                 (let [ci (row i) cj (row j)]
                                                   (+ (* (cx/re ci) (cx/re cj))
                                                      (* (cx/im ci) (cx/im cj)))))
                                               table))
                             expected (if (= i j)
                                        (/ (double order) (double (nth class-sizes i)))
                                        0.0)]
                         (Math/abs (- ip expected)))))]
  (< max-err 1e-8))

(kind/test-last [true?])

;; ## Dihedral groups
;;
;; For $D_n$, the character table has:
;;
;; - **Odd $n$**: 2 one-dimensional irreps + $(n-1)/2$ two-dimensional
;; - **Even $n$**: 4 one-dimensional irreps + $(n/2 - 1)$ two-dimensional
;;
;; All entries are real (dihedral characters can be realized over $\mathbb{R}$).

(hm/show-character-table (hm/character-table (hm/dihedral-group 4)))

;; The $D_3 \cong S_3$ isomorphism means their character dimensions match:

(let [ct-d3 (hm/character-table (hm/dihedral-group 3))
      dims (sort (mapv #(Math/round (cx/re (% 0))) (:table ct-d3)))
      ct-s3 (hm/character-table (hm/symmetric-group 3))
      dims-s3 (sort (mapv #(Math/round (cx/re (% 0))) (:table ct-s3)))]
  (= dims dims-s3))

(kind/test-last [true?])

;; For the structure of dihedral character tables across many values of
;; $n$, see [Algebraic Identities](algebraic_identities.html).

;; ## Murnaghan-Nakayama spot checks
;;
;; The trivial character $\chi_{[n]}$ has value 1 for every conjugacy class.

(let [trivial-row (first (:table (hm/character-table (hm/symmetric-group 5))))]
  (allclose? (cx/re trivial-row) 1.0))

(kind/test-last [true?])

;; The sign character $\chi_{[1^n]}(\mu) = (-1)^{n-k}$ where $k$ is
;; the number of parts of $\mu$.

(let [ct (hm/character-table (hm/symmetric-group 5))
      sign-label [1 1 1 1 1]
      labels (:irrep-labels ct)
      row-idx (.indexOf labels sign-label)
      sign-row (nth (:table ct) row-idx)
      classes (:classes ct)
      expected (mapv (fn [mu] (Math/pow -1 (- 5 (count mu)))) classes)]
  (allclose? (cx/re sign-row) expected))

(kind/test-last [true?])

;; ## The character table of $S_5$

(hm/show-character-table (hm/character-table (hm/symmetric-group 5)))

;; ## What comes next
;;
;; Characters are the "scalar" side of representation theory. The full
;; picture includes the actual **matrices**:
;;
;; - **[Random Transpositions](random_transpositions.html)** — using
;;   characters to analyze the cutoff phenomenon in card shuffling
;; - **[Representation Matrices](representation_matrices.html)** — Young's
;;   orthogonal form, explicit matrices for each irrep
;; - **[Riffle Shuffles](riffle_shuffle.html)** — when characters aren't
;;   enough and you need the full matrix Fourier transform
