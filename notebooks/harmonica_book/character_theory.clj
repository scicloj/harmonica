;; # Character Theory
;;
;; The **[character table](https://en.wikipedia.org/wiki/Character_table)** of a finite group encodes every [irreducible
;; representation](https://en.wikipedia.org/wiki/Irreducible_representation) as a single row of complex numbers. This notebook
;; introduces character tables and their key properties. For exhaustive
;; verification across many groups, see
;; [Algebraic Identities](algebraic_identities.html).

(ns harmonica-book.character-theory
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.complex :as cx]
   [scicloj.kindly.v4.kind :as kind]))

;; ## What is a character table?
;;
;; A character table is a square matrix indexed by **irreducible
;; representations** (rows) and **conjugacy classes** (columns). The entry
;; $\chi_\rho(C)$ is the [trace](https://en.wikipedia.org/wiki/Trace_(linear_algebra)) of the representation matrix $\rho(g)$
;; for any $g \in C$.
;;
;; The character table completely determines the representation theory of
;; the group. It is the Fourier-analytic backbone: characters are
;; to group Fourier analysis what sines and cosines are to classical
;; Fourier analysis.

;; ## Cyclic groups — the DFT matrix
;;
;; For $\mathbb{Z}/n\mathbb{Z}$, the character table is the DFT matrix:
;; $\chi_k(g) = \omega^{kg}$ where $\omega = e^{2\pi i/n}$.
;; All entries lie on the unit circle.

(let [ct (hm/character-table (hm/cyclic-group 6))]
  (count (:table ct)))

(kind/test-last [= 6])

;; Every entry has magnitude 1:

(let [ct (hm/character-table (hm/cyclic-group 8))
      entries (for [row (:table ct) v row] v)]
  (every? #(< (Math/abs (- (cx/cabs %) 1.0)) 1e-10) entries))

(kind/test-last [true?])

;; ## Symmetric groups — integer entries
;;
;; For $S_n$, the character table is computed by the [Murnaghan-Nakayama
;; rule](https://en.wikipedia.org/wiki/Murnaghan%E2%80%93Nakayama_rule). All entries are **real integers**.

(let [ct (hm/character-table (hm/symmetric-group 4))
      entries (for [row (:table ct) v row] v)]
  (every? (fn [v]
            (and (< (Math/abs (cx/im v)) 1e-10)
                 (< (Math/abs (- (cx/re v) (Math/round (cx/re v)))) 1e-10)))
          entries))

(kind/test-last [true?])

;; ## Dihedral groups
;;
;; For $D_n$, the character table has $\lfloor n/2 \rfloor + 3$ rows for
;; even $n$ and $(n+3)/2$ rows for odd $n$.

(let [ct (hm/character-table (hm/dihedral-group 5))]
  (count (:table ct)))

(kind/test-last [= 4])

(let [ct (hm/character-table (hm/dihedral-group 6))]
  (count (:table ct)))

(kind/test-last [= 6])

;; ## Row orthogonality
;;
;; The central identity ([Schur orthogonality](https://en.wikipedia.org/wiki/Schur_orthogonality_relations)):
;; different irreps are orthogonal as class functions.
;;
;; $$\sum_C |C| \, \chi_i(C) \, \overline{\chi_j(C)} = |G| \, \delta_{ij}$$
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

;; The result: the inner product matrix is $|G| \cdot I$. Row $i$ dotted
;; with row $j$ (weighted by class sizes) gives 0 when $i \neq j$ and
;; $|G|$ when $i = j$. This is what makes the Fourier inversion formula
;; work.

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

;; ## Known character tables
;;
;; We verify specific entries against standard references.

;; ### $S_3$ character table
;;
;; ||$[1,1,1]$|$[2,1]$|$[3]$|
;; |:--|:--:|:--:|:--:|
;; |$[3]$ (trivial)| 1 | 1 | 1 |
;; |$[2,1]$ (standard)| 2 | 0 | -1 |
;; |$[1,1,1]$ (sign)| 1 | -1 | 1 |

(let [ct (hm/character-table (hm/symmetric-group 3))
      re-table (mapv (fn [row] (mapv #(long (Math/round (cx/re %))) row))
                     (:table ct))]
  re-table)

(kind/test-last [= [[1 1 1] [2 0 -1] [1 -1 1]]])

;; ### $S_4$ character table

(let [ct (hm/character-table (hm/symmetric-group 4))
      re-table (mapv (fn [row] (mapv #(long (Math/round (cx/re %))) row))
                     (:table ct))]
  re-table)

(kind/test-last
 [= [[1 1 1 1 1]
     [3 1 -1 0 -1]
     [2 0 2 -1 0]
     [3 -1 -1 0 1]
     [1 -1 1 1 -1]]])

;; ### $D_3 \cong S_3$
;;
;; The dihedral group $D_3$ is isomorphic to $S_3$, so their character
;; table dimensions should match.

(let [ct-d3 (hm/character-table (hm/dihedral-group 3))
      dims (sort (mapv #(long (Math/round (cx/re (% 0)))) (:table ct-d3)))
      ct-s3 (hm/character-table (hm/symmetric-group 3))
      dims-s3 (sort (mapv #(long (Math/round (cx/re (% 0)))) (:table ct-s3)))]
  (= dims dims-s3))

(kind/test-last [true?])

;; ## Murnaghan-Nakayama spot checks
;;
;; The trivial character $\chi_{[n]}$ has value 1 for every conjugacy class.

(let [ct (hm/character-table (hm/symmetric-group 5))
      trivial-row (first (:table ct))]
  (every? #(< (cx/cabs (cx/csub % (cx/complex 1.0 0.0))) 1e-10)
          trivial-row))

(kind/test-last [true?])

;; The sign character $\chi_{[1^n]}(\mu) = (-1)^{n-k}$ where $k$ is
;; the number of parts of $\mu$.

(let [ct (hm/character-table (hm/symmetric-group 5))
      sign-label [1 1 1 1 1]
      labels (:irrep-labels ct)
      row-idx (.indexOf labels sign-label)
      sign-row (nth (:table ct) row-idx)
      classes (:classes ct)]
  (every? identity
          (map-indexed (fn [i mu]
                         (let [expected (Math/pow -1 (- 5 (count mu)))
                               actual (cx/re (nth sign-row i))]
                           (< (Math/abs (- actual expected)) 1e-10)))
                       classes)))

(kind/test-last [true?])

;; ## Dihedral character table structure
;;
;; For $D_n$ with odd $n$: 2 one-dimensional irreps + $(n-1)/2$ two-dimensional.
;;
;; For $D_n$ with even $n$: 4 one-dimensional irreps + $(n/2 - 1)$ two-dimensional.

(let [results
      (for [n (range 3 13)]
        (let [ct (hm/character-table (hm/dihedral-group n))
              dims (mapv #(long (Math/round (cx/re (% 0)))) (:table ct))
              one-dims (count (filter #(= 1 %) dims))
              two-dims (count (filter #(= 2 %) dims))
              expected-1d (if (odd? n) 2 4)
              expected-2d (if (odd? n) (quot (dec n) 2) (dec (quot n 2)))]
          (and (= one-dims expected-1d)
               (= two-dims expected-2d))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Character table visualization
;;
;; The character table of $S_5$:

(let [ct (hm/character-table (hm/symmetric-group 5))
      {:keys [table classes irrep-labels]} ct]
  (kind/table
   {:column-names (into [""] (mapv str classes))
    :row-vectors (mapv (fn [label row]
                         (into [(str label)]
                               (mapv #(long (Math/round (cx/re %))) row)))
                       irrep-labels table)}))

;; ## What comes next
;;
;; Characters are the "scalars" of representation theory. The full picture
;; includes the actual **matrices**:
;;
;; - **[Representation Matrices](representation_matrices.html)** — Young's
;;   orthogonal form, Coxeter relations, Schur orthogonality at the matrix level
;; - **[Random Transpositions](random_transpositions.html)** — using
;;   characters to analyze the cutoff phenomenon in card shuffling
;; - **[Riffle Shuffles](riffle_shuffle.html)** — when characters aren't
;;   enough and you need the full matrix Fourier transform
