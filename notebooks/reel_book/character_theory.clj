;; # Character Theory
;;
;; The **character table** of a finite group encodes every irreducible
;; representation as a single row of complex numbers. This notebook
;; explores character tables and their deep orthogonality properties
;; across all group types in the library: cyclic, symmetric, and dihedral.
;;
;; The properties verified here are not just mathematical curiosities —
;; they are the **structural backbone** that makes Fourier analysis on
;; finite groups work.

(ns reel-book.character-theory
  (:require
   [scicloj.reel.core :as reel]
   [fastmath.complex :as c]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Character tables by group type
;;
;; A character table is a square matrix indexed by irreducible representations
;; (rows) and conjugacy classes (columns). The entry $\chi_\rho(C)$ is
;; the trace of the representation matrix $\rho(g)$ for any $g \in C$.

;; ### Cyclic groups
;;
;; For $\mathbb{Z}/n\mathbb{Z}$, the character table is the DFT matrix:
;; $\chi_k(g) = \omega^{kg}$ where $\omega = e^{2\pi i/n}$.

(let [ct (reel/character-table (reel/cyclic-group 4))]
  (count (:table ct)))

(kind/test-last [= 4])

;; All entries lie on the unit circle.

(let [ct (reel/character-table (reel/cyclic-group 8))
      entries (for [row (:table ct) v row] v)]
  (every? #(< (Math/abs (- (c/abs %) 1.0)) 1e-10) entries))

(kind/test-last [true?])

;; ### Symmetric groups
;;
;; For $S_n$, the character table is computed by the Murnaghan-Nakayama
;; rule. All entries are real integers.

(let [ct (reel/character-table (reel/symmetric-group 4))
      entries (for [row (:table ct) v row] v)]
  (every? (fn [v]
            (and (< (Math/abs (c/im v)) 1e-10)
                 (< (Math/abs (- (c/re v) (Math/round (c/re v)))) 1e-10)))
          entries))

(kind/test-last [true?])

;; ### Dihedral groups
;;
;; For $D_n$, the character table has $\lfloor n/2 \rfloor + 3$ rows for
;; even $n$ and $(n+3)/2$ rows for odd $n$. Two-dimensional characters
;; are $2\cos(2\pi mk/n)$ on rotation classes and $0$ on reflections.

(let [ct (reel/character-table (reel/dihedral-group 5))]
  (count (:table ct)))

(kind/test-last [= 4])

(let [ct (reel/character-table (reel/dihedral-group 6))]
  (count (:table ct)))

(kind/test-last [= 6])

;; ## Row orthogonality
;;
;; The central identity: different irreps are orthogonal as class functions.
;;
;; $$\sum_C |C| \, \chi_i(C) \, \overline{\chi_j(C)} = |G| \, \delta_{ij}$$
;;
;; This is verified across all group families with diverse sizes.

(defn check-row-orthogonality
  "Check row orthogonality for a character table. Returns max absolute error."
  [ct]
  (let [{:keys [table class-sizes]} ct
        order (reel/order (:group ct))
        n (count table)]
    (apply max
           (for [i (range n) j (range n)]
             (let [ip (reduce + (map-indexed
                                 (fn [k sz]
                                   (let [ci (nth (nth table i) k)
                                         cj (nth (nth table j) k)]
                                     (* (double sz)
                                        (+ (* (c/re ci) (c/re cj))
                                           (* (c/im ci) (c/im cj))))))
                                 class-sizes))
                   expected (if (= i j) (double order) 0.0)]
               (Math/abs (- ip expected)))))))

;; Cyclic groups:

(let [results
      (for [n [2 3 5 7 11 13 16 24]]
        (< (check-row-orthogonality (reel/character-table (reel/cyclic-group n)))
           1e-8))]
  (every? true? results))

(kind/test-last [true?])

;; Symmetric groups:

(let [results
      (for [n [2 3 4 5 6]]
        (< (check-row-orthogonality (reel/character-table (reel/symmetric-group n)))
           1e-8))]
  (every? true? results))

(kind/test-last [true?])

;; Dihedral groups:

(let [results
      (for [n [3 4 5 6 7 8 9 10 12 15 16 20 24]]
        (< (check-row-orthogonality (reel/character-table (reel/dihedral-group n)))
           1e-8))]
  (every? true? results))

(kind/test-last [true?])

;; ## Column orthogonality
;;
;; The dual identity: different conjugacy classes are orthogonal across irreps.
;;
;; $$\sum_\rho \chi_\rho(C_i) \, \overline{\chi_\rho(C_j)} = \frac{|G|}{|C_i|} \, \delta_{ij}$$

(defn check-column-orthogonality
  "Check column orthogonality. Returns max absolute error."
  [ct]
  (let [{:keys [table class-sizes]} ct
        order (reel/order (:group ct))
        n (count class-sizes)]
    (apply max
           (for [i (range n) j (range n)]
             (let [ip (reduce + (map (fn [row]
                                       (let [ci (nth row i) cj (nth row j)]
                                         (+ (* (c/re ci) (c/re cj))
                                            (* (c/im ci) (c/im cj)))))
                                     table))
                   expected (if (= i j)
                              (/ (double order) (double (nth class-sizes i)))
                              0.0)]
               (Math/abs (- ip expected)))))))

;; All group families:

(let [results
      (concat
       (for [n [3 5 7 12 16]]
         (< (check-column-orthogonality (reel/character-table (reel/cyclic-group n)))
            1e-8))
       (for [n [3 4 5 6]]
         (< (check-column-orthogonality (reel/character-table (reel/symmetric-group n)))
            1e-8))
       (for [n [3 4 5 6 8 10 12 15]]
         (< (check-column-orthogonality (reel/character-table (reel/dihedral-group n)))
            1e-8)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Dimension-squared sum
;;
;; $$\sum_\rho d_\rho^2 = |G|$$
;;
;; where $d_\rho = \chi_\rho(e)$ is the dimension (the character value at
;; the identity element, which is always the first class).

(defn dim-sq-sum-check
  "Verify dimension-squared sum equals group order."
  [ct]
  (let [{:keys [table]} ct
        order (reel/order (:group ct))
        dims (map #(c/re (first %)) table)
        sum-sq (reduce + (map #(* % %) dims))]
    (< (Math/abs (- sum-sq (double order))) 1e-8)))

(let [results
      (concat
       (for [n [2 3 5 7 11 16 24]]
         (dim-sq-sum-check (reel/character-table (reel/cyclic-group n))))
       (for [n [2 3 4 5 6 7]]
         (dim-sq-sum-check (reel/character-table (reel/symmetric-group n))))
       (for [n [3 4 5 6 7 8 10 12 15 20]]
         (dim-sq-sum-check (reel/character-table (reel/dihedral-group n)))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Number of irreps equals number of classes
;;
;; The character table is always square: the number of irreducible
;; representations equals the number of conjugacy classes.

(let [results
      (concat
       (for [n [2 3 5 7 12 24]]
         (let [G (reel/cyclic-group n)
               ct (reel/character-table G)]
           (= (count (:table ct))
              (count (reel/conjugacy-classes G)))))
       (for [n [2 3 4 5 6 7]]
         (let [G (reel/symmetric-group n)
               ct (reel/character-table G)]
           (= (count (:table ct))
              (count (reel/conjugacy-classes G)))))
       (for [n [3 4 5 6 8 10 12]]
         (let [G (reel/dihedral-group n)
               ct (reel/character-table G)]
           (= (count (:table ct))
              (count (reel/conjugacy-classes G))))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Trivial character
;;
;; Every group has a trivial representation (the first row) with all
;; character values equal to 1.

(let [results
      (concat
       (for [n [2 5 12]]
         (let [ct (reel/character-table (reel/cyclic-group n))]
           (every? #(< (c/abs (c/sub % (c/complex 1.0 0.0))) 1e-10)
                   (first (:table ct)))))
       (for [n [3 4 5]]
         (let [ct (reel/character-table (reel/symmetric-group n))]
           (every? #(< (c/abs (c/sub % (c/complex 1.0 0.0))) 1e-10)
                   (first (:table ct)))))
       (for [n [3 5 6 8]]
         (let [ct (reel/character-table (reel/dihedral-group n))]
           (every? #(< (c/abs (c/sub % (c/complex 1.0 0.0))) 1e-10)
                   (first (:table ct))))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Character inner product
;;
;; The character inner product $\langle \chi_i, \chi_j \rangle$ equals
;; the Kronecker delta $\delta_{ij}$. This is a reformulation of row
;; orthogonality normalized by $|G|$.

(let [results
      (for [n [3 4 5]]
        (let [G (reel/symmetric-group n)
              ct (reel/character-table G)
              {:keys [table class-sizes]} ct
              order (reel/order G)
              n-irreps (count table)]
          (every? identity
                  (for [i (range n-irreps) j (range n-irreps)]
                    (let [ip (reel/character-inner-product
                              (nth table i) (nth table j) class-sizes order)
                          expected (if (= i j) 1.0 0.0)]
                      (< (c/abs (c/sub ip (c/complex expected 0.0))) 1e-8))))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Known character tables
;;
;; We verify specific entries against known values.

;; ### $S_3$ character table
;;
;; ||$[1,1,1]$|$[2,1]$|$[3]$|
;; |:--|:--:|:--:|:--:|
;; |$[3]$ (trivial)| 1 | 1 | 1 |
;; |$[2,1]$ (standard)| 2 | 0 | -1 |
;; |$[1,1,1]$ (sign)| 1 | -1 | 1 |

(let [ct (reel/character-table (reel/symmetric-group 3))
      re-table (mapv (fn [row] (mapv #(long (Math/round (c/re %))) row))
                     (:table ct))]
  re-table)

(kind/test-last [= [[1 1 1] [2 0 -1] [1 -1 1]]])

;; ### $S_4$ character table

(let [ct (reel/character-table (reel/symmetric-group 4))
      re-table (mapv (fn [row] (mapv #(long (Math/round (c/re %))) row))
                     (:table ct))]
  re-table)

(kind/test-last
 [= [[1 1 1 1 1]
     [3 1 -1 0 -1]
     [2 0 2 -1 0]
     [3 -1 -1 0 1]
     [1 -1 1 1 -1]]])

;; ### $D_3$ character table
;;
;; $D_3 \cong S_3$, so the tables should match (up to class ordering).

(let [ct-d3 (reel/character-table (reel/dihedral-group 3))
      dims (sort (mapv #(long (Math/round (c/re (first %)))) (:table ct-d3)))
      ct-s3 (reel/character-table (reel/symmetric-group 3))
      dims-s3 (sort (mapv #(long (Math/round (c/re (first %)))) (:table ct-s3)))]
  (= dims dims-s3))

(kind/test-last [true?])

;; ## Murnaghan-Nakayama rule: spot checks
;;
;; The Murnaghan-Nakayama rule computes $\chi_\lambda(\mu)$ recursively
;; by summing over rim hook removals. We verify specific values.

;; $\chi_{[n]}(\mu) = 1$ for all $\mu$ (trivial representation).

(let [results
      (for [n (range 2 8)
            mu (reel/partitions n)]
        (let [ct (reel/character-table (reel/symmetric-group n))
              classes (:classes ct)
              idx (.indexOf classes mu)
              val (c/re (nth (first (:table ct)) idx))]
          (= 1.0 val)))]
  (every? true? results))

(kind/test-last [true?])

;; $\chi_{[1^n]}(\mu) = \text{sign}(\mu)$ (sign representation).
;; The sign of a cycle type $\mu = [\mu_1, \ldots, \mu_k]$ is
;; $(-1)^{n - k}$ where $k$ is the number of parts.

(let [results
      (for [n (range 2 8)
            mu (reel/partitions n)]
        (let [ct (reel/character-table (reel/symmetric-group n))
              sign-label (vec (repeat n 1))
              classes (:classes ct)
              labels (:irrep-labels ct)
              row-idx (.indexOf labels sign-label)
              col-idx (.indexOf classes mu)
              val (long (Math/round (c/re (nth (nth (:table ct) row-idx) col-idx))))
              ;; sign of cycle type: (-1)^(n - number of parts)
              expected (long (Math/pow -1 (- n (count mu))))]
          (= val expected)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Dihedral character table structure
;;
;; For $D_n$ with $n$ odd:
;;
;; - 2 one-dimensional irreps (trivial and sign)
;; - $(n-1)/2$ two-dimensional irreps
;;
;; For $D_n$ with $n$ even:
;;
;; - 4 one-dimensional irreps
;; - $(n/2 - 1)$ two-dimensional irreps

(let [results
      (for [n (range 3 21)]
        (let [ct (reel/character-table (reel/dihedral-group n))
              dims (mapv #(long (Math/round (c/re (first %)))) (:table ct))
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
;; Display the character table of $S_5$ as a formatted table.

(let [ct (reel/character-table (reel/symmetric-group 5))
      {:keys [table classes irrep-labels]} ct]
  (kind/table
   {:column-names (into [""] (mapv str classes))
    :row-vectors (mapv (fn [label row]
                         (into [(str label)]
                               (mapv #(long (Math/round (c/re %))) row)))
                       irrep-labels table)}))

;; ## Summary of verified identities
;;
;; This notebook verified:
;;
;; - **Row orthogonality** for cyclic ($n$ up to 24), symmetric ($n$ up to 6),
;;   dihedral ($n$ up to 24) groups
;; - **Column orthogonality** for all three families
;; - **Dimension-squared sum** $= |G|$ for all three families
;; - **Number of irreps** $=$ number of conjugacy classes
;; - **Trivial character** is all ones
;; - **Character inner product** gives Kronecker delta for $S_3, S_4, S_5$
;; - **Known character tables** for $S_3$, $S_4$, $D_3 \cong S_3$
;; - **MN trivial character**: $\chi_{[n]}(\mu) = 1$ for all $\mu$
;; - **MN sign character**: $\chi_{[1^n]}(\mu) = (-1)^{n-k}$ for all $\mu$
;; - **Dihedral structure**: correct count of 1D and 2D irreps for $n = 3, \ldots, 20$


;; For applications of character tables to random walks, see
;; [Random Transpositions](random_transpositions.html).
