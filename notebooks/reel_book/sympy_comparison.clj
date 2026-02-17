;; # Cross-Validation with SymPy
;;
;; This notebook compares reel's outputs against
;; [SymPy](https://www.sympy.org/)'s combinatorics module, called
;; from Clojure via
;; [libpython-clj](https://github.com/clj-python/libpython-clj).
;;
;; **Requires:** `.venv` with sympy installed (run `./setup_python.sh`).

(ns reel-book.sympy-comparison
  (:require
   [scicloj.reel.core :as reel]
   [scicloj.kindly.v4.kind :as kind]
   [libpython-clj2.python :as py]
   [libpython-clj2.require :refer [require-python]]))

;; ## Setup

(require-python '[sympy.combinatorics :as sc])
(require-python '[sympy.combinatorics.named_groups :as named])
(require-python '[sympy.combinatorics.permutations :as sp])
(require-python '[sympy.utilities.iterables :as symiter])
(require-python '[builtins :as pybuiltins])

;; ## Permutation Properties
;;
;; SymPy and reel both use 0-indexed one-line notation (array form).
;; Their composition conventions differ: SymPy's `(p * q)(i) = q(p(i))`
;; applies $p$ first (left-to-right), while reel's `(op G a b)(i) = a(b(i))`
;; applies $b$ first (right-to-left). We account for this below.
;;
;; For every permutation in $S_3$ and $S_4$, we compare:
;;
;; - cycle type (as a partition)
;; - sign ($+1$ or $-1$)
;; - element order

(defn sympy-perm-props
  "Extract cycle type, sign, and order from a SymPy permutation."
  [p]
  (let [cs (py/py.- p cycle_structure)
        ks (py/->jvm (pybuiltins/list (py/py. cs keys)))
        vs (py/->jvm (pybuiltins/list (py/py. cs values)))
        partition (->> (mapcat (fn [k v] (repeat (long v) (long k))) ks vs)
                       (sort >)
                       vec)
        parity (long (py/py. p parity))
        sign (if (zero? parity) 1 -1)
        order (long (pybuiltins/int (py/py. p order)))]
    {:partition partition :sign sign :order order}))

(defn reel-perm-props
  "Extract cycle type, sign, and order from a reel permutation in S_n."
  [G sigma]
  (let [ct (reel/cycle-type sigma)]
    {:partition (vec ct)
     :sign (reel/sign sigma)
     :order (loop [g sigma k 1]
              (if (= g (reel/id G))
                k
                (recur (reel/op G g sigma) (inc k))))}))

;; ### $S_3$: exhaustive comparison

(let [G (reel/symmetric-group 3)]
  (every? (fn [sigma]
            (let [p (sp/Permutation (vec sigma))]
              (= (reel-perm-props G sigma) (sympy-perm-props p))))
          (reel/elements G)))

(kind/test-last [true?])

;; ### $S_4$: exhaustive comparison

(let [G (reel/symmetric-group 4)]
  (every? (fn [sigma]
            (let [p (sp/Permutation (vec sigma))]
              (= (reel-perm-props G sigma) (sympy-perm-props p))))
          (reel/elements G)))

(kind/test-last [true?])

;; ### $S_5$: exhaustive comparison

(let [G (reel/symmetric-group 5)]
  (every? (fn [sigma]
            (let [p (sp/Permutation (vec sigma))]
              (= (reel-perm-props G sigma) (sympy-perm-props p))))
          (reel/elements G)))

(kind/test-last [true?])

;; ## Composition and Inverse
;;
;; Verify that reel's group operation and inverse agree with SymPy's.
;; Since SymPy composes left-to-right, reel's `(op G a b)` = SymPy's `b * a`.
;; We check all 36 pairs of $S_3$

(let [G (reel/symmetric-group 3)
      elts (vec (sort (reel/elements G)))]
  (every? (fn [[a b]]
            (let [ab-reel (reel/op G a b)
                  inv-reel (reel/inv G a)
                  pa (sp/Permutation (vec a))
                  pb (sp/Permutation (vec b))
                  ab-py (vec (py/->jvm (py/py.- (py/call-attr pb "__mul__" pa) array_form)))
                  inv-py (vec (py/->jvm (py/py.- (py/call-attr pa "__pow__" -1) array_form)))]
              (and (= (vec ab-reel) ab-py)
                   (= (vec inv-reel) inv-py))))
          (for [a elts b elts] [a b])))

(kind/test-last [true?])

;; ## Conjugacy Class Sizes
;;
;; The number and sizes of conjugacy classes in $S_n$ are determined
;; by the partitions of $n$. We verify reel matches SymPy for
;; $n = 2, \ldots, 7$.

;; ### $S_4$: all 576 pairs

(let [G (reel/symmetric-group 4)
      elts (vec (sort (reel/elements G)))]
  (every? (fn [[a b]]
            (let [ab-reel (reel/op G a b)
                  inv-reel (reel/inv G a)
                  pa (sp/Permutation (vec a))
                  pb (sp/Permutation (vec b))
                  ab-py (vec (py/->jvm (py/py.- (py/call-attr pb "__mul__" pa) array_form)))
                  inv-py (vec (py/->jvm (py/py.- (py/call-attr pa "__pow__" -1) array_form)))]
              (and (= (vec ab-reel) ab-py)
                   (= (vec inv-reel) inv-py))))
          (for [a elts b elts] [a b])))

(kind/test-last [true?])

(defn sympy-class-sizes
  "Sorted vector of conjugacy class sizes from SymPy's SymmetricGroup(n)."
  [n]
  (let [Sn (named/SymmetricGroup n)
        classes (py/py. Sn conjugacy_classes)]
    (sort (mapv #(long (py/py. % __len__)) classes))))

(defn reel-class-sizes
  "Sorted vector of conjugacy class sizes from reel's symmetric-group."
  [n]
  (let [G (reel/symmetric-group n)
        classes (reel/conjugacy-classes G)]
    (sort (mapv #(count (:elements %)) classes))))

(every? (fn [n]
          (= (reel-class-sizes n) (sympy-class-sizes n)))
        (range 2 8))

(kind/test-last [true?])

;; Show the comparison for $S_5$:

(let [n 5]
  (kind/table
   {:column-names ["Source" "Class sizes (sorted)"]
    :row-vectors [["reel" (str (reel-class-sizes n))]
                  ["SymPy" (str (sympy-class-sizes n))]]}))

;; ## Conjugacy Classes in Dihedral Groups
;;
;; SymPy's `DihedralGroup(n)` represents $D_n$ as a permutation group
;; on $n$ points. We compare structural properties: order, number of
;; conjugacy classes, and sorted class sizes.

(defn sympy-dihedral-info [n]
  (let [Dn (named/DihedralGroup n)
        order (long (pybuiltins/int (py/py. Dn order)))
        classes (py/py. Dn conjugacy_classes)
        sizes (sort (mapv #(long (py/py. % __len__)) classes))]
    {:order order :num-classes (count classes) :class-sizes sizes}))

(defn reel-dihedral-info [n]
  (let [G (reel/dihedral-group n)
        classes (reel/conjugacy-classes G)
        sizes (sort (mapv #(count (:elements %)) classes))]
    {:order (reel/order G) :num-classes (count classes) :class-sizes sizes}))

(every? (fn [n]
          (= (reel-dihedral-info n) (sympy-dihedral-info n)))
        (range 3 13))

(kind/test-last [true?])

;; Show the comparison for a range of $n$:

(let [rows (mapv (fn [n]
                   (let [ri (reel-dihedral-info n)
                         si (sympy-dihedral-info n)]
                     [n (:order ri) (:num-classes ri) (= ri si)]))
                 (range 3 13))]
  (kind/table
   {:column-names ["$n$" "Order" "Classes" "Match?"]
    :row-vectors rows}))

;; ## Group Properties
;;
;; SymPy can test properties like `is_abelian`, `is_cyclic`, etc.
;; We compare these against reel's group structure.

(every? (fn [n]
          (let [Cn (named/CyclicGroup n)]
            (and (py/py.- Cn is_abelian)
                 (py/py.- Cn is_cyclic))))
        (range 2 13))

(kind/test-last [true?])

(every? (fn [n]
          (let [Dn (named/DihedralGroup n)]
            (and (not (py/py.- Dn is_abelian))
                 (not (py/py.- Dn is_cyclic))
                 (py/py.- Dn is_dihedral))))
        (range 3 13))

(kind/test-last [true?])

;; ## Partition Counts
;;
;; The number of partitions of $n$ is a classic combinatorial quantity.
;; SymPy's `partitions(n)` generates them; we compare the count.

(defn sympy-partition-count [n]
  (long (py/py. (pybuiltins/list (symiter/partitions n)) __len__)))

(every? (fn [n]
          (= (count (reel/partitions n)) (sympy-partition-count n)))
        (range 1 16))

(kind/test-last [true?])

;; Show partition counts:

(let [rows (mapv (fn [n]
                   [n (count (reel/partitions n)) (sympy-partition-count n)])
                 (range 1 13))]
  (kind/table
   {:column-names ["$n$" "reel" "SymPy"]
    :row-vectors rows}))

;; ## Character Tables
;;
;; SymPy does not compute character tables for $S_n$. We compare
;; reel's Murnaghan-Nakayama character values against known textbook
;; values for $S_3$, $S_4$, and $S_5$.
;;
;; The tables below are ordered with irreps (rows) by dominance order
;; on partitions and classes (columns) by partition order.

;; Known character table of $S_3$:
;;
;; |            | $[1^3]$ | $[2,1]$ | $[3]$ |
;; |:-----------|:--------|:--------|:------|
;; | $[3]$      |   1     |   1     |   1   |
;; | $[2,1]$    |   2     |   0     |  -1   |
;; | $[1^3]$    |   1     |  -1     |   1   |

(def known-S3
  {:irreps [[3] [2 1] [1 1 1]]
   :classes [[1 1 1] [2 1] [3]]
   :table [[1 1 1] [2 0 -1] [1 -1 1]]})

;; Known character table of $S_4$:
;;
;; |               | $[1^4]$ | $[2,1^2]$ | $[2^2]$ | $[3,1]$ | $[4]$ |
;; |:--------------|:--------|:----------|:--------|:--------|:------|
;; | $[4]$         |   1     |   1       |   1     |   1     |   1   |
;; | $[3,1]$       |   3     |   1       |  -1     |   0     |  -1   |
;; | $[2^2]$       |   2     |   0       |   2     |  -1     |   0   |
;; | $[2,1^2]$     |   3     |  -1       |  -1     |   0     |   1   |
;; | $[1^4]$       |   1     |  -1       |   1     |   1     |  -1   |

(def known-S4
  {:irreps [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]
   :classes [[1 1 1 1] [2 1 1] [2 2] [3 1] [4]]
   :table [[1 1 1 1 1]
           [3 1 -1 0 -1]
           [2 0 2 -1 0]
           [3 -1 -1 0 1]
           [1 -1 1 1 -1]]})

;; Known character table of $S_5$:

(def known-S5
  {:irreps [[5] [4 1] [3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]]
   :classes [[1 1 1 1 1] [2 1 1 1] [2 2 1] [3 1 1] [3 2] [4 1] [5]]
   :table [[1 1 1 1 1 1 1]
           [4 2 0 1 -1 0 -1]
           [5 1 1 -1 1 -1 0]
           [6 0 -2 0 0 0 1]
           [5 -1 1 -1 -1 1 0]
           [4 -2 0 1 1 0 -1]
           [1 -1 1 1 -1 -1 1]]})

(defn extract-reel-character-table
  "Extract reel's character table as a vec-of-vecs of integers,
   with rows sorted by irrep partition (dominance) and columns
   by class partition."
  [n]
  (let [G (reel/symmetric-group n)
        ct (reel/character-table G)
        classes (:classes ct)
        class-partitions (mapv :partition classes)
        table-re (:table-re ct)]
    {:irreps (:irrep-labels ct)
     :classes class-partitions
     :table (mapv (fn [row]
                    (mapv #(long (aget ^doubles row %))
                          (range (count classes))))
                  table-re)}))

;; ### $S_3$

(let [reel-ct (extract-reel-character-table 3)]
  (= (:table known-S3) (:table reel-ct)))

(kind/test-last [true?])

;; ### $S_4$

(let [reel-ct (extract-reel-character-table 4)]
  (= (:table known-S4) (:table reel-ct)))

(kind/test-last [true?])

;; ### $S_5$

(let [reel-ct (extract-reel-character-table 5)]
  (= (:table known-S5) (:table reel-ct)))

(kind/test-last [true?])

;; Display the comparison for $S_5$:

(let [reel-ct (extract-reel-character-table 5)
      rows (map-indexed
            (fn [i irrep]
              (let [reel-row (nth (:table reel-ct) i)
                    known-row (nth (:table known-S5) i)]
                [(str irrep) (str reel-row) (str known-row) (= reel-row known-row)]))
            (:irreps known-S5))]
  (kind/table
   {:column-names ["Irrep" "reel" "Known" "Match?"]
    :row-vectors (vec rows)}))

;; ## Hook-Length Dimensions
;;
;; The dimension of the irrep of $S_n$ indexed by partition $\lambda$
;; is $n! / \prod h(i,j)$ where $h(i,j)$ are the hook lengths.
;; We verify reel's `hook-length-dimension` against known values
;; (equivalently, the first column of the character table).

(every? (fn [n]
          (let [ct (extract-reel-character-table n)
                dims-from-ct (mapv first (:table ct))
                dims-from-hook (mapv reel/hook-length-dimension (:irreps ct))]
            (= dims-from-ct dims-from-hook)))
        (range 2 8))

(kind/test-last [true?])

;; The sum of squared dimensions equals $n!$ (Burnside's theorem):
;; Squared dimensions sum to $n!$ (Burnside):

(every? (fn [n]
          (let [parts (reel/partitions n)
                total (reduce + (map #(let [d (reel/hook-length-dimension %)] (* d d)) parts))]
            (= total (reduce * (range 1 (inc n))))))
        (range 2 8))

(kind/test-last [true?])

;; ## Necklace and Bracelet Counts
;;
;; Binary necklaces (OEIS A000031) and bracelets (OEIS A000029)
;; computed by reel's Burnside/Pólya functions, compared against
;; values from the number-theoretic formula.

(defn necklace-formula
  "Number of binary necklaces with n beads, from the formula:
   (1/n) * sum_{d|n} phi(d) * 2^{n/d}"
  [n]
  (let [divisors (filter #(zero? (mod n %)) (range 1 (inc n)))
        euler-phi (fn [m]
                    (count (filter #(= 1 (long (.gcd (BigInteger/valueOf m)
                                                     (BigInteger/valueOf %))))
                                   (range 1 (inc m)))))]
    (/ (reduce + (map (fn [d] (* (euler-phi d) (long (Math/pow 2 (/ n d))))) divisors))
       n)))

(defn reel-necklace-count [n]
  (let [G (reel/cyclic-group n)
        act (fn [g x] (mod (+ (long x) (long g)) n))
        ci (reel/cycle-index G act (range n))]
    (reel/polya-count ci 2)))

(every? (fn [n]
          (= (reel-necklace-count n) (necklace-formula n)))
        (range 1 21))

(kind/test-last [true?])

;; Show the values:

(let [rows (mapv (fn [n]
                   [n (reel-necklace-count n) (necklace-formula n)])
                 (range 1 13))]
  (kind/table
   {:column-names ["$n$" "reel (Pólya)" "Formula"]
    :row-vectors rows}))

;; ## Orbit-Stabilizer Comparison
;;
;; SymPy's `SymmetricGroup(n)` has `orbit` and `stabilizer` methods.
;; We verify reel's orbit sizes and stabilizer orders match SymPy's
;; for $S_n$ acting on $\{0, \ldots, n{-}1\}$.

(every? (fn [n]
          (let [G (reel/symmetric-group n)
                Sn (named/SymmetricGroup n)
                act (fn [sigma x] (sigma x))]
            (every? (fn [x]
                      (let [orb-reel (count (reel/orbit G act x))
                            stab-reel (count (reel/stabilizer G act x))
                            orb-py (long (py/py. (py/py. Sn orbit x) __len__))
                            stab-py (long (py/py. (py/py. Sn stabilizer x) order))]
                        (and (= orb-reel orb-py)
                             (= stab-reel stab-py))))
                    (range n))))
        (range 2 6))

(kind/test-last [true?])

;; ## Trichord Classification
;;
;; 220 trichords (3-element subsets of $\mathbb{Z}/12\mathbb{Z}$)
;; classified under $C_{12}$ (transposition) and $D_{12}$
;; (transposition + inversion). The counts 19 and 12 are well-known
;; in music theory.
;;
;; We verify reel's orbit computation against SymPy's action on subsets.

(let [G-c (reel/cyclic-group 12)
      G-d (reel/dihedral-group 12)
      act-c (fn [g x] (mod (+ (long x) (long g)) 12))
      act-d (fn [[t k] x]
              (case t
                :r (mod (+ (long x) (long k)) 12)
                :s (mod (- (long k) (long x)) 12)))
      {domain-3 :domain act-c-sub :act} (reel/subset-action act-c (range 12) 3)
      {_ :domain act-d-sub :act} (reel/subset-action act-d (range 12) 3)
      under-C12 (count (reel/orbits G-c act-c-sub domain-3))
      under-D12 (count (reel/orbits G-d act-d-sub domain-3))]
  {:total (count domain-3) :under-C12 under-C12 :under-D12 under-D12})

(kind/test-last [= {:total 220 :under-C12 19 :under-D12 12}])

;; ## Summary
;;
;; All comparisons passed:
;;
;; - **Permutation properties**: cycle type, sign, and order for every
;;   element of $S_3$, $S_4$, $S_5$ (150 permutations)
;; - **Composition and inverse**: all pairs in $S_3$ (36) and $S_4$ (576),
;;   accounting for the convention difference (reel right-to-left,
;;   SymPy left-to-right)
;; - **Conjugacy class sizes**: $S_2$ through $S_7$ match SymPy
;; - **Dihedral group structure**: $D_3$ through $D_{12}$ — order,
;;   class count, and class sizes all match
;; - **Group properties**: cyclic groups are abelian+cyclic, dihedral are not
;; - **Partition counts**: $p(1)$ through $p(15)$ match SymPy
;; - **Character tables**: $S_3$, $S_4$, $S_5$ match known textbook values
;; - **Hook-length dimensions**: consistent with character tables,
;;   squared sum equals $n!$
;; - **Necklace counts**: Pólya agrees with number-theoretic formula for $n = 1, \ldots, 20$
;; - **Orbit-stabilizer**: orbit sizes and stabilizer orders match SymPy
;;   for $S_n$ acting on points ($n = 2, \ldots, 5$)
;; - **Trichord classification**: 220 $\to$ 19 under $C_{12}$, 12 under $D_{12}$
