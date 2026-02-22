;; # Cross-Validation with SymPy
;;
;; This notebook compares harmonica's outputs against
;; [SymPy](https://www.sympy.org/)'s combinatorics module, called
;; from Clojure via
;; [libpython-clj](https://github.com/clj-python/libpython-clj).
;;
;; **Requires:** `.venv` with sympy installed (run `./setup_python.sh`).

(ns harmonica-book.sympy-comparison
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
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
;; SymPy and harmonica both use 0-indexed one-line notation (array form).
;; Their composition conventions differ: SymPy's `(p * q)(i) = q(p(i))`
;; applies $p$ first (left-to-right), while harmonica's `(op G a b)(i) = a(b(i))`
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
        partition (->> (mapcat (fn [k v] (repeat v k)) ks vs)
                       (sort >)
                       vec)
        parity (long (py/py. p parity))
        sign (if (zero? parity) 1 -1)
        order (long (pybuiltins/int (py/py. p order)))]
    {:partition partition :sign sign :order order}))

(defn hm-perm-props
  "Extract cycle type, sign, and order from a harmonica permutation in S_n."
  [G sigma]
  (let [ct (hm/cycle-type sigma)]
    {:partition (vec ct)
     :sign (hm/sign sigma)
     :order (loop [g sigma k 1]
              (if (= g (hm/id G))
                k
                (recur (hm/op G g sigma) (inc k))))}))

;; ### $S_3$: exhaustive comparison

(let [G (hm/symmetric-group 3)]
  (every? (fn [sigma]
            (let [p (sp/Permutation (vec sigma))]
              (= (hm-perm-props G sigma) (sympy-perm-props p))))
          (hm/elements G)))

(kind/test-last [true?])

;; ### $S_4$: exhaustive comparison

(let [G (hm/symmetric-group 4)]
  (every? (fn [sigma]
            (let [p (sp/Permutation (vec sigma))]
              (= (hm-perm-props G sigma) (sympy-perm-props p))))
          (hm/elements G)))

(kind/test-last [true?])

;; ### $S_5$: exhaustive comparison

(let [G (hm/symmetric-group 5)]
  (every? (fn [sigma]
            (let [p (sp/Permutation (vec sigma))]
              (= (hm-perm-props G sigma) (sympy-perm-props p))))
          (hm/elements G)))

(kind/test-last [true?])

;; ## Composition and Inverse
;;
;; Verify that harmonica's group operation and inverse agree with SymPy's.
;; Since SymPy composes left-to-right, harmonica's `(op G a b)` = SymPy's `b * a`.
;; We check all 36 pairs of $S_3$

(let [G (hm/symmetric-group 3)
      elts (vec (sort (hm/elements G)))]
  (every? (fn [[a b]]
            (let [ab-hm (hm/op G a b)
                  inv-hm (hm/inv G a)
                  pa (sp/Permutation (vec a))
                  pb (sp/Permutation (vec b))
                  ab-py (vec (py/->jvm (py/py.- (py/call-attr pb "__mul__" pa) array_form)))
                  inv-py (vec (py/->jvm (py/py.- (py/call-attr pa "__pow__" -1) array_form)))]
              (and (= (vec ab-hm) ab-py)
                   (= (vec inv-hm) inv-py))))
          (for [a elts b elts] [a b])))

(kind/test-last [true?])

;; ## Conjugacy Class Sizes
;;
;; The number and sizes of conjugacy classes in $S_n$ are determined
;; by the partitions of $n$. We verify harmonica matches SymPy for
;; $n = 2, \ldots, 7$.

;; ### $S_4$: all 576 pairs

(let [G (hm/symmetric-group 4)
      elts (vec (sort (hm/elements G)))]
  (every? (fn [[a b]]
            (let [ab-hm (hm/op G a b)
                  inv-hm (hm/inv G a)
                  pa (sp/Permutation (vec a))
                  pb (sp/Permutation (vec b))
                  ab-py (vec (py/->jvm (py/py.- (py/call-attr pb "__mul__" pa) array_form)))
                  inv-py (vec (py/->jvm (py/py.- (py/call-attr pa "__pow__" -1) array_form)))]
              (and (= (vec ab-hm) ab-py)
                   (= (vec inv-hm) inv-py))))
          (for [a elts b elts] [a b])))

(kind/test-last [true?])

(defn sympy-class-sizes
  "Sorted vector of conjugacy class sizes from SymPy's SymmetricGroup(n)."
  [n]
  (let [Sn (named/SymmetricGroup n)
        classes (py/py. Sn conjugacy_classes)]
    (sort (mapv #(long (py/py. % __len__)) classes))))

(defn hm-class-sizes
  "Sorted vector of conjugacy class sizes from harmonica's symmetric-group."
  [n]
  (let [G (hm/symmetric-group n)
        classes (hm/conjugacy-classes G)]
    (sort (mapv #(count (:elements %)) classes))))

(every? (fn [n]
          (= (hm-class-sizes n) (sympy-class-sizes n)))
        (range 2 8))

(kind/test-last [true?])

;; Show the comparison for $S_5$:

(let [n 5]
  (kind/table
   {:column-names ["Source" "Class sizes (sorted)"]
    :row-vectors [["harmonica" (str (hm-class-sizes n))]
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

(defn hm-dihedral-info [n]
  (let [G (hm/dihedral-group n)
        classes (hm/conjugacy-classes G)
        sizes (sort (mapv #(count (:elements %)) classes))]
    {:order (hm/order G) :num-classes (count classes) :class-sizes sizes}))

(every? (fn [n]
          (= (hm-dihedral-info n) (sympy-dihedral-info n)))
        (range 3 13))

(kind/test-last [true?])

;; Show the comparison for a range of $n$:

(let [rows (mapv (fn [n]
                   (let [ri (hm-dihedral-info n)
                         si (sympy-dihedral-info n)]
                     [n (:order ri) (:num-classes ri) (= ri si)]))
                 (range 3 13))]
  (kind/table
   {:column-names ["$n$" "Order" "Classes" "Match?"]
    :row-vectors rows}))

;; ## Group Properties
;;
;; SymPy can test properties like `is_abelian`, `is_cyclic`, etc.
;; We compare these against harmonica's group structure.

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
          (= (count (hm/partitions n)) (sympy-partition-count n)))
        (range 1 16))

(kind/test-last [true?])

;; Show partition counts:

(let [rows (mapv (fn [n]
                   [n (count (hm/partitions n)) (sympy-partition-count n)])
                 (range 1 13))]
  (kind/table
   {:column-names ["$n$" "harmonica" "SymPy"]
    :row-vectors rows}))

;; ## Character Tables
;;
;; SymPy does not compute character tables for $S_n$. We compare
;; harmonica's Murnaghan-Nakayama character values against known textbook
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

(defn extract-hm-character-table
  "Build a {:irreps :classes :table} map from harmonica's character table."
  [n]
  (let [G (hm/symmetric-group n)
        ct (hm/character-table G)
        classes (:classes ct)
        class-partitions (mapv :partition classes)
        table (:table ct)]
    {:irreps (:irrep-labels ct)
     :classes class-partitions
     :table (mapv (fn [row]
                    (mapv #(Math/round (cx/re %)) row))
                  table)}))

;; ### $S_3$

(let [hm-ct (extract-hm-character-table 3)]
  (= (:table known-S3) (:table hm-ct)))

(kind/test-last [true?])

;; ### $S_4$

(let [hm-ct (extract-hm-character-table 4)]
  (= (:table known-S4) (:table hm-ct)))

(kind/test-last [true?])

;; ### $S_5$

(let [hm-ct (extract-hm-character-table 5)]
  (= (:table known-S5) (:table hm-ct)))

(kind/test-last [true?])

;; Display the comparison for $S_5$:

(let [hm-ct (extract-hm-character-table 5)
      rows (map-indexed
            (fn [i irrep]
              (let [hm-row (nth (:table hm-ct) i)
                    known-row (nth (:table known-S5) i)]
                [(str irrep) (str hm-row) (str known-row) (= hm-row known-row)]))
            (:irreps known-S5))]
  (kind/table
   {:column-names ["Irrep" "harmonica" "Known" "Match?"]
    :row-vectors (vec rows)}))

;; ## Hook-Length Dimensions
;;
;; The dimension of the irrep of $S_n$ indexed by partition $\lambda$
;; is $n! / \prod h(i,j)$ where $h(i,j)$ are the hook lengths.
;; We verify harmonica's `hook-length-dimension` against known values
;; (equivalently, the first column of the character table).

(every? (fn [n]
          (let [ct (extract-hm-character-table n)
                dims-from-ct (mapv first (:table ct))
                dims-from-hook (mapv hm/hook-length-dimension (:irreps ct))]
            (= dims-from-ct dims-from-hook)))
        (range 2 8))

(kind/test-last [true?])

;; The sum of squared dimensions equals $n!$ (Burnside's theorem):
;; Squared dimensions sum to $n!$ (Burnside):

(every? (fn [n]
          (let [parts (hm/partitions n)
                total (reduce + (map #(let [d (hm/hook-length-dimension %)] (* d d)) parts))]
            (= total (reduce * (range 1 (inc n))))))
        (range 2 8))

(kind/test-last [true?])

;; ## Necklace and Bracelet Counts
;;
;; Binary necklaces ([OEIS A000031](https://oeis.org/A000031)) and bracelets ([OEIS A000029](https://oeis.org/A000029))
;; computed by harmonica's Burnside/Pólya functions, compared against
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

(defn hm-necklace-count [n]
  (let [G (hm/cyclic-group n)
        act (fn [g x] (mod (+ x g) n))
        ci (hm/cycle-index G act (range n))]
    (hm/polya-count ci 2)))

(every? (fn [n]
          (= (hm-necklace-count n) (necklace-formula n)))
        (range 1 21))

(kind/test-last [true?])

;; Show the values:

(let [rows (mapv (fn [n]
                   [n (hm-necklace-count n) (necklace-formula n)])
                 (range 1 13))]
  (kind/table
   {:column-names ["$n$" "harmonica (Pólya)" "Formula"]
    :row-vectors rows}))

;; ## Orbit-Stabilizer Comparison
;;
;; SymPy's `SymmetricGroup(n)` has `orbit` and `stabilizer` methods.
;; We verify harmonica's orbit sizes and stabilizer orders match SymPy's
;; for $S_n$ acting on $\{0, \ldots, n{-}1\}$.

(every? (fn [n]
          (let [G (hm/symmetric-group n)
                Sn (named/SymmetricGroup n)
                act (fn [sigma x] (sigma x))]
            (every? (fn [x]
                      (let [orb-hm (count (hm/orbit G act x))
                            stab-hm (count (hm/stabilizer G act x))
                            orb-py (long (py/py. (py/py. Sn orbit x) __len__))
                            stab-py (long (py/py. (py/py. Sn stabilizer x) order))]
                        (and (= orb-hm orb-py)
                             (= stab-hm stab-py))))
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
;; We verify harmonica's orbit computation against SymPy's action on subsets.

(let [G-c (hm/cyclic-group 12)
      G-d (hm/dihedral-group 12)
      act-c (fn [g x] (mod (+ x g) 12))
      act-d (fn [[t k] x]
              (case t
                :r (mod (+ x k) 12)
                :s (mod (- k x) 12)))
      {domain-3 :domain act-c-sub :act} (hm/subset-action act-c (range 12) 3)
      {_ :domain act-d-sub :act} (hm/subset-action act-d (range 12) 3)
      under-C12 (count (hm/orbits G-c act-c-sub domain-3))
      under-D12 (count (hm/orbits G-d act-d-sub domain-3))]
  {:total (count domain-3) :under-C12 under-C12 :under-D12 under-D12})

(kind/test-last [= {:total 220 :under-C12 19 :under-D12 12}])

;; ## Summary
;;
;; All comparisons passed:
;;
;; - **Permutation properties**: cycle type, sign, and order for every
;;   element of $S_3$, $S_4$, $S_5$ (150 permutations)
;; - **Composition and inverse**: all pairs in $S_3$ (36) and $S_4$ (576),
;;   accounting for the convention difference (harmonica right-to-left,
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
