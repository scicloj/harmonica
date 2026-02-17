(ns
 reel-book.sympy-comparison-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.kindly.v4.kind :as kind]
  [libpython-clj2.python :as py]
  [libpython-clj2.require :refer [require-python]]
  [clojure.test :refer [deftest is]]))


(def v3_l19 (require-python '[sympy.combinatorics :as sc]))


(def
 v4_l20
 (require-python '[sympy.combinatorics.named_groups :as named]))


(def v5_l21 (require-python '[sympy.combinatorics.permutations :as sp]))


(def v6_l22 (require-python '[sympy.utilities.iterables :as symiter]))


(def v7_l23 (require-python '[builtins :as pybuiltins]))


(def
 v9_l38
 (defn
  sympy-perm-props
  "Extract cycle type, sign, and order from a SymPy permutation."
  [p]
  (let
   [cs
    (py/py.- p cycle_structure)
    ks
    (py/->jvm (pybuiltins/list (py/py. cs keys)))
    vs
    (py/->jvm (pybuiltins/list (py/py. cs values)))
    partition
    (->>
     (mapcat (fn [k v] (repeat (long v) (long k))) ks vs)
     (sort >)
     vec)
    parity
    (long (py/py. p parity))
    sign
    (if (zero? parity) 1 -1)
    order
    (long (pybuiltins/int (py/py. p order)))]
   {:partition partition, :sign sign, :order order})))


(def
 v10_l52
 (defn
  reel-perm-props
  "Extract cycle type, sign, and order from a reel permutation in S_n."
  [G sigma]
  (let
   [ct (reel/cycle-type sigma)]
   {:partition (vec ct),
    :sign (reel/sign sigma),
    :order
    (loop
     [g sigma k 1]
     (if (= g (reel/id G)) k (recur (reel/op G g sigma) (inc k))))})))


(def
 v12_l65
 (let
  [G (reel/symmetric-group 3)]
  (every?
   (fn
    [sigma]
    (let
     [p (sp/Permutation (vec sigma))]
     (= (reel-perm-props G sigma) (sympy-perm-props p))))
   (reel/elements G))))


(deftest t13_l71 (is (true? v12_l65)))


(def
 v15_l75
 (let
  [G (reel/symmetric-group 4)]
  (every?
   (fn
    [sigma]
    (let
     [p (sp/Permutation (vec sigma))]
     (= (reel-perm-props G sigma) (sympy-perm-props p))))
   (reel/elements G))))


(deftest t16_l81 (is (true? v15_l75)))


(def
 v18_l85
 (let
  [G (reel/symmetric-group 5)]
  (every?
   (fn
    [sigma]
    (let
     [p (sp/Permutation (vec sigma))]
     (= (reel-perm-props G sigma) (sympy-perm-props p))))
   (reel/elements G))))


(deftest t19_l91 (is (true? v18_l85)))


(def
 v21_l99
 (let
  [G (reel/symmetric-group 3) elts (vec (sort (reel/elements G)))]
  (every?
   (fn
    [[a b]]
    (let
     [ab-reel
      (reel/op G a b)
      inv-reel
      (reel/inv G a)
      pa
      (sp/Permutation (vec a))
      pb
      (sp/Permutation (vec b))
      ab-py
      (vec
       (py/->jvm (py/py.- (py/call-attr pb "__mul__" pa) array_form)))
      inv-py
      (vec
       (py/->jvm (py/py.- (py/call-attr pa "__pow__" -1) array_form)))]
     (and (= (vec ab-reel) ab-py) (= (vec inv-reel) inv-py))))
   (for [a elts b elts] [a b]))))


(deftest t22_l112 (is (true? v21_l99)))


(def
 v24_l120
 (defn
  sympy-class-sizes
  "Sorted vector of conjugacy class sizes from SymPy's SymmetricGroup(n)."
  [n]
  (let
   [Sn (named/SymmetricGroup n) classes (py/py. Sn conjugacy_classes)]
   (sort
    (mapv
     (fn* [p1__78098#] (long (py/py. p1__78098# __len__)))
     classes)))))


(def
 v25_l127
 (defn
  reel-class-sizes
  "Sorted vector of conjugacy class sizes from reel's symmetric-group."
  [n]
  (let
   [G (reel/symmetric-group n) classes (reel/conjugacy-classes G)]
   (sort
    (mapv (fn* [p1__78099#] (count (:elements p1__78099#))) classes)))))


(def
 v26_l134
 (every?
  (fn [n] (= (reel-class-sizes n) (sympy-class-sizes n)))
  (range 2 8)))


(deftest t27_l138 (is (true? v26_l134)))


(def
 v29_l142
 (let
  [n 5]
  (kind/table
   {:column-names ["Source" "Class sizes (sorted)"],
    :row-vectors
    [["reel" (str (reel-class-sizes n))]
     ["SymPy" (str (sympy-class-sizes n))]]})))


(def
 v31_l154
 (defn
  sympy-dihedral-info
  [n]
  (let
   [Dn
    (named/DihedralGroup n)
    order
    (long (pybuiltins/int (py/py. Dn order)))
    classes
    (py/py. Dn conjugacy_classes)
    sizes
    (sort
     (mapv
      (fn* [p1__78100#] (long (py/py. p1__78100# __len__)))
      classes))]
   {:order order, :num-classes (count classes), :class-sizes sizes})))


(def
 v32_l161
 (defn
  reel-dihedral-info
  [n]
  (let
   [G
    (reel/dihedral-group n)
    classes
    (reel/conjugacy-classes G)
    sizes
    (sort
     (mapv (fn* [p1__78101#] (count (:elements p1__78101#))) classes))]
   {:order (reel/order G),
    :num-classes (count classes),
    :class-sizes sizes})))


(def
 v33_l167
 (every?
  (fn [n] (= (reel-dihedral-info n) (sympy-dihedral-info n)))
  (range 3 13)))


(deftest t34_l171 (is (true? v33_l167)))


(def
 v36_l175
 (let
  [rows
   (mapv
    (fn
     [n]
     (let
      [ri (reel-dihedral-info n) si (sympy-dihedral-info n)]
      [n (:order ri) (:num-classes ri) (= ri si)]))
    (range 3 13))]
  (kind/table
   {:column-names ["$n$" "Order" "Classes" "Match?"],
    :row-vectors rows})))


(def
 v38_l189
 (every?
  (fn
   [n]
   (let
    [Cn (named/CyclicGroup n)]
    (and (py/py.- Cn is_abelian) (py/py.- Cn is_cyclic))))
  (range 2 13)))


(deftest t39_l195 (is (true? v38_l189)))


(def
 v40_l197
 (every?
  (fn
   [n]
   (let
    [Dn (named/DihedralGroup n)]
    (and
     (not (py/py.- Dn is_abelian))
     (not (py/py.- Dn is_cyclic))
     (py/py.- Dn is_dihedral))))
  (range 3 13)))


(deftest t41_l204 (is (true? v40_l197)))


(def
 v43_l211
 (defn
  sympy-partition-count
  [n]
  (long (py/py. (pybuiltins/list (symiter/partitions n)) __len__))))


(def
 v44_l214
 (every?
  (fn [n] (= (count (reel/partitions n)) (sympy-partition-count n)))
  (range 1 16)))


(deftest t45_l218 (is (true? v44_l214)))


(def
 v47_l222
 (let
  [rows
   (mapv
    (fn [n] [n (count (reel/partitions n)) (sympy-partition-count n)])
    (range 1 13))]
  (kind/table
   {:column-names ["$n$" "reel" "SymPy"], :row-vectors rows})))


(def
 v49_l246
 (def
  known-S3
  {:irreps [[3] [2 1] [1 1 1]],
   :classes [[1 1 1] [2 1] [3]],
   :table [[1 1 1] [2 0 -1] [1 -1 1]]}))


(def
 v51_l261
 (def
  known-S4
  {:irreps [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]],
   :classes [[1 1 1 1] [2 1 1] [2 2] [3 1] [4]],
   :table
   [[1 1 1 1 1]
    [3 1 -1 0 -1]
    [2 0 2 -1 0]
    [3 -1 -1 0 1]
    [1 -1 1 1 -1]]}))


(def
 v53_l272
 (def
  known-S5
  {:irreps [[5] [4 1] [3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]],
   :classes [[1 1 1 1 1] [2 1 1 1] [2 2 1] [3 1 1] [3 2] [4 1] [5]],
   :table
   [[1 1 1 1 1 1 1]
    [4 2 0 1 -1 0 -1]
    [5 1 1 -1 1 -1 0]
    [6 0 -2 0 0 0 1]
    [5 -1 1 -1 -1 1 0]
    [4 -2 0 1 1 0 -1]
    [1 -1 1 1 -1 -1 1]]}))


(def
 v54_l283
 (defn
  extract-reel-character-table
  "Extract reel's character table as a vec-of-vecs of integers,\n   with rows sorted by irrep partition (dominance) and columns\n   by class partition."
  [n]
  (let
   [G
    (reel/symmetric-group n)
    ct
    (reel/character-table G)
    classes
    (:classes ct)
    class-partitions
    (mapv :partition classes)
    table-re
    (:table-re ct)]
   {:irreps (:irrep-labels ct),
    :classes class-partitions,
    :table
    (mapv
     (fn
      [row]
      (mapv
       (fn* [p1__78102#] (long (aget row p1__78102#)))
       (range (count classes))))
     table-re)})))


(def
 v56_l302
 (let
  [reel-ct (extract-reel-character-table 3)]
  (= (:table known-S3) (:table reel-ct))))


(deftest t57_l305 (is (true? v56_l302)))


(def
 v59_l309
 (let
  [reel-ct (extract-reel-character-table 4)]
  (= (:table known-S4) (:table reel-ct))))


(deftest t60_l312 (is (true? v59_l309)))


(def
 v62_l316
 (let
  [reel-ct (extract-reel-character-table 5)]
  (= (:table known-S5) (:table reel-ct))))


(deftest t63_l319 (is (true? v62_l316)))


(def
 v65_l323
 (let
  [reel-ct
   (extract-reel-character-table 5)
   rows
   (map-indexed
    (fn
     [i irrep]
     (let
      [reel-row
       (nth (:table reel-ct) i)
       known-row
       (nth (:table known-S5) i)]
      [(str irrep)
       (str reel-row)
       (str known-row)
       (= reel-row known-row)]))
    (:irreps known-S5))]
  (kind/table
   {:column-names ["Irrep" "reel" "Known" "Match?"],
    :row-vectors (vec rows)})))


(def
 v67_l341
 (every?
  (fn
   [n]
   (let
    [ct
     (extract-reel-character-table n)
     dims-from-ct
     (mapv first (:table ct))
     dims-from-hook
     (mapv reel/hook-length-dimension (:irreps ct))]
    (= dims-from-ct dims-from-hook)))
  (range 2 8)))


(deftest t68_l348 (is (true? v67_l341)))


(def
 v70_l353
 (every?
  (fn
   [n]
   (let
    [parts
     (reel/partitions n)
     total
     (reduce
      +
      (map
       (fn*
        [p1__78103#]
        (let [d (reel/hook-length-dimension p1__78103#)] (* d d)))
       parts))]
    (= total (reduce * (range 1 (inc n))))))
  (range 2 8)))


(deftest t71_l359 (is (true? v70_l353)))


(def
 v73_l367
 (defn
  necklace-formula
  "Number of binary necklaces with n beads, from the formula:\n   (1/n) * sum_{d|n} phi(d) * 2^{n/d}"
  [n]
  (let
   [divisors
    (filter
     (fn* [p1__78104#] (zero? (mod n p1__78104#)))
     (range 1 (inc n)))
    euler-phi
    (fn
     [m]
     (count
      (filter
       (fn*
        [p1__78105#]
        (=
         1
         (long
          (.gcd
           (BigInteger/valueOf m)
           (BigInteger/valueOf p1__78105#)))))
       (range 1 (inc m)))))]
   (/
    (reduce
     +
     (map
      (fn [d] (* (euler-phi d) (long (Math/pow 2 (/ n d)))))
      divisors))
    n))))


(def
 v74_l379
 (defn
  reel-necklace-count
  [n]
  (let
   [G
    (reel/cyclic-group n)
    act
    (fn [g x] (mod (+ (long x) (long g)) n))
    ci
    (reel/cycle-index G act (range n))]
   (reel/polya-count ci 2))))


(def
 v75_l385
 (every?
  (fn [n] (= (reel-necklace-count n) (necklace-formula n)))
  (range 1 21)))


(deftest t76_l389 (is (true? v75_l385)))


(def
 v78_l393
 (let
  [rows
   (mapv
    (fn [n] [n (reel-necklace-count n) (necklace-formula n)])
    (range 1 13))]
  (kind/table
   {:column-names ["$n$" "reel (Pólya)" "Formula"],
    :row-vectors rows})))


(def
 v80_l409
 (let
  [G-c
   (reel/cyclic-group 12)
   G-d
   (reel/dihedral-group 12)
   act-c
   (fn [g x] (mod (+ (long x) (long g)) 12))
   act-d
   (fn
    [[t k] x]
    (case
     t
     :r
     (mod (+ (long x) (long k)) 12)
     :s
     (mod (- (long k) (long x)) 12)))
   {domain-3 :domain, act-c-sub :act}
   (reel/subset-action act-c (range 12) 3)
   {_ :domain, act-d-sub :act}
   (reel/subset-action act-d (range 12) 3)
   under-C12
   (count (reel/orbits G-c act-c-sub domain-3))
   under-D12
   (count (reel/orbits G-d act-d-sub domain-3))]
  {:total (count domain-3),
   :under-C12 under-C12,
   :under-D12 under-D12}))


(deftest
 t81_l422
 (is (= v80_l409 {:total 220, :under-C12 19, :under-D12 12})))
