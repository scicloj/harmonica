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
 v24_l122
 (let
  [G (reel/symmetric-group 4) elts (vec (sort (reel/elements G)))]
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


(deftest t25_l135 (is (true? v24_l122)))


(def
 v26_l137
 (defn
  sympy-class-sizes
  "Sorted vector of conjugacy class sizes from SymPy's SymmetricGroup(n)."
  [n]
  (let
   [Sn (named/SymmetricGroup n) classes (py/py. Sn conjugacy_classes)]
   (sort
    (mapv
     (fn* [p1__45818#] (long (py/py. p1__45818# __len__)))
     classes)))))


(def
 v27_l144
 (defn
  reel-class-sizes
  "Sorted vector of conjugacy class sizes from reel's symmetric-group."
  [n]
  (let
   [G (reel/symmetric-group n) classes (reel/conjugacy-classes G)]
   (sort
    (mapv (fn* [p1__45819#] (count (:elements p1__45819#))) classes)))))


(def
 v28_l151
 (every?
  (fn [n] (= (reel-class-sizes n) (sympy-class-sizes n)))
  (range 2 8)))


(deftest t29_l155 (is (true? v28_l151)))


(def
 v31_l159
 (let
  [n 5]
  (kind/table
   {:column-names ["Source" "Class sizes (sorted)"],
    :row-vectors
    [["reel" (str (reel-class-sizes n))]
     ["SymPy" (str (sympy-class-sizes n))]]})))


(def
 v33_l171
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
      (fn* [p1__45820#] (long (py/py. p1__45820# __len__)))
      classes))]
   {:order order, :num-classes (count classes), :class-sizes sizes})))


(def
 v34_l178
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
     (mapv (fn* [p1__45821#] (count (:elements p1__45821#))) classes))]
   {:order (reel/order G),
    :num-classes (count classes),
    :class-sizes sizes})))


(def
 v35_l184
 (every?
  (fn [n] (= (reel-dihedral-info n) (sympy-dihedral-info n)))
  (range 3 13)))


(deftest t36_l188 (is (true? v35_l184)))


(def
 v38_l192
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
 v40_l206
 (every?
  (fn
   [n]
   (let
    [Cn (named/CyclicGroup n)]
    (and (py/py.- Cn is_abelian) (py/py.- Cn is_cyclic))))
  (range 2 13)))


(deftest t41_l212 (is (true? v40_l206)))


(def
 v42_l214
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


(deftest t43_l221 (is (true? v42_l214)))


(def
 v45_l228
 (defn
  sympy-partition-count
  [n]
  (long (py/py. (pybuiltins/list (symiter/partitions n)) __len__))))


(def
 v46_l231
 (every?
  (fn [n] (= (count (reel/partitions n)) (sympy-partition-count n)))
  (range 1 16)))


(deftest t47_l235 (is (true? v46_l231)))


(def
 v49_l239
 (let
  [rows
   (mapv
    (fn [n] [n (count (reel/partitions n)) (sympy-partition-count n)])
    (range 1 13))]
  (kind/table
   {:column-names ["$n$" "reel" "SymPy"], :row-vectors rows})))


(def
 v51_l263
 (def
  known-S3
  {:irreps [[3] [2 1] [1 1 1]],
   :classes [[1 1 1] [2 1] [3]],
   :table [[1 1 1] [2 0 -1] [1 -1 1]]}))


(def
 v53_l278
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
 v55_l289
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
 v56_l300
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
       (fn* [p1__45822#] (long (aget row p1__45822#)))
       (range (count classes))))
     table-re)})))


(def
 v58_l319
 (let
  [reel-ct (extract-reel-character-table 3)]
  (= (:table known-S3) (:table reel-ct))))


(deftest t59_l322 (is (true? v58_l319)))


(def
 v61_l326
 (let
  [reel-ct (extract-reel-character-table 4)]
  (= (:table known-S4) (:table reel-ct))))


(deftest t62_l329 (is (true? v61_l326)))


(def
 v64_l333
 (let
  [reel-ct (extract-reel-character-table 5)]
  (= (:table known-S5) (:table reel-ct))))


(deftest t65_l336 (is (true? v64_l333)))


(def
 v67_l340
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
 v69_l358
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


(deftest t70_l365 (is (true? v69_l358)))


(def
 v72_l370
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
        [p1__45823#]
        (let [d (reel/hook-length-dimension p1__45823#)] (* d d)))
       parts))]
    (= total (reduce * (range 1 (inc n))))))
  (range 2 8)))


(deftest t73_l376 (is (true? v72_l370)))


(def
 v75_l384
 (defn
  necklace-formula
  "Number of binary necklaces with n beads, from the formula:\n   (1/n) * sum_{d|n} phi(d) * 2^{n/d}"
  [n]
  (let
   [divisors
    (filter
     (fn* [p1__45824#] (zero? (mod n p1__45824#)))
     (range 1 (inc n)))
    euler-phi
    (fn
     [m]
     (count
      (filter
       (fn*
        [p1__45825#]
        (=
         1
         (long
          (.gcd
           (BigInteger/valueOf m)
           (BigInteger/valueOf p1__45825#)))))
       (range 1 (inc m)))))]
   (/
    (reduce
     +
     (map
      (fn [d] (* (euler-phi d) (long (Math/pow 2 (/ n d)))))
      divisors))
    n))))


(def
 v76_l396
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
 v77_l402
 (every?
  (fn [n] (= (reel-necklace-count n) (necklace-formula n)))
  (range 1 21)))


(deftest t78_l406 (is (true? v77_l402)))


(def
 v80_l410
 (let
  [rows
   (mapv
    (fn [n] [n (reel-necklace-count n) (necklace-formula n)])
    (range 1 13))]
  (kind/table
   {:column-names ["$n$" "reel (Pólya)" "Formula"],
    :row-vectors rows})))


(def
 v82_l423
 (every?
  (fn
   [n]
   (let
    [G
     (reel/symmetric-group n)
     Sn
     (named/SymmetricGroup n)
     act
     (fn [sigma x] (sigma x))]
    (every?
     (fn
      [x]
      (let
       [orb-reel
        (count (reel/orbit G act x))
        stab-reel
        (count (reel/stabilizer G act x))
        orb-py
        (long (py/py. (py/py. Sn orbit x) __len__))
        stab-py
        (long (py/py. (py/py. Sn stabilizer x) order))]
       (and (= orb-reel orb-py) (= stab-reel stab-py))))
     (range n))))
  (range 2 6)))


(deftest t83_l437 (is (true? v82_l423)))


(def
 v85_l448
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
 t86_l461
 (is (= v85_l448 {:total 220, :under-C12 19, :under-D12 12})))
