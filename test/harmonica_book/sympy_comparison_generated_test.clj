(ns
 harmonica-book.sympy-comparison-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [scicloj.kindly.v4.kind :as kind]
  [libpython-clj2.python :as py]
  [libpython-clj2.require :refer [require-python]]
  [clojure.test :refer [deftest is]]))


(def v3_l20 (require-python '[sympy.combinatorics :as sc]))


(def
 v4_l21
 (require-python '[sympy.combinatorics.named_groups :as named]))


(def v5_l22 (require-python '[sympy.combinatorics.permutations :as sp]))


(def v6_l23 (require-python '[sympy.utilities.iterables :as symiter]))


(def v7_l24 (require-python '[builtins :as pybuiltins]))


(def
 v9_l39
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
    (->> (mapcat (fn [k v] (repeat v k)) ks vs) (sort >) vec)
    parity
    (long (py/py. p parity))
    sign
    (if (zero? parity) 1 -1)
    order
    (long (pybuiltins/int (py/py. p order)))]
   {:partition partition, :sign sign, :order order})))


(def
 v10_l53
 (defn
  hm-perm-props
  "Extract cycle type, sign, and order from a harmonica permutation in S_n."
  [G sigma]
  (let
   [ct (hm/cycle-type sigma)]
   {:partition (vec ct),
    :sign (hm/sign sigma),
    :order
    (loop
     [g sigma k 1]
     (if (= g (hm/id G)) k (recur (hm/op G g sigma) (inc k))))})))


(def
 v12_l66
 (let
  [G (hm/symmetric-group 3)]
  (every?
   (fn
    [sigma]
    (let
     [p (sp/Permutation (vec sigma))]
     (= (hm-perm-props G sigma) (sympy-perm-props p))))
   (hm/elements G))))


(deftest t13_l72 (is (true? v12_l66)))


(def
 v15_l76
 (let
  [G (hm/symmetric-group 4)]
  (every?
   (fn
    [sigma]
    (let
     [p (sp/Permutation (vec sigma))]
     (= (hm-perm-props G sigma) (sympy-perm-props p))))
   (hm/elements G))))


(deftest t16_l82 (is (true? v15_l76)))


(def
 v18_l86
 (let
  [G (hm/symmetric-group 5)]
  (every?
   (fn
    [sigma]
    (let
     [p (sp/Permutation (vec sigma))]
     (= (hm-perm-props G sigma) (sympy-perm-props p))))
   (hm/elements G))))


(deftest t19_l92 (is (true? v18_l86)))


(def
 v21_l100
 (let
  [G (hm/symmetric-group 3) elts (vec (sort (hm/elements G)))]
  (every?
   (fn
    [[a b]]
    (let
     [ab-hm
      (hm/op G a b)
      inv-hm
      (hm/inv G a)
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
     (and (= (vec ab-hm) ab-py) (= (vec inv-hm) inv-py))))
   (for [a elts b elts] [a b]))))


(deftest t22_l113 (is (true? v21_l100)))


(def
 v24_l123
 (let
  [G (hm/symmetric-group 4) elts (vec (sort (hm/elements G)))]
  (every?
   (fn
    [[a b]]
    (let
     [ab-hm
      (hm/op G a b)
      inv-hm
      (hm/inv G a)
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
     (and (= (vec ab-hm) ab-py) (= (vec inv-hm) inv-py))))
   (for [a elts b elts] [a b]))))


(deftest t25_l136 (is (true? v24_l123)))


(def
 v26_l138
 (defn
  sympy-class-sizes
  "Sorted vector of conjugacy class sizes from SymPy's SymmetricGroup(n)."
  [n]
  (let
   [Sn (named/SymmetricGroup n) classes (py/py. Sn conjugacy_classes)]
   (sort
    (mapv
     (fn* [p1__141184#] (long (py/py. p1__141184# __len__)))
     classes)))))


(def
 v27_l145
 (defn
  hm-class-sizes
  "Sorted vector of conjugacy class sizes from harmonica's symmetric-group."
  [n]
  (let
   [G (hm/symmetric-group n) classes (hm/conjugacy-classes G)]
   (sort
    (mapv
     (fn* [p1__141185#] (count (:elements p1__141185#)))
     classes)))))


(def
 v28_l152
 (every?
  (fn [n] (= (hm-class-sizes n) (sympy-class-sizes n)))
  (range 2 8)))


(deftest t29_l156 (is (true? v28_l152)))


(def
 v31_l160
 (let
  [n 5]
  (kind/table
   {:column-names ["Source" "Class sizes (sorted)"],
    :row-vectors
    [["harmonica" (str (hm-class-sizes n))]
     ["SymPy" (str (sympy-class-sizes n))]]})))


(def
 v33_l172
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
      (fn* [p1__141186#] (long (py/py. p1__141186# __len__)))
      classes))]
   {:order order, :num-classes (count classes), :class-sizes sizes})))


(def
 v34_l179
 (defn
  hm-dihedral-info
  [n]
  (let
   [G
    (hm/dihedral-group n)
    classes
    (hm/conjugacy-classes G)
    sizes
    (sort
     (mapv
      (fn* [p1__141187#] (count (:elements p1__141187#)))
      classes))]
   {:order (hm/order G),
    :num-classes (count classes),
    :class-sizes sizes})))


(def
 v35_l185
 (every?
  (fn [n] (= (hm-dihedral-info n) (sympy-dihedral-info n)))
  (range 3 13)))


(deftest t36_l189 (is (true? v35_l185)))


(def
 v38_l193
 (let
  [rows
   (mapv
    (fn
     [n]
     (let
      [ri (hm-dihedral-info n) si (sympy-dihedral-info n)]
      [n (:order ri) (:num-classes ri) (= ri si)]))
    (range 3 13))]
  (kind/table
   {:column-names ["n" "Order" "Classes" "Match?"],
    :row-vectors rows})))


(def
 v40_l207
 (every?
  (fn
   [n]
   (let
    [Cn (named/CyclicGroup n)]
    (and (py/py.- Cn is_abelian) (py/py.- Cn is_cyclic))))
  (range 2 13)))


(deftest t41_l213 (is (true? v40_l207)))


(def
 v42_l215
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


(deftest t43_l222 (is (true? v42_l215)))


(def
 v45_l229
 (defn
  sympy-partition-count
  [n]
  (long (py/py. (pybuiltins/list (symiter/partitions n)) __len__))))


(def
 v46_l232
 (every?
  (fn [n] (= (count (hm/partitions n)) (sympy-partition-count n)))
  (range 1 16)))


(deftest t47_l236 (is (true? v46_l232)))


(def
 v49_l240
 (let
  [rows
   (mapv
    (fn [n] [n (count (hm/partitions n)) (sympy-partition-count n)])
    (range 1 13))]
  (kind/table
   {:column-names ["n" "harmonica" "SymPy"], :row-vectors rows})))


(def
 v51_l264
 (def
  known-S3
  {:irreps [[3] [2 1] [1 1 1]],
   :classes [[1 1 1] [2 1] [3]],
   :table [[1 1 1] [2 0 -1] [1 -1 1]]}))


(def
 v53_l279
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
 v55_l290
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
 v56_l301
 (defn
  extract-hm-character-table
  "Build a {:irreps :classes :table} map from harmonica's character table."
  [n]
  (let
   [G
    (hm/symmetric-group n)
    ct
    (hm/character-table G)
    classes
    (:classes ct)
    class-partitions
    (mapv :partition classes)
    table
    (:table ct)]
   {:irreps (:irrep-labels ct),
    :classes class-partitions,
    :table
    (mapv
     (fn
      [row]
      (mapv (fn* [p1__141188#] (Math/round (cx/re p1__141188#))) row))
     table)})))


(def
 v58_l317
 (let
  [hm-ct (extract-hm-character-table 3)]
  (= (:table known-S3) (:table hm-ct))))
