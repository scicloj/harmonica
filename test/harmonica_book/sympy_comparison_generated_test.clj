(ns
 harmonica-book.sympy-comparison-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [scicloj.kindly.v4.kind :as kind]
  [libpython-clj2.python :as py]
  [libpython-clj2.require :refer [require-python]]
  [clojure.test :refer [deftest is]]))


(def v3_l21 (require-python '[sympy.combinatorics :as sc]))


(def
 v4_l22
 (require-python '[sympy.combinatorics.named_groups :as named]))


(def v5_l23 (require-python '[sympy.combinatorics.permutations :as sp]))


(def v6_l24 (require-python '[sympy.utilities.iterables :as symiter]))


(def v7_l25 (require-python '[builtins :as pybuiltins]))


(def
 v9_l40
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
 v10_l54
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
 v12_l67
 (let
  [G (hm/symmetric-group 3)]
  (every?
   (fn
    [sigma]
    (let
     [p (sp/Permutation (vec sigma))]
     (= (hm-perm-props G sigma) (sympy-perm-props p))))
   (hm/elements G))))


(deftest t13_l73 (is (true? v12_l67)))


(def
 v15_l77
 (let
  [G (hm/symmetric-group 4)]
  (every?
   (fn
    [sigma]
    (let
     [p (sp/Permutation (vec sigma))]
     (= (hm-perm-props G sigma) (sympy-perm-props p))))
   (hm/elements G))))


(deftest t16_l83 (is (true? v15_l77)))


(def
 v18_l87
 (let
  [G (hm/symmetric-group 5)]
  (every?
   (fn
    [sigma]
    (let
     [p (sp/Permutation (vec sigma))]
     (= (hm-perm-props G sigma) (sympy-perm-props p))))
   (hm/elements G))))


(deftest t19_l93 (is (true? v18_l87)))


(def
 v21_l101
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


(deftest t22_l114 (is (true? v21_l101)))


(def
 v24_l124
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


(deftest t25_l137 (is (true? v24_l124)))


(def
 v26_l139
 (defn
  sympy-class-sizes
  "Sorted vector of conjugacy class sizes from SymPy's SymmetricGroup(n)."
  [n]
  (let
   [Sn (named/SymmetricGroup n) classes (py/py. Sn conjugacy_classes)]
   (sort
    (mapv
     (fn* [p1__92611#] (long (py/py. p1__92611# __len__)))
     classes)))))


(def
 v27_l146
 (defn
  hm-class-sizes
  "Sorted vector of conjugacy class sizes from harmonica's symmetric-group."
  [n]
  (let
   [G (hm/symmetric-group n) classes (hm/conjugacy-classes G)]
   (sort
    (mapv (fn* [p1__92612#] (count (:elements p1__92612#))) classes)))))


(def
 v28_l153
 (every?
  (fn [n] (= (hm-class-sizes n) (sympy-class-sizes n)))
  (range 2 8)))


(deftest t29_l157 (is (true? v28_l153)))


(def
 v31_l161
 (let
  [n 5]
  (kind/table
   {:column-names ["Source" "Class sizes (sorted)"],
    :row-vectors
    [["harmonica" (str (hm-class-sizes n))]
     ["SymPy" (str (sympy-class-sizes n))]]})))


(def
 v33_l173
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
      (fn* [p1__92613#] (long (py/py. p1__92613# __len__)))
      classes))]
   {:order order, :num-classes (count classes), :class-sizes sizes})))


(def
 v34_l180
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
     (mapv (fn* [p1__92614#] (count (:elements p1__92614#))) classes))]
   {:order (hm/order G),
    :num-classes (count classes),
    :class-sizes sizes})))


(def
 v35_l186
 (every?
  (fn [n] (= (hm-dihedral-info n) (sympy-dihedral-info n)))
  (range 3 13)))


(deftest t36_l190 (is (true? v35_l186)))


(def
 v38_l194
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
 v40_l208
 (every?
  (fn
   [n]
   (let
    [Cn (named/CyclicGroup n)]
    (and (py/py.- Cn is_abelian) (py/py.- Cn is_cyclic))))
  (range 2 13)))


(deftest t41_l214 (is (true? v40_l208)))


(def
 v42_l216
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


(deftest t43_l223 (is (true? v42_l216)))


(def
 v45_l230
 (defn
  sympy-partition-count
  [n]
  (long (py/py. (pybuiltins/list (symiter/partitions n)) __len__))))


(def
 v46_l233
 (every?
  (fn [n] (= (count (hm/partitions n)) (sympy-partition-count n)))
  (range 1 16)))


(deftest t47_l237 (is (true? v46_l233)))


(def
 v49_l241
 (let
  [rows
   (mapv
    (fn [n] [n (count (hm/partitions n)) (sympy-partition-count n)])
    (range 1 13))]
  (kind/table
   {:column-names ["n" "harmonica" "SymPy"], :row-vectors rows})))


(def
 v51_l265
 (def
  known-S3
  {:irreps [[3] [2 1] [1 1 1]],
   :classes [[1 1 1] [2 1] [3]],
   :table [[1 1 1] [2 0 -1] [1 -1 1]]}))


(def
 v53_l280
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
 v55_l291
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
 v56_l302
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
      (mapv (fn* [p1__92615#] (Math/round (el/re p1__92615#))) row))
     table)})))


(def
 v58_l318
 (let
  [hm-ct (extract-hm-character-table 3)]
  (= (:table known-S3) (:table hm-ct))))


(deftest t59_l321 (is (true? v58_l318)))


(def
 v61_l325
 (let
  [hm-ct (extract-hm-character-table 4)]
  (= (:table known-S4) (:table hm-ct))))


(deftest t62_l328 (is (true? v61_l325)))


(def
 v64_l332
 (let
  [hm-ct (extract-hm-character-table 5)]
  (= (:table known-S5) (:table hm-ct))))


(deftest t65_l335 (is (true? v64_l332)))


(def
 v67_l339
 (let
  [hm-ct
   (extract-hm-character-table 5)
   rows
   (map-indexed
    (fn
     [i irrep]
     (let
      [hm-row
       (nth (:table hm-ct) i)
       known-row
       (nth (:table known-S5) i)]
      [(str irrep) (str hm-row) (str known-row) (= hm-row known-row)]))
    (:irreps known-S5))]
  (kind/table
   {:column-names ["Irrep" "harmonica" "Known" "Match?"],
    :row-vectors (vec rows)})))


(def
 v69_l357
 (every?
  (fn
   [n]
   (let
    [ct
     (extract-hm-character-table n)
     dims-from-ct
     (mapv first (:table ct))
     dims-from-hook
     (mapv hm/hook-length-dimension (:irreps ct))]
    (= dims-from-ct dims-from-hook)))
  (range 2 8)))


(deftest t70_l364 (is (true? v69_l357)))


(def
 v72_l369
 (every?
  (fn
   [n]
   (let
    [parts
     (hm/partitions n)
     total
     (reduce
      +
      (map
       (fn*
        [p1__92616#]
        (let [d (hm/hook-length-dimension p1__92616#)] (* d d)))
       parts))]
    (= total (reduce * (range 1 (inc n))))))
  (range 2 8)))


(deftest t73_l375 (is (true? v72_l369)))


(def
 v75_l383
 (defn
  necklace-formula
  "Number of binary necklaces with n beads, from the formula:\n   (1/n) * sum_{d|n} phi(d) * 2^{n/d}"
  [n]
  (let
   [divisors
    (filter
     (fn* [p1__92617#] (zero? (mod n p1__92617#)))
     (range 1 (inc n)))
    euler-phi
    (fn
     [m]
     (count
      (filter
       (fn*
        [p1__92618#]
        (=
         1
         (long
          (.gcd
           (BigInteger/valueOf m)
           (BigInteger/valueOf p1__92618#)))))
       (range 1 (inc m)))))]
   (/
    (reduce
     +
     (map
      (fn [d] (* (euler-phi d) (long (Math/pow 2 (/ n d)))))
      divisors))
    n))))


(def
 v76_l395
 (defn
  hm-necklace-count
  [n]
  (let
   [G
    (hm/cyclic-group n)
    act
    (fn [g x] (mod (+ x g) n))
    ci
    (hm/cycle-index G act (range n))]
   (hm/polya-count ci 2))))


(def
 v77_l401
 (every?
  (fn [n] (= (hm-necklace-count n) (necklace-formula n)))
  (range 1 21)))


(deftest t78_l405 (is (true? v77_l401)))


(def
 v80_l409
 (let
  [rows
   (mapv
    (fn [n] [n (hm-necklace-count n) (necklace-formula n)])
    (range 1 13))]
  (kind/table
   {:column-names ["n" "harmonica (Pólya)" "Formula"],
    :row-vectors rows})))


(def
 v82_l422
 (every?
  (fn
   [n]
   (let
    [G
     (hm/symmetric-group n)
     Sn
     (named/SymmetricGroup n)
     act
     (fn [sigma x] (sigma x))]
    (every?
     (fn
      [x]
      (let
       [orb-hm
        (count (hm/orbit G act x))
        stab-hm
        (count (hm/stabilizer G act x))
        orb-py
        (long (py/py. (py/py. Sn orbit x) __len__))
        stab-py
        (long (py/py. (py/py. Sn stabilizer x) order))]
       (and (= orb-hm orb-py) (= stab-hm stab-py))))
     (range n))))
  (range 2 6)))


(deftest t83_l436 (is (true? v82_l422)))


(def
 v85_l447
 (let
  [G-c
   (hm/cyclic-group 12)
   G-d
   (hm/dihedral-group 12)
   act-c
   (fn [g x] (mod (+ x g) 12))
   act-d
   (fn [[t k] x] (case t :r (mod (+ x k) 12) :s (mod (- k x) 12)))
   {domain-3 :domain, act-c-sub :act}
   (hm/subset-action act-c (range 12) 3)
   {_ :domain, act-d-sub :act}
   (hm/subset-action act-d (range 12) 3)
   under-C12
   (count (hm/orbits G-c act-c-sub domain-3))
   under-D12
   (count (hm/orbits G-d act-d-sub domain-3))]
  {:total (count domain-3),
   :under-C12 under-C12,
   :under-D12 under-D12}))


(deftest
 t86_l460
 (is (= v85_l447 {:total 220, :under-C12 19, :under-D12 12})))
