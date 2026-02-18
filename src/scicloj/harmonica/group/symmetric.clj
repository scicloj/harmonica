(ns scicloj.harmonica.group.symmetric
  "Symmetric group S_n — the group of all permutations of {0, ..., n-1}.

   Elements are 0-indexed one-line notation vectors: [σ(0) σ(1) ... σ(n-1)].
   The group operation is composition (right-to-left):
   (op G σ τ) = σ∘τ where (σ∘τ)(i) = σ(τ(i))."
  (:require [scicloj.harmonica.protocols :as p]
            [scicloj.harmonica.combinatorics.permutation :as perm]
            [scicloj.harmonica.combinatorics.partition :as part]))

(defn- all-permutations
  "All permutations of [0 .. n-1] as vectors."
  [n]
  (if (<= n 0)
    [[]]
    (let [perms-of-rest (all-permutations (dec n))]
      (for [perm perms-of-rest
            i (range n)]
        (let [;; Insert (n-1) at position i in the permutation of [0..n-2]
              ;; But we need to work with permutations of {0..n-1}
              ;; Strategy: take a permutation of {0..n-2}, then insert n-1 at position i
              ;; by shifting elements
              before (subvec perm 0 i)
              after (subvec perm i)]
          (into (conj before (dec n)) after))))))

(defn- representative-from-partition
  "Build a canonical permutation with the given cycle type.
   Lays out consecutive cycles: partition [3 2 1] on n=6 gives (0 1 2)(3 4)(5)."
  [n partition]
  (let [[_ cycles] (reduce (fn [[pos cs] part-len]
                             (let [cycle (vec (range pos (+ pos part-len)))]
                               [(+ pos part-len) (conj cs cycle)]))
                           [0 []]
                           partition)]
    (perm/from-cycles n cycles)))

(defn- compute-conjugacy-classes
  "Compute conjugacy classes of S_n indexed by cycle type (partition)."
  [n]
  (if (<= n 1)
    [{:representative (perm/identity-perm (max n 0))
      :elements #{(perm/identity-perm (max n 0))}
      :size 1
      :cycle-type (if (pos? n) [1] [])}]
    (let [parts (part/partitions n)
          ;; For small n, enumerate elements and group by cycle type
          enumerate? (<= n 8)
          by-ct (when enumerate?
                  (group-by perm/cycle-type (all-permutations n)))]
      (mapv (fn [partition]
              (let [elts (when enumerate? (set (get by-ct partition)))]
                {:representative (if elts
                                   (first elts)
                                   (representative-from-partition n partition))
                 :elements elts
                 :size (part/partition-class-size n partition)
                 :cycle-type partition}))
            parts))))

(defrecord SymmetricGroup [n]
  p/Group
  (op [_ g h] (perm/compose g h))
  (inv [_ g] (perm/inverse g))
  (id [_] (perm/identity-perm n))

  p/FiniteGroup
  (elements [_] (all-permutations n))
  (order [_] (reduce *' (range 1 (inc n))))

  p/GroupStructure
  (conjugacy-classes [_]
    (compute-conjugacy-classes n))

  p/GroupType
  (group-type [_] :symmetric))

(defn symmetric-group
  "Create the symmetric group S_n — all permutations of {0, ..., n-1}."
  [n]
  {:pre [(and (integer? n) (>= n 0))]}
  (->SymmetricGroup n))
