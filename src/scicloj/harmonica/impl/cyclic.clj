(ns scicloj.harmonica.impl.cyclic
  "Cyclic group Z/nZ.

   Elements are integers 0, 1, ..., n-1.
   The group operation is addition mod n."
  (:require [scicloj.harmonica.protocols :as p]))

(defrecord CyclicGroup [n]
  p/Group
  (op [_ g h] (mod (+ (long g) (long h)) n))
  (inv [_ g] (mod (- n (long g)) n))
  (id [_] 0)

  p/FiniteGroup
  (elements [_] (range n))
  (order [_] n)

  p/GroupStructure
  (conjugacy-classes [_]
    ;; Abelian group: each element is its own conjugacy class.
    (mapv (fn [g]
            {:representative g
             :elements #{g}
             :size 1})
          (range n)))

  p/GroupType
  (group-type [_] :cyclic))

(defn cyclic-group
  "Create the cyclic group Z/nZ of order n."
  [n]
  {:pre [(pos-int? n)]}
  (->CyclicGroup n))
