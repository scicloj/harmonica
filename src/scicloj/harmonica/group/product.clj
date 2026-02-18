(ns scicloj.harmonica.group.product
  "Direct product of two finite groups G₁ × G₂.

   Elements are pairs [g h] where g ∈ G₁ and h ∈ G₂.
   All operations are componentwise."
  (:require [scicloj.harmonica.protocols :as p]))

(defrecord ProductGroup [G1 G2]
  p/Group
  (op [_ [g1 g2] [h1 h2]]
    [(p/op G1 g1 h1) (p/op G2 g2 h2)])
  (inv [_ [g h]]
    [(p/inv G1 g) (p/inv G2 h)])
  (id [_]
    [(p/id G1) (p/id G2)])

  p/FiniteGroup
  (elements [_]
    (for [g (p/elements G1)
          h (p/elements G2)]
      [g h]))
  (order [_]
    (* (p/order G1) (p/order G2)))

  p/GroupStructure
  (conjugacy-classes [_]
    ;; Classes of G₁ × G₂ are products of classes from each factor.
    ;; Only materialize :elements when both factors provide them
    ;; (avoids expensive Cartesian products for large groups).
    (let [classes1 (p/conjugacy-classes G1)
          classes2 (p/conjugacy-classes G2)]
      (vec (for [c1 classes1
                 c2 classes2]
             (cond-> {:representative [(:representative c1) (:representative c2)]
                      :size (* (:size c1) (:size c2))}
               (and (:elements c1) (:elements c2))
               (assoc :elements (set (for [g (:elements c1)
                                           h (:elements c2)]
                                       [g h]))))))))

  p/GroupType
  (group-type [_] :product))

(defn product-group
  "Create the direct product G₁ × G₂ of two finite groups."
  [G1 G2]
  (->ProductGroup G1 G2))
