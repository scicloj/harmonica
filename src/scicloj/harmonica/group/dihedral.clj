(ns scicloj.harmonica.group.dihedral
  "Dihedral group D_n — the group of symmetries of a regular n-gon.

   Order 2n. Presentation: r^n = s^2 = e, s·r·s = r^{-1}.

   Elements are tagged pairs:
     [:r k] — rotation by 2πk/n (k = 0, ..., n-1)
     [:s k] — reflection (r^k composed with base reflection s)

   The group operation follows from the presentation:
     r^a · r^b = r^{(a+b) mod n}
     r^a · s^b = s^{(a+b) mod n}    (i.e. rotation then reflection)
     s^a · r^b = s^{(a-b) mod n}    (conjugation reverses rotation)
     s^a · s^b = r^{(a-b) mod n}    (two reflections give a rotation)"
  (:require [scicloj.harmonica.protocols :as p]))

(defn- dihedral-op
  "Group operation for D_n."
  [n [t1 k1] [t2 k2]]
  (case [t1 t2]
    [:r :r] [:r (mod (+ (long k1) (long k2)) n)]
    [:r :s] [:s (mod (+ (long k1) (long k2)) n)]
    [:s :r] [:s (mod (- (long k1) (long k2)) n)]
    [:s :s] [:r (mod (- (long k1) (long k2)) n)]))

(defn- dihedral-inv
  "Group inverse for D_n."
  [n [t k]]
  (case t
    :r [:r (mod (- n (long k)) n)]
    :s [t k])) ;; reflections are involutions

(defn- dihedral-conjugacy-classes
  "Conjugacy classes of D_n."
  [n]
  (if (odd? n)
    ;; n odd: (n+3)/2 classes
    (let [identity-class {:representative [:r 0]
                          :elements #{[:r 0]}
                          :size 1}
          rotation-classes (mapv (fn [k]
                                   {:representative [:r k]
                                    :elements #{[:r k] [:r (- n k)]}
                                    :size 2})
                                 (range 1 (inc (quot (dec n) 2))))
          reflection-class {:representative [:s 0]
                            :elements (set (map (fn [k] [:s k]) (range n)))
                            :size n}]
      (into [identity-class] (conj rotation-classes reflection-class)))
    ;; n even: n/2 + 3 classes
    (let [half (quot n 2)
          identity-class {:representative [:r 0]
                          :elements #{[:r 0]}
                          :size 1}
          half-turn-class {:representative [:r half]
                           :elements #{[:r half]}
                           :size 1}
          rotation-classes (mapv (fn [k]
                                   {:representative [:r k]
                                    :elements #{[:r k] [:r (- n k)]}
                                    :size 2})
                                 (range 1 half))
          ;; Even-index reflections: [:s 0], [:s 2], [:s 4], ...
          even-reflections {:representative [:s 0]
                            :elements (set (map (fn [k] [:s (* 2 k)])
                                                (range half)))
                            :size half}
          ;; Odd-index reflections: [:s 1], [:s 3], [:s 5], ...
          odd-reflections {:representative [:s 1]
                           :elements (set (map (fn [k] [:s (inc (* 2 k))])
                                               (range half)))
                           :size half}]
      (into [identity-class half-turn-class]
            (into rotation-classes [even-reflections odd-reflections])))))

(defrecord DihedralGroup [n]
  p/Group
  (op [_ g h] (dihedral-op n g h))
  (inv [_ g] (dihedral-inv n g))
  (id [_] [:r 0])

  p/FiniteGroup
  (elements [_]
    (concat (map (fn [k] [:r k]) (range n))
            (map (fn [k] [:s k]) (range n))))
  (order [_] (* 2 n))

  p/GroupStructure
  (conjugacy-classes [_]
    (dihedral-conjugacy-classes n))

  p/GroupType
  (group-type [_] :dihedral))

(defn dihedral-group
  "Create the dihedral group D_n — symmetries of a regular n-gon.
   Order 2n. Elements are [:r k] (rotations) and [:s k] (reflections)."
  [n]
  {:pre [(pos-int? n)]}
  (->DihedralGroup n))
