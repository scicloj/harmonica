(ns scicloj.reel.characters
  "Character tables for finite groups.

   A character table is a value (map) containing:
     :group        - the group
     :classes      - vector of conjugacy class representatives
     :class-sizes  - vector of class sizes
     :irrep-labels - vector of labels for each irreducible representation
     :table        - vector of vectors of complex values (Vec2)
                     table[i][j] = chi_i(class_j)

   For cyclic groups Z/nZ, the character table is exactly the DFT matrix:
   table[j][k] = omega^(jk) where omega = e^(2*pi*i/n)."
  (:require [scicloj.reel.protocols :as p]
            [fastmath.complex :as c]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]))

;; ---------------------------------------------------------------------------
;; Character table construction (multimethod on group type)
;; ---------------------------------------------------------------------------

(defmulti character-table
  "Compute the character table of a finite group.
   Returns a map with :group, :classes, :class-sizes, :irrep-labels, :table."
  p/group-type)

(defmethod character-table :cyclic
  [G]
  (let [n (p/order G)
        ;; omega = e^(2*pi*i/n), the primitive n-th root of unity
        angle (/ (* 2.0 Math/PI) n)
        ;; Precompute omega^k for k = 0..n-1
        roots (mapv (fn [k]
                      (c/complex (Math/cos (* k angle))
                                 (Math/sin (* k angle))))
                    (range n))
        ;; table[j][k] = omega^(j*k) = roots[(j*k) mod n]
        table (mapv (fn [j]
                      (mapv (fn [k]
                              (nth roots (mod (* j k) n)))
                            (range n)))
                    (range n))]
    {:group G
     :classes (vec (range n))
     :class-sizes (vec (repeat n 1))
     :irrep-labels (vec (range n))
     :table table}))

;; ---------------------------------------------------------------------------
;; Character inner product
;; ---------------------------------------------------------------------------

(defn character-inner-product
  "Compute the inner product of two characters (or class functions).

   <chi, psi> = (1/|G|) * sum_{classes} |C_mu| * chi(mu) * conj(psi(mu))

   chi-vals and psi-vals are vectors of complex values (Vec2), one per class.
   class-sizes is a vector of class sizes.
   group-order is |G|."
  [chi-vals psi-vals class-sizes group-order]
  (let [n (count chi-vals)
        sum (reduce (fn [^Vec2 acc i]
                      (let [chi-i (nth chi-vals i)
                            psi-i (nth psi-vals i)
                            size (nth class-sizes i)]
                        (c/add acc (c/scale (c/mult chi-i (c/conjugate psi-i))
                                           (double size)))))
                    c/ZERO
                    (range n))]
    (c/scale sum (/ 1.0 (double group-order)))))
