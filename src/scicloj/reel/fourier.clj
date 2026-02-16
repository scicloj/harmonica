(ns scicloj.reel.fourier
  "Fourier analysis on finite groups.

   For abelian groups, the Fourier transform of a function f: G -> C is:
     f-hat(k) = sum_{g in G} f(g) * conj(chi_k(g))

   where chi_k are the irreducible characters (rows of the character table).

   The inverse transform recovers f:
     f(g) = (1/|G|) * sum_k f-hat(k) * chi_k(g)

   Convolution in the group domain corresponds to pointwise multiplication
   in the Fourier domain."
  (:require [scicloj.reel.protocols :as p]
            [scicloj.reel.characters :as ch]
            [fastmath.complex :as c])
  (:import [fastmath.vector Vec2]))

(defn fourier-transform
  "Compute the Fourier transform of a function on a finite group.

   f-vals is a vector of complex values (Vec2) indexed by group elements.
   For abelian groups, returns a vector of complex Fourier coefficients.

   f-hat(k) = sum_{g} f(g) * conj(chi_k(g))"
  [ct f-vals]
  (let [{:keys [table]} ct
        n (count f-vals)]
    (mapv (fn [chi-row]
            (reduce (fn [^Vec2 acc g]
                      (c/add acc
                             (c/mult (nth f-vals g)
                                     (c/conjugate (nth chi-row g)))))
                    c/ZERO
                    (range n)))
          table)))

(defn inverse-fourier-transform
  "Recover a function from its Fourier coefficients.

   f-hat is a vector of complex Fourier coefficients.
   Returns a vector of complex values indexed by group elements.

   f(g) = (1/|G|) * sum_k f-hat(k) * chi_k(g)"
  [ct f-hat]
  (let [{:keys [table group]} ct
        n (p/order group)
        scale (/ 1.0 (double n))]
    (mapv (fn [g]
            (c/scale
             (reduce (fn [^Vec2 acc k]
                       (c/add acc
                              (c/mult (nth f-hat k)
                                      (nth (nth table k) g))))
                     c/ZERO
                     (range n))
             scale))
          (range n))))

(defn convolve
  "Convolve two functions on a finite group via the Fourier domain.

   (f * h)(g) = sum_{x} f(x) * h(x^{-1} g)

   Computed as: IFFT(FFT(f) . FFT(h)) where . is pointwise multiplication."
  [ct f-vals h-vals]
  (let [f-hat (fourier-transform ct f-vals)
        h-hat (fourier-transform ct h-vals)
        product (mapv c/mult f-hat h-hat)]
    (inverse-fourier-transform ct product)))

(defn total-variation-distance
  "Total variation distance between two probability distributions on a finite group.

   ||P - Q||_TV = (1/2) * sum_{g} |P(g) - Q(g)|

   p-vals and q-vals are vectors of real probabilities (as doubles)."
  [p-vals q-vals]
  (let [n (count p-vals)]
    (* 0.5
       (reduce (fn [^double acc i]
                 (+ acc (Math/abs (- (double (nth p-vals i))
                                     (double (nth q-vals i))))))
               0.0
               (range n)))))
