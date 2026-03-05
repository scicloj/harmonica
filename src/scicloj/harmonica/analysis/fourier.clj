(ns scicloj.harmonica.analysis.fourier
  "Fourier analysis on finite groups.

   For abelian groups, the Fourier transform of a function f: G -> C is:
     f-hat(k) = sum_{g in G} f(g) * conj(chi_k(g))

   where chi_k are the irreducible characters (rows of the character table).

   The inverse transform recovers f:
     f(g) = (1/|G|) * sum_k f-hat(k) * chi_k(g)

   Convolution in the group domain corresponds to pointwise multiplication
   in the Fourier domain."
  (:require [scicloj.harmonica.protocols :as p]
            [scicloj.lalinea.tensor :as t]
            [scicloj.lalinea.elementwise :as el]
            [scicloj.lalinea.linalg :as la]))

;; ---------------------------------------------------------------------------

(defn- scalars->complex-tensor
  "Assemble a sequence of scalar ComplexTensors into a 1D ComplexTensor."
  [scalars]
  (t/complex-tensor (double-array (mapv #(double (el/re %)) scalars))
                    (double-array (mapv #(double (el/im %)) scalars))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn fourier-transform
  "Compute the Fourier transform of a function on a finite group.

   f-vals is a ComplexTensor vector indexed by group elements.
   For abelian groups, returns a ComplexTensor vector of Fourier coefficients.

   f-hat(k) = sum_{g} f(g) * conj(chi_k(g)) = <f, chi_k>_H"
  [ct f-vals]
  (let [table (:table ct)
        k (count (:irrep-labels ct))
        scalars (mapv (fn [j] (la/dot f-vals (table j))) (range k))]
    (scalars->complex-tensor scalars)))

(defn inverse-fourier-transform
  "Recover a function from its Fourier coefficients.

   f-hat is a ComplexTensor vector of Fourier coefficients.
   Returns a ComplexTensor vector indexed by group elements.

   f(g) = (1/|G|) * sum_k f-hat(k) * chi_k(g)"
  [ct f-hat]
  (let [table (:table ct)
        n (p/order (:group ct))
        f-hat (t/materialize f-hat)
        inv-n (/ 1.0 (double n))
        scalars (mapv (fn [g]
                        (el/scale (el/sum (el/* f-hat (t/select table :all g)))
                                  inv-n))
                      (range n))]
    (scalars->complex-tensor scalars)))

(defn convolve
  "Convolve two functions on a finite group via the Fourier domain.

   (f * h)(g) = sum_{x} f(x) * h(x^{-1} g)

   Computed as: IFFT(FFT(f) . FFT(h)) where . is pointwise multiplication."
  [ct f-vals h-vals]
  (let [fhat-f (fourier-transform ct f-vals)
        fhat-h (fourier-transform ct h-vals)
        product (t/materialize (el/* fhat-f fhat-h))]
    (inverse-fourier-transform ct product)))

(defn total-variation-distance
  "Total variation distance between two probability distributions on a finite group.

   ||P - Q||_TV = (1/2) * sum_{g} |P(g) - Q(g)|

   p-vals and q-vals are numeric collections of real probabilities."
  [p-vals q-vals]
  (* 0.5 (el/sum (el/abs (el/- p-vals q-vals)))))
