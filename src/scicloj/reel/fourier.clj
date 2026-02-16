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
            [fastmath.complex :as c]
            [tech.v3.datatype.functional :as dfn])
  (:import [fastmath.vector Vec2]))

(defn fourier-transform
  "Compute the Fourier transform of a function on a finite group.

   f-vals is a vector of complex values (Vec2) indexed by group elements.
   For abelian groups, returns a vector of complex Fourier coefficients.

   f-hat(k) = sum_{g} f(g) * conj(chi_k(g))

   Uses split real/imaginary double arrays from the character table
   for vectorized computation via dtype-next."
  [ct f-vals]
  (let [{:keys [table-re table-im]} ct
        n (count f-vals)
        ;; Split f into real/imaginary double arrays
        f-re (double-array n)
        f-im (double-array n)]
    (dotimes [i n]
      (let [^Vec2 v (nth f-vals i)]
        (aset f-re i (.-x v))
        (aset f-im i (.-y v))))
    ;; f(g) * conj(chi_k(g)) = (a + bi)(c - di) = (ac + bd) + (bc - ad)i
    ;; f-hat(k) = sum_g [ (f-re * chi-re + f-im * chi-im)
    ;;                   + i*(f-im * chi-re - f-re * chi-im) ]
    (mapv (fn [chi-re chi-im]
            (c/complex (dfn/sum (dfn/+ (dfn/* f-re chi-re)
                                       (dfn/* f-im chi-im)))
                       (dfn/sum (dfn/- (dfn/* f-im chi-re)
                                       (dfn/* f-re chi-im)))))
          table-re table-im)))

(defn inverse-fourier-transform
  "Recover a function from its Fourier coefficients.

   f-hat is a vector of complex Fourier coefficients.
   Returns a vector of complex values indexed by group elements.

   f(g) = (1/|G|) * sum_k f-hat(k) * chi_k(g)

   Uses split real/imaginary double arrays for vectorized computation."
  [ct f-hat]
  (let [{:keys [table-re table-im group]} ct
        n (p/order group)
        scale (/ 1.0 (double n))
        ;; Split f-hat into real/imaginary double arrays
        fh-re (double-array n)
        fh-im (double-array n)]
    (dotimes [k n]
      (let [^Vec2 v (nth f-hat k)]
        (aset fh-re k (.-x v))
        (aset fh-im k (.-y v))))
    ;; f(g) = (1/n) * sum_k f-hat(k) * chi_k(g)
    ;; For each g, we need column g of the table: chi_k(g) for all k.
    ;; We extract column g from the split arrays and compute:
    ;; sum_k (fh-re * col-re - fh-im * col-im) + i*(fh-re * col-im + fh-im * col-re)
    (mapv (fn [g]
            (let [col-re (double-array n)
                  col-im (double-array n)]
              (dotimes [k n]
                (aset col-re k (aget ^doubles (table-re k) (int g)))
                (aset col-im k (aget ^doubles (table-im k) (int g))))
              (c/complex (* scale (dfn/sum (dfn/- (dfn/* fh-re col-re)
                                                  (dfn/* fh-im col-im))))
                         (* scale (dfn/sum (dfn/+ (dfn/* fh-re col-im)
                                                  (dfn/* fh-im col-re)))))))
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
  (* 0.5 (dfn/sum (dfn/abs (dfn/- (double-array p-vals)
                                  (double-array q-vals))))))
