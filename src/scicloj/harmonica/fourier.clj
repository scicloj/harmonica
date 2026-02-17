(ns scicloj.harmonica.fourier
  "Fourier analysis on finite groups.

   For abelian groups, the Fourier transform of a function f: G -> C is:
     f-hat(k) = sum_{g in G} f(g) * conj(chi_k(g))

   where chi_k are the irreducible characters (rows of the character table).

   The inverse transform recovers f:
     f(g) = (1/|G|) * sum_k f-hat(k) * chi_k(g)

   Convolution in the group domain corresponds to pointwise multiplication
   in the Fourier domain."
  (:require [scicloj.harmonica.protocols :as p]
            [scicloj.harmonica.complex :as cx]
            [tech.v3.datatype :as dtype]
            [tech.v3.datatype.functional :as dfn]))

;; ---------------------------------------------------------------------------

(defn- forward-split
  "Forward Fourier transform on split arrays.
   f(g) * conj(chi_k(g)) = (a+bi)(c-di) = (ac+bd) + (bc-ad)i
   table-re and table-im are [k n] tensor views (rows of character table).
   f-re and f-im are [n] tensor views.
   Returns [fhat-re fhat-im] as double arrays."
  [table-re table-im f-re f-im]
  (let [k (long (first (dtype/shape table-re)))
        fhat-re (double-array k)
        fhat-im (double-array k)]
    (dotimes [i k]
      (let [chi-re (table-re i)
            chi-im (table-im i)]
        (aset fhat-re i (double (dfn/sum (dfn/+ (dfn/* f-re chi-re)
                                                (dfn/* f-im chi-im)))))
        (aset fhat-im i (double (dfn/sum (dfn/- (dfn/* f-im chi-re)
                                                (dfn/* f-re chi-im)))))))
    [fhat-re fhat-im]))

(defn- inverse-split
  "Inverse Fourier transform on split arrays.
   f(g) = (1/n) * sum_k fhat(k) * chi_k(g)
   table-re and table-im are [k n] tensor views.
   fhat-re and fhat-im are [k] double arrays.
   Returns [f-re f-im] as double arrays."
  [table-re table-im ^doubles fhat-re ^doubles fhat-im n]
  (let [scale (/ 1.0 (double n))
        k (alength fhat-re)
        f-re (double-array n)
        f-im (double-array n)]
    (dotimes [g n]
      (let [col-re (double-array k)
            col-im (double-array k)]
        (dotimes [i k]
          (aset col-re i (double ((table-re i) g)))
          (aset col-im i (double ((table-im i) g))))
        (aset f-re g (* scale (double (dfn/sum (dfn/- (dfn/* fhat-re col-re)
                                                      (dfn/* fhat-im col-im))))))
        (aset f-im g (* scale (double (dfn/sum (dfn/+ (dfn/* fhat-re col-im)
                                                      (dfn/* fhat-im col-re))))))))
    [f-re f-im]))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn fourier-transform
  "Compute the Fourier transform of a function on a finite group.

   f-vals is a ComplexTensor vector indexed by group elements.
   For abelian groups, returns a ComplexTensor vector of Fourier coefficients.

   f-hat(k) = sum_{g} f(g) * conj(chi_k(g))

   Uses the character table's ComplexTensor for direct re/im access."
  [ct f-vals]
  (let [table (:table ct)
        table-re (cx/re table)
        table-im (cx/im table)
        f-re (cx/re f-vals)
        f-im (cx/im f-vals)
        [fhat-re fhat-im] (forward-split table-re table-im f-re f-im)]
    (cx/complex-tensor fhat-re fhat-im)))

(defn inverse-fourier-transform
  "Recover a function from its Fourier coefficients.

   f-hat is a ComplexTensor vector of Fourier coefficients.
   Returns a ComplexTensor vector indexed by group elements.

   f(g) = (1/|G|) * sum_k f-hat(k) * chi_k(g)"
  [ct f-hat]
  (let [table (:table ct)
        table-re (cx/re table)
        table-im (cx/im table)
        n (p/order (:group ct))
        fhat-re (double-array (cx/re f-hat))
        fhat-im (double-array (cx/im f-hat))
        [f-re f-im] (inverse-split table-re table-im fhat-re fhat-im n)]
    (cx/complex-tensor f-re f-im)))

(defn convolve
  "Convolve two functions on a finite group via the Fourier domain.

   (f * h)(g) = sum_{x} f(x) * h(x^{-1} g)

   Computed as: IFFT(FFT(f) . FFT(h)) where . is pointwise multiplication."
  [ct f-vals h-vals]
  (let [table (:table ct)
        table-re (cx/re table)
        table-im (cx/im table)
        n (p/order (:group ct))
        ;; Forward transforms
        [fhat-re fhat-im] (forward-split table-re table-im
                                         (cx/re f-vals) (cx/im f-vals))
        [hhat-re hhat-im] (forward-split table-re table-im
                                         (cx/re h-vals) (cx/im h-vals))
        ;; Pointwise complex multiply: (a+bi)(c+di) = (ac-bd) + (ad+bc)i
        prod-re (double-array (dfn/- (dfn/* fhat-re hhat-re)
                                     (dfn/* fhat-im hhat-im)))
        prod-im (double-array (dfn/+ (dfn/* fhat-re hhat-im)
                                     (dfn/* fhat-im hhat-re)))
        ;; Inverse transform
        [result-re result-im] (inverse-split table-re table-im prod-re prod-im n)]
    (cx/complex-tensor result-re result-im)))

(defn total-variation-distance
  "Total variation distance between two probability distributions on a finite group.

   ||P - Q||_TV = (1/2) * sum_{g} |P(g) - Q(g)|

   p-vals and q-vals are vectors of real probabilities (as doubles)."
  [p-vals q-vals]
  (* 0.5 (dfn/sum (dfn/abs (dfn/- (double-array p-vals)
                                  (double-array q-vals))))))
