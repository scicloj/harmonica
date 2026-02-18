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
            [scicloj.harmonica.linalg.complex :as cx]
            [tech.v3.datatype :as dtype]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.tensor :as tensor]))

;; ---------------------------------------------------------------------------

(defn- forward-split
  "Forward Fourier transform on split arrays.
   f(g) * conj(chi_k(g)) = (a+bi)(c-di) = (ac+bd) + (bc-ad)i
   table-re and table-im are [k n] tensor views (rows of character table).
   f-re and f-im are [n] tensor views.
   Returns [fhat-re fhat-im] as realized dtype buffers."
  [table-re table-im f-re f-im]
  (let [k (long (first (dtype/shape table-re)))]
    [(dtype/clone
      (dtype/make-reader :float64 k
                         (let [chi-re (table-re idx)
                               chi-im (table-im idx)]
                           (dfn/sum (dfn/+ (dfn/* f-re chi-re)
                                           (dfn/* f-im chi-im))))))
     (dtype/clone
      (dtype/make-reader :float64 k
                         (let [chi-re (table-re idx)
                               chi-im (table-im idx)]
                           (dfn/sum (dfn/- (dfn/* f-im chi-re)
                                           (dfn/* f-re chi-im))))))]))

(defn- inverse-split
  "Inverse Fourier transform on split arrays.
   f(g) = (1/n) * sum_k fhat(k) * chi_k(g)
   table-re and table-im are [k n] tensor views.
   fhat-re and fhat-im are [k] dtype buffers.
   Returns [f-re f-im] as realized dtype buffers."
  [table-re table-im fhat-re fhat-im n]
  (let [scale (/ 1.0 (double n))]
    [(dtype/clone
      (dtype/make-reader :float64 n
                         (let [col-re (tensor/select table-re :all idx)
                               col-im (tensor/select table-im :all idx)]
                           (* scale (dfn/sum (dfn/- (dfn/* fhat-re col-re)
                                                    (dfn/* fhat-im col-im)))))))
     (dtype/clone
      (dtype/make-reader :float64 n
                         (let [col-re (tensor/select table-re :all idx)
                               col-im (tensor/select table-im :all idx)]
                           (* scale (dfn/sum (dfn/+ (dfn/* fhat-re col-im)
                                                    (dfn/* fhat-im col-re)))))))]))

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
        [f-re f-im] (inverse-split table-re table-im
                                   (cx/re f-hat) (cx/im f-hat) n)]
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
        ;; Realize because inverse-split reads each element n times
        prod-re (dtype/clone (dfn/- (dfn/* fhat-re hhat-re)
                                    (dfn/* fhat-im hhat-im)))
        prod-im (dtype/clone (dfn/+ (dfn/* fhat-re hhat-im)
                                    (dfn/* fhat-im hhat-re)))
        ;; Inverse transform
        [result-re result-im] (inverse-split table-re table-im prod-re prod-im n)]
    (cx/complex-tensor result-re result-im)))

(defn total-variation-distance
  "Total variation distance between two probability distributions on a finite group.

   ||P - Q||_TV = (1/2) * sum_{g} |P(g) - Q(g)|

   p-vals and q-vals are numeric collections of real probabilities."
  [p-vals q-vals]
  (* 0.5 (dfn/sum (dfn/abs (dfn/- p-vals q-vals)))))
