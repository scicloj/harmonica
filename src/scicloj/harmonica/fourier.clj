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
            [scicloj.harmonica.characters :as ch]
            [fastmath.complex :as c]
            [tech.v3.datatype.functional :as dfn])
  (:import [fastmath.vector Vec2]))

;; ---------------------------------------------------------------------------
;; Internal helpers: split real/imaginary double arrays
;; ---------------------------------------------------------------------------

(defn- split-vec2
  "Split a vector of Vec2 into [re-array im-array] double arrays."
  [vals]
  (let [n (count vals)
        re (double-array n)
        im (double-array n)]
    (dotimes [i n]
      (let [^Vec2 v (nth vals i)]
        (aset re i (.-x v))
        (aset im i (.-y v))))
    [re im]))

(defn- join-vec2
  "Join re/im double arrays into a vector of Vec2."
  [^doubles re-arr ^doubles im-arr]
  (let [n (alength re-arr)]
    (mapv (fn [i]
            (c/complex (aget re-arr (int i))
                       (aget im-arr (int i))))
          (range n))))

;; ---------------------------------------------------------------------------
;; Core transforms on split double arrays
;; ---------------------------------------------------------------------------

(defn- forward-split
  "Forward Fourier transform on split arrays.
   f(g) * conj(chi_k(g)) = (a+bi)(c-di) = (ac+bd) + (bc-ad)i
   Returns [fhat-re fhat-im] as double arrays."
  [table-re table-im ^doubles f-re ^doubles f-im]
  (let [n (alength f-re)
        fhat-re (double-array n)
        fhat-im (double-array n)]
    (dotimes [k n]
      (let [chi-re (table-re k)
            chi-im (table-im k)]
        (aset fhat-re k (dfn/sum (dfn/+ (dfn/* f-re chi-re)
                                        (dfn/* f-im chi-im))))
        (aset fhat-im k (dfn/sum (dfn/- (dfn/* f-im chi-re)
                                        (dfn/* f-re chi-im))))))
    [fhat-re fhat-im]))

(defn- inverse-split
  "Inverse Fourier transform on split arrays.
   f(g) = (1/n) * sum_k fhat(k) * chi_k(g)
   Returns [f-re f-im] as double arrays."
  [table-re table-im ^doubles fhat-re ^doubles fhat-im n]
  (let [scale (/ 1.0 (double n))
        f-re (double-array n)
        f-im (double-array n)]
    (dotimes [g n]
      (let [col-re (double-array n)
            col-im (double-array n)]
        (dotimes [k n]
          (aset col-re k (aget ^doubles (table-re k) (int g)))
          (aset col-im k (aget ^doubles (table-im k) (int g))))
        (aset f-re g (* scale (dfn/sum (dfn/- (dfn/* fhat-re col-re)
                                              (dfn/* fhat-im col-im)))))
        (aset f-im g (* scale (dfn/sum (dfn/+ (dfn/* fhat-re col-im)
                                              (dfn/* fhat-im col-re)))))))
    [f-re f-im]))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn fourier-transform
  "Compute the Fourier transform of a function on a finite group.

   f-vals is a vector of complex values (Vec2) indexed by group elements.
   For abelian groups, returns a vector of complex Fourier coefficients.

   f-hat(k) = sum_{g} f(g) * conj(chi_k(g))

   Uses split real/imaginary double arrays from the character table
   for vectorized computation via dtype-next."
  [ct f-vals]
  (let [{:keys [table-re table-im]} ct
        [f-re f-im] (split-vec2 f-vals)
        [fhat-re fhat-im] (forward-split table-re table-im f-re f-im)]
    (join-vec2 fhat-re fhat-im)))

(defn inverse-fourier-transform
  "Recover a function from its Fourier coefficients.

   f-hat is a vector of complex Fourier coefficients.
   Returns a vector of complex values indexed by group elements.

   f(g) = (1/|G|) * sum_k f-hat(k) * chi_k(g)

   Uses split real/imaginary double arrays for vectorized computation."
  [ct f-hat]
  (let [{:keys [table-re table-im group]} ct
        n (p/order group)
        [fhat-re fhat-im] (split-vec2 f-hat)
        [f-re f-im] (inverse-split table-re table-im fhat-re fhat-im n)]
    (join-vec2 f-re f-im)))

(defn convolve
  "Convolve two functions on a finite group via the Fourier domain.

   (f * h)(g) = sum_{x} f(x) * h(x^{-1} g)

   Computed as: IFFT(FFT(f) . FFT(h)) where . is pointwise multiplication.
   Uses split arrays throughout to avoid Vec2 intermediates."
  [ct f-vals h-vals]
  (let [{:keys [table-re table-im group]} ct
        n (p/order group)
        ;; Split inputs
        [f-re f-im] (split-vec2 f-vals)
        [h-re h-im] (split-vec2 h-vals)
        ;; Forward transforms (on double arrays)
        [fhat-re fhat-im] (forward-split table-re table-im f-re f-im)
        [hhat-re hhat-im] (forward-split table-re table-im h-re h-im)
        ;; Pointwise complex multiply: (a+bi)(c+di) = (ac-bd) + (ad+bc)i
        prod-re (double-array (dfn/- (dfn/* fhat-re hhat-re)
                                     (dfn/* fhat-im hhat-im)))
        prod-im (double-array (dfn/+ (dfn/* fhat-re hhat-im)
                                     (dfn/* fhat-im hhat-re)))
        ;; Inverse transform (on double arrays)
        [result-re result-im] (inverse-split table-re table-im prod-re prod-im n)]
    (join-vec2 result-re result-im)))

(defn total-variation-distance
  "Total variation distance between two probability distributions on a finite group.

   ||P - Q||_TV = (1/2) * sum_{g} |P(g) - Q(g)|

   p-vals and q-vals are vectors of real probabilities (as doubles)."
  [p-vals q-vals]
  (* 0.5 (dfn/sum (dfn/abs (dfn/- (double-array p-vals)
                                  (double-array q-vals))))))
