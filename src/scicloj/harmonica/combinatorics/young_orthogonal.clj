(ns scicloj.harmonica.combinatorics.young-orthogonal
  "Young's orthogonal form for irreducible representations of S_n.

   For each partition λ of n, we construct the irreducible representation
   ρ_λ : S_n → GL(d_λ) where d_λ = number of standard Young tableaux of shape λ.

   The representation is defined by its action on adjacent transposition
   generators s_i = (i, i+1) for i = 1, ..., n-1.

   For each s_i, the matrix in the SYT basis has a simple structure determined
   by the axial distances of the values i and i+1 in each tableau."
  (:require [scicloj.harmonica.combinatorics.young-tableaux :as yt]
            [scicloj.harmonica.combinatorics.permutation :as perm]
            [fastmath.matrix :as fm]))

(defn- syt-index
  "Build a map from SYT → index in the basis ordering."
  [syts]
  (into {} (map-indexed (fn [idx t] [t idx]) syts)))

(defn- swap-values
  "Swap values v1 and v2 in a SYT (vector of row vectors).
   Returns the modified SYT (which may or may not be standard)."
  [syt v1 v2]
  (mapv (fn [row]
          (mapv (fn [x]
                  (cond (= x v1) v2
                        (= x v2) v1
                        :else x))
                row))
        syt))

(defn- is-standard?
  "Check if a tableau (vector of row vectors) is a standard Young tableau:
   increasing along rows and down columns."
  [syt]
  (let [num-rows (count syt)]
    (and
     ;; Rows increasing
     (every? (fn [row] (apply < row)) syt)
     ;; Columns increasing
     (every? true?
             (for [r (range 1 num-rows)
                   c (range (count (syt r)))]
               (< ((syt (dec r)) c) ((syt r) c)))))))

(defn generator-matrix
  "Compute the d×d matrix for adjacent transposition s_i (swapping values
   i and i+1, 1-indexed) in the irrep indexed by partition lambda.

   syts: vector of all SYTs of shape lambda (the basis)
   syt-idx: map from SYT to basis index
   i: the transposition index (1-indexed, swaps values i and i+1)

   Returns a fastmath RealMatrix (double[][])."
  [syts syt-idx i]
  (let [d (count syts)
        mat (make-array Double/TYPE d d)]
    (dotimes [t d]
      (let [syt (syts t)
            rho (yt/axial-distance syt i (inc i))
            inv-rho (/ 1.0 (double rho))]
        (if (or (= rho 1) (= rho -1))
          ;; Same row (ρ=1) or same column (ρ=-1): diagonal entry only
          (aset ^"[[D" mat t t inv-rho)
          ;; Swap produces valid SYT: find it and fill 2×2 block
          (let [swapped (swap-values syt i (inc i))
                t2 (syt-idx swapped)]
            (when (and t2 (< t t2)) ;; only fill once per pair
              (let [off-diag (Math/sqrt (- 1.0 (* inv-rho inv-rho)))]
                (aset ^"[[D" mat t t inv-rho)
                (aset ^"[[D" mat t t2 off-diag)
                (aset ^"[[D" mat t2 t off-diag)
                (aset ^"[[D" mat t2 t2 (- inv-rho))))))))
    (fm/array2d->RealMatrix mat)))

(defn irrep-generators
  "Compute all generator matrices for the irrep of S_n indexed by partition lambda.
   Returns a map with:
     :lambda     - the partition
     :dimension  - d_lambda (number of SYTs)
     :syts       - vector of standard Young tableaux (the basis)
     :generators - vector of (n-1) matrices, one for each s_i (i=1..n-1)"
  [lambda]
  (let [n (reduce + 0 lambda)
        syts (yt/standard-young-tableaux lambda)
        syt-idx (syt-index syts)
        generators (mapv (fn [i]
                           (generator-matrix syts syt-idx i))
                         (range 1 n))]
    {:lambda lambda
     :dimension (count syts)
     :syts syts
     :generators generators}))

(defn representation-matrix
  "Compute the representation matrix ρ_λ(σ) for a permutation σ in the
   irrep indexed by partition lambda.

   Decomposes σ into adjacent transpositions and multiplies the corresponding
   generator matrices.

   irrep: result of irrep-generators
   sigma: permutation vector (0-indexed one-line notation)"
  [irrep sigma]
  (let [d (:dimension irrep)
        generators (:generators irrep)
        adj-indices (perm/adjacent-transposition-decomposition sigma)]
    (if (empty? adj-indices)
      ;; Identity permutation → identity matrix
      (fm/eye d true)
      ;; sigma = s_{i_1} ∘ s_{i_2} ∘ ... ∘ s_{i_k}
      ;; adj-indices are 0-indexed: index j means swap positions j,j+1
      ;; generators[j] is the matrix for s_{j+1} (1-indexed) = swap of positions j,j+1
      (reduce fm/mulm
              (mapv (fn [j] (generators j)) adj-indices)))))
