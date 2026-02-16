(ns scicloj.reel.core
  "Public API for reel — computational group theory and representation theory.

   Groups:
     cyclic-group     - create Z/nZ
     symmetric-group  - create S_n

   Group operations:
     op, inv, id      - group operation, inverse, identity
     elements, order  - enumerate elements, group size
     conjugacy-classes - structural decomposition

   Permutations:
     cycles           - cycle decomposition
     cycle-type       - partition (cycle type) of a permutation
     sign             - permutation sign (+1 or -1)
     adjacent-transposition-decomposition - decompose into adjacent swaps

   Partitions:
     partitions       - all partitions of n

   Young tableaux:
     standard-young-tableaux - enumerate SYTs of a partition
     hook-length-dimension   - dimension via hook-length formula

   Characters:
     character-table  - compute the character table
     character-inner-product - inner product of class functions

   Representations:
     irrep            - irreducible representation of S_n
     rep-matrix       - representation matrix for a group element
     rep-dimension    - dimension of a representation
     rep-character    - character value (trace of matrix)
     rep-generators   - generator matrices for adjacent transpositions
     frobenius-norm-sq - squared Frobenius norm
     frobenius-norm   - Frobenius norm
     matrix-fourier-transform     - f̂(ρ) = Σ f(σ)·ρ(σ)
     matrix-fourier-transform-all - transform for all irreps

   Riffle shuffles:
     gsr-probability      - GSR probability of a permutation after k shuffles
     rising-sequences     - number of rising sequences of a permutation
     gsr-distribution-vec - GSR distribution as double array

   Fourier analysis:
     fourier-transform         - transform a function on a group
     inverse-fourier-transform - recover function from coefficients
     convolve                  - convolution via Fourier domain
     total-variation-distance  - distance between distributions"
  (:require [scicloj.reel.protocols :as p]
            [scicloj.reel.impl.cyclic :as cyclic]
            [scicloj.reel.impl.symmetric :as symmetric]
            [scicloj.reel.impl.permutation :as perm]
            [scicloj.reel.impl.partition :as part]
            [scicloj.reel.impl.young-tableaux :as yt]
            [scicloj.reel.impl.riffle :as riffle]
            [scicloj.reel.characters :as ch]
            [scicloj.reel.representations :as rep]
            [scicloj.reel.fourier :as fourier]))

;; ---------------------------------------------------------------------------
;; Group constructors
;; ---------------------------------------------------------------------------

(def cyclic-group
  "Create the cyclic group Z/nZ of order n.
   Elements are integers 0, 1, ..., n-1.
   The group operation is addition mod n."
  cyclic/cyclic-group)

(def symmetric-group
  "Create the symmetric group S_n — all permutations of {0, ..., n-1}.
   Elements are 0-indexed one-line notation vectors.
   The group operation is composition (right-to-left)."
  symmetric/symmetric-group)

;; ---------------------------------------------------------------------------
;; Permutation utilities
;; ---------------------------------------------------------------------------

(def cycles
  "Cycle decomposition of a permutation.
   Returns a vector of cycles (each a vector), omitting fixed points."
  perm/cycles)

(def cycle-type
  "The cycle type of a permutation as a partition (descending sorted).
   Includes 1-cycles (fixed points)."
  perm/cycle-type)

(def sign
  "The sign of a permutation: +1 for even, -1 for odd."
  perm/sign)

(def adjacent-transposition-decomposition
  "Decompose a permutation into adjacent transpositions s_i = (i, i+1).
   Returns a vector of indices [i_1, i_2, ...] such that
   sigma = s_{i_1} ∘ s_{i_2} ∘ ... ∘ s_{i_k}."
  perm/adjacent-transposition-decomposition)

;; ---------------------------------------------------------------------------
;; Partitions
;; ---------------------------------------------------------------------------

(def partitions
  "All partitions of n, as descending vectors."
  part/partitions)

;; ---------------------------------------------------------------------------
;; Young tableaux
;; ---------------------------------------------------------------------------

(def standard-young-tableaux
  "All standard Young tableaux of shape lambda (a partition).
   Each SYT is a vector of row vectors."
  yt/standard-young-tableaux)

(def hook-length-dimension
  "Dimension of the irrep of S_n for partition lambda, via the hook-length formula."
  yt/hook-length-dimension)

;; ---------------------------------------------------------------------------
;; Group protocol functions
;; ---------------------------------------------------------------------------

(def op
  "Group operation: combine two elements."
  p/op)

(def inv
  "Group inverse of an element."
  p/inv)

(def id
  "The identity element of a group."
  p/id)

(def elements
  "Sequence of all elements of a finite group."
  p/elements)

(def order
  "Number of elements in a finite group."
  p/order)

(def conjugacy-classes
  "Conjugacy classes of a finite group."
  p/conjugacy-classes)

;; ---------------------------------------------------------------------------
;; Characters
;; ---------------------------------------------------------------------------

(def character-table
  "Compute the character table of a finite group."
  ch/character-table)

(def character-inner-product
  "Inner product of two class functions."
  ch/character-inner-product)

;; ---------------------------------------------------------------------------
;; Representations
;; ---------------------------------------------------------------------------

(def irrep
  "Construct an irreducible representation of S_n from a partition.
   Returns an opaque value to pass to rep-matrix, etc."
  rep/irrep)

(def rep-matrix
  "Compute the representation matrix ρ(σ) for a group element."
  rep/rep-matrix)

(def rep-dimension
  "Dimension of a representation."
  rep/rep-dimension)

(def rep-character
  "Character value χ(σ) = tr(ρ(σ))."
  rep/rep-character)

(def rep-generators
  "Generator matrices for adjacent transpositions s_1, ..., s_{n-1}."
  rep/rep-generators)

(def frobenius-norm-sq
  "Squared Frobenius norm: ||M||²_F = tr(M Mᵀ)."
  rep/frobenius-norm-sq)

(def frobenius-norm
  "Frobenius norm: ||M||_F = sqrt(tr(M Mᵀ))."
  rep/frobenius-norm)

(def matrix-fourier-transform
  "Matrix-valued Fourier transform: f̂(ρ) = Σ f(σ)·ρ(σ)."
  rep/matrix-fourier-transform)

(def matrix-fourier-transform-all
  "Matrix Fourier transform for all irreps."
  rep/matrix-fourier-transform-all)

;; ---------------------------------------------------------------------------
;; Riffle shuffles
;; ---------------------------------------------------------------------------

(def gsr-probability
  "Probability of permutation σ after k riffle shuffles (GSR model).
   Q_a(σ) = C(a+n-r(σ), n) / a^n where a=2^k, r = rising sequences."
  riffle/gsr-probability)

(def rising-sequences
  "Number of rising sequences of a permutation."
  riffle/rising-sequences)

(def gsr-distribution-vec
  "GSR distribution as a double array for a vector of permutations."
  riffle/gsr-distribution-vec)

;; ---------------------------------------------------------------------------
;; Fourier analysis
;; ---------------------------------------------------------------------------

(def fourier-transform
  "Fourier transform of a function on a finite group."
  fourier/fourier-transform)

(def inverse-fourier-transform
  "Recover a function from its Fourier coefficients."
  fourier/inverse-fourier-transform)

(def convolve
  "Convolve two functions on a finite group via the Fourier domain."
  fourier/convolve)

(def total-variation-distance
  "Total variation distance between two distributions."
  fourier/total-variation-distance)
