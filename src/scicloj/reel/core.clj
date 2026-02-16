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

   Partitions:
     partitions       - all partitions of n

   Characters:
     character-table  - compute the character table
     character-inner-product - inner product of class functions

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
            [scicloj.reel.characters :as ch]
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

;; ---------------------------------------------------------------------------
;; Partitions
;; ---------------------------------------------------------------------------

(def partitions
  "All partitions of n, as descending vectors."
  part/partitions)

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
