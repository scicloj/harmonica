(ns scicloj.reel.core
  "Public API for reel — computational group theory and representation theory.

   Groups:
     cyclic-group     - create Z/nZ

   Group operations:
     op, inv, id      - group operation, inverse, identity
     elements, order  - enumerate elements, group size
     conjugacy-classes - structural decomposition

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
