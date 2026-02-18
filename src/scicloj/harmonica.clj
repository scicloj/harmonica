(ns scicloj.harmonica
  "Public API for harmonica — computational group theory and representation theory.

   Groups:
     cyclic-group     - create Z/nZ
     symmetric-group  - create S_n
     dihedral-group   - create D_n
     product-group    - create G₁ × G₂

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
     partition-conjugate - conjugate (transpose) of a partition

   Young tableaux:
     standard-young-tableaux - enumerate SYTs of a partition
     hook-length-dimension   - dimension via hook-length formula

   Characters:
     character-table  - compute the character table

   Fourier analysis:
     fourier-transform        - transform a function on the group
     inverse-fourier-transform - recover a function from Fourier coefficients
     convolve                 - convolution via pointwise multiply in Fourier domain

   Representations:
     irrep, rep-matrix, rep-dimension, rep-character
     tensor-product, direct-sum, restrict-rep, induce-rep

   Group actions:
     orbit, orbits, stabilizer, fixed-points
     burnside-count, cycle-index, polya-count
     subset-action, coloring-action

   Visualization:
     young-diagram-svg, hook-diagram-svg, syt-svg
     cycle-diagram-svg, cayley-table-svg, cayley-graph-svg

   Riffle shuffles:
     riffle-shuffle-distribution - GSR distribution for k shuffles of n cards
     total-variation-distance    - TV distance to uniform"
  (:require [scicloj.harmonica.protocols :as p]
            [scicloj.harmonica.group.cyclic :as cyclic]
            [scicloj.harmonica.group.symmetric :as symmetric]
            [scicloj.harmonica.group.dihedral :as dihedral]
            [scicloj.harmonica.group.product :as product]
            [scicloj.harmonica.combinatorics.permutation :as perm]
            [scicloj.harmonica.combinatorics.partition :as part]
            [scicloj.harmonica.combinatorics.young-tableaux :as yt]
            [scicloj.harmonica.combinatorics.riffle :as riffle]
            [scicloj.harmonica.analysis.characters :as ch]
            [scicloj.harmonica.analysis.representations :as rep]
            [scicloj.harmonica.analysis.fourier :as fourier]
            [scicloj.harmonica.action :as action]
            [scicloj.harmonica.svg :as svg]))

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

(def dihedral-group
  "Create the dihedral group D_n — symmetries of a regular n-gon.
   Order 2n. Elements are [:r k] (rotations) and [:s k] (reflections)."
  dihedral/dihedral-group)

(def product-group
  "Create the direct product G₁ × G₂ of two finite groups.
   Elements are pairs [g h]."
  product/product-group)

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

(def partition-conjugate
  "Conjugate (transpose) of a partition — swap rows and columns in the Young diagram."
  part/conjugate)

(def identity-perm
  "The identity permutation of size n: [0 1 2 ... n-1]."
  perm/identity-perm)

(def transposition
  "Create a transposition permutation that swaps positions i and j.
   (transposition n i j) returns a permutation of size n."
  perm/transposition)

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

(def tensor-product
  "Tensor product ρ₁ ⊗ ρ₂ of two representations.
   (ρ₁⊗ρ₂)(g) = ρ₁(g) ⊗ ρ₂(g) (Kronecker product)."
  rep/tensor-product)

(def direct-sum
  "Direct sum ρ₁ ⊕ ρ₂ of two representations.
   (ρ₁⊕ρ₂)(g) is block-diagonal with ρ₁(g) and ρ₂(g)."
  rep/direct-sum)

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

(def class-of
  "Return the conjugacy class of element g in group G."
  rep/class-of)

(def irrep-multiplicities
  "Decompose a representation into irreducible components.
   Returns a map from partition (irrep label) to multiplicity."
  rep/irrep-multiplicities)

(def restrict-rep
  "Restrict a representation of S_n to the subgroup S_k (fixing points >= k).
   (restrict-rep rep n k) returns a representation of S_k."
  rep/restrict-rep)

(def induce-rep
  "Induce a representation of S_k to S_n (standard embedding).
   (induce-rep rep k n) returns a representation of S_n with dimension (n!/k!)·d."
  rep/induce-rep)

(def branching-rule
  "Compute how an irrep of S_n decomposes when restricted to S_{n-1}.
   Returns a map from partition of (n-1) to multiplicity."
  rep/branching-rule)

(def embed-perm
  "Embed a permutation of S_k into S_n by fixing points >= k."
  rep/embed-perm)

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
;; Group actions
;; ---------------------------------------------------------------------------

(def orbit
  "The orbit of point x under the action of group G.
   act is a function (act g x) → x'."
  action/orbit)

(def orbits
  "Partition domain into orbits under the action of group G.
   Returns a vector of sets."
  action/orbits)

(def fixed-points
  "The set of points in domain fixed by group element g.
   (fixed-points act g domain)"
  action/fixed-points)

(def stabilizer
  "The stabilizer of point x: group elements that fix x."
  action/stabilizer)

(def burnside-count
  "Number of orbits via Burnside's lemma:
   |orbits| = (1/|G|) Σ_{g∈G} |Fix(g)|"
  action/burnside-count)

(def cycle-index
  "The cycle index of a group action.
   Returns a map from cycle-type partition to coefficient."
  action/cycle-index)

(def polya-count
  "Number of distinct colorings with k colors via Pólya enumeration."
  action/polya-count)

(def subset-action
  "Induced action on k-element subsets of a domain.
   Returns {:act act-fn :domain all-subsets}."
  action/subset-action)

(def coloring-action
  "Induced action on k-colorings of n positions.
   Returns {:act act-fn :domain all-colorings}."
  action/coloring-action)

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

;; ---------------------------------------------------------------------------
;; SVG visualizations
;; ---------------------------------------------------------------------------

(def young-diagram-svg
  "Render a partition as an SVG Young diagram (hiccup).
   Options: :cell-size, :fill, :stroke."
  svg/young-diagram-svg)

(def young-hooks-svg
  "Render a Young diagram with hook lengths in each cell (hiccup).
   Options: :cell-size."
  svg/young-hooks-svg)

(def syt-svg
  "Render a standard Young tableau as SVG (hiccup).
   Options: :cell-size."
  svg/syt-svg)

(def cycle-diagram-svg
  "Render a permutation as a cycle diagram SVG (hiccup).
   Options: :radius."
  svg/cycle-diagram-svg)

(def cayley-table-svg
  "Render a group Cayley table as an SVG grid with colored cells (hiccup).
   Options: :cell-size."
  svg/cayley-table-svg)

(def cayley-graph-svg
  "Render a Cayley graph as SVG (hiccup).
   Vertices = group elements in a circle, directed edges colored by generator.
   Options: :radius, :node-r, :labels, :colors."
  svg/cayley-graph-svg)
