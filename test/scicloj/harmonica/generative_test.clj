(ns scicloj.harmonica.generative-test
  "Property-based tests using test.check.
   Tests invariants that must hold for all groups, representations, etc."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [scicloj.harmonica :as hm]
            [scicloj.harmonica.linalg.complex :as cx]
            [scicloj.harmonica.analysis.characters :as ch]
            [fastmath.matrix :as fm]))

;; ---------------------------------------------------------------------------
;; Generators
;; ---------------------------------------------------------------------------

(def gen-small-n
  "Generate n in [2, 5] for symmetric groups (keep tests fast)."
  (gen/choose 2 5))

(def gen-cyclic-n
  "Generate n in [2, 12] for cyclic groups."
  (gen/choose 2 12))

(def gen-dihedral-n
  "Generate n in [3, 8] for dihedral groups."
  (gen/choose 3 8))

(defn gen-element
  "Generator for a random element of group G."
  [G]
  (gen/elements (vec (hm/elements G))))

(defn gen-elements-triple
  "Generator for three random elements of group G."
  [G]
  (gen/tuple (gen-element G) (gen-element G) (gen-element G)))

;; ---------------------------------------------------------------------------
;; Group axiom properties
;; ---------------------------------------------------------------------------

(defn associativity-property [G]
  (prop/for-all [[a b c] (gen-elements-triple G)]
    (= (hm/op G (hm/op G a b) c)
       (hm/op G a (hm/op G b c)))))

(defn identity-property [G]
  (prop/for-all [g (gen-element G)]
    (and (= g (hm/op G g (hm/id G)))
         (= g (hm/op G (hm/id G) g)))))

(defn inverse-property [G]
  (prop/for-all [g (gen-element G)]
    (and (= (hm/id G) (hm/op G g (hm/inv G g)))
         (= (hm/id G) (hm/op G (hm/inv G g) g)))))

(defn closure-property [G]
  (let [elt-set (set (hm/elements G))]
    (prop/for-all [[a b _] (gen-elements-triple G)]
      (contains? elt-set (hm/op G a b)))))

(defspec symmetric-group-associativity 50
  (prop/for-all [n gen-small-n]
    (let [G (hm/symmetric-group n)]
      (:pass? (tc/quick-check 20 (associativity-property G))))))

(defspec symmetric-group-identity 50
  (prop/for-all [n gen-small-n]
    (let [G (hm/symmetric-group n)]
      (:pass? (tc/quick-check 20 (identity-property G))))))

(defspec symmetric-group-inverse 50
  (prop/for-all [n gen-small-n]
    (let [G (hm/symmetric-group n)]
      (:pass? (tc/quick-check 20 (inverse-property G))))))

(defspec cyclic-group-axioms 50
  (prop/for-all [n gen-cyclic-n]
    (let [G (hm/cyclic-group n)]
      (and (:pass? (tc/quick-check 20 (associativity-property G)))
           (:pass? (tc/quick-check 20 (identity-property G)))
           (:pass? (tc/quick-check 20 (inverse-property G)))
           (:pass? (tc/quick-check 20 (closure-property G)))))))

(defspec dihedral-group-axioms 30
  (prop/for-all [n gen-dihedral-n]
    (let [G (hm/dihedral-group n)]
      (and (:pass? (tc/quick-check 20 (associativity-property G)))
           (:pass? (tc/quick-check 20 (identity-property G)))
           (:pass? (tc/quick-check 20 (inverse-property G)))
           (:pass? (tc/quick-check 20 (closure-property G)))))))

;; ---------------------------------------------------------------------------
;; Character table properties
;; ---------------------------------------------------------------------------

(defspec character-row-orthogonality 20
  (prop/for-all [n (gen/choose 2 5)]
    (let [G (hm/symmetric-group n)
          ct (hm/character-table G)
          table (:table ct)
          sizes (:class-sizes ct)
          order (hm/order G)
          k (count (:irrep-labels ct))]
      (every? true?
              (for [i (range k) j (range (inc i) k)]
                (let [ip (cx/cabs (hm/character-inner-product
                                    (table i) (table j) sizes order))]
                  (< ip 1e-8)))))))

(defspec dimension-sum-of-squares 20
  ;; Σ d_λ² = |G| for any finite group
  (prop/for-all [n (gen/choose 2 5)]
    (let [G (hm/symmetric-group n)
          ct (hm/character-table G)
          table (:table ct)
          ;; First column of character table = χ_λ(e) = d_λ
          ;; For S_n, identity is the class with cycle type [1 1 ... 1]
          ;; which is the last column
          dims (mapv (fn [i]
                       (let [row-re (cx/re (table i))]
                         ;; Identity class is the one with cycle type [1,...,1]
                         ;; which has index matching the trivial partition
                         ;; Actually just compute dimension from hook-length
                         (long (Math/round (cx/re ((table i) 0))))))
                     (range (count (:irrep-labels ct))))
          sum-sq (reduce + (map #(* % %) dims))]
      (= sum-sq (hm/order G)))))

;; ---------------------------------------------------------------------------
;; Representation homomorphism property
;; ---------------------------------------------------------------------------

(defspec irrep-is-homomorphism 30
  ;; ρ(g·h) = ρ(g)·ρ(h) for random irreps of S_n
  (prop/for-all [n (gen/choose 2 4)]
    (let [G (hm/symmetric-group n)
          parts (hm/partitions n)]
      (every? true?
              (for [lam parts]
                (let [rep (hm/irrep lam)
                      elts (vec (hm/elements G))
                      ;; Pick 10 random pairs
                      pairs (take 10 (repeatedly #(vector (rand-nth elts) (rand-nth elts))))]
                  (every? (fn [[g h]]
                            (let [rho-gh (hm/rep-matrix rep (hm/op G g h))
                                  rho-g-times-rho-h (fm/mulm (hm/rep-matrix rep g)
                                                             (hm/rep-matrix rep h))
                                  diff (fm/sub rho-gh rho-g-times-rho-h)
                                  err (Math/sqrt (hm/frobenius-norm-sq diff))]
                              (< err 1e-10)))
                          pairs)))))))

;; ---------------------------------------------------------------------------
;; Character is a class function
;; ---------------------------------------------------------------------------

(defspec character-is-class-function 20
  ;; χ(g) = χ(h g h⁻¹) for all g, h
  (prop/for-all [n (gen/choose 2 4)]
    (let [G (hm/symmetric-group n)
          parts (hm/partitions n)]
      (every? true?
              (for [lam parts]
                (let [rep (hm/irrep lam)
                      elts (vec (hm/elements G))
                      ;; Pick 15 random pairs
                      pairs (take 15 (repeatedly #(vector (rand-nth elts) (rand-nth elts))))]
                  (every? (fn [[g h]]
                            (let [conj-g (hm/op G (hm/op G h g) (hm/inv G h))
                                  chi-g (hm/rep-character rep g)
                                  chi-conj (hm/rep-character rep conj-g)]
                              (< (Math/abs (- chi-g chi-conj)) 1e-10)))
                          pairs)))))))

;; ---------------------------------------------------------------------------
;; Burnside's lemma
;; ---------------------------------------------------------------------------

(defspec burnside-matches-orbits 20
  ;; burnside-count should equal the number of orbits
  (prop/for-all [n (gen/choose 2 5)]
    (let [G (hm/symmetric-group n)
          domain (vec (range n))
          act (fn [sigma x] (sigma x))
          b-count (hm/burnside-count G act domain)
          orbit-count (count (hm/orbits G act domain))]
      (= b-count orbit-count))))

;; ---------------------------------------------------------------------------
;; Fourier round-trip for random signals on cyclic groups
;; ---------------------------------------------------------------------------

(defspec fourier-round-trip-cyclic 20
  (prop/for-all [n (gen/choose 2 16)]
    (let [G (hm/cyclic-group n)
          ct (hm/character-table G)
          signal (cx/complex-tensor-real
                   (mapv (fn [_] (double (- (rand-int 201) 100))) (range n)))
          f-hat (hm/fourier-transform ct signal)
          recovered (hm/inverse-fourier-transform ct f-hat)
          max-err (apply max (vec (cx/cabs (cx/csub recovered signal))))]
      (< max-err 1e-8))))

;; ---------------------------------------------------------------------------
;; Product group axioms
;; ---------------------------------------------------------------------------

(defspec product-group-axioms 20
  (prop/for-all [[m n] (gen/tuple (gen/choose 2 5) (gen/choose 2 5))]
    (let [G (hm/product-group (hm/cyclic-group m) (hm/cyclic-group n))]
      (and (:pass? (tc/quick-check 15 (associativity-property G)))
           (:pass? (tc/quick-check 15 (identity-property G)))
           (:pass? (tc/quick-check 15 (inverse-property G)))))))

;; ---------------------------------------------------------------------------
;; Permutation sign homomorphism
;; ---------------------------------------------------------------------------

(defspec sign-is-homomorphism 30
  ;; sign(g·h) = sign(g) * sign(h)
  (prop/for-all [n (gen/choose 2 5)]
    (let [G (hm/symmetric-group n)
          elts (vec (hm/elements G))
          pairs (take 20 (repeatedly #(vector (rand-nth elts) (rand-nth elts))))]
      (every? (fn [[g h]]
                (= (hm/sign (hm/op G g h))
                   (* (hm/sign g) (hm/sign h))))
              pairs))))

;; ---------------------------------------------------------------------------
;; Branching rule: multiplicities are non-negative and dimensions match
;; ---------------------------------------------------------------------------

(defspec branching-rule-dimensions-match 20
  ;; Σ m_μ · dim(μ) = dim(λ) when restricting from S_n to S_{n-1}
  (prop/for-all [n (gen/choose 3 5)]
    (let [parts (hm/partitions n)]
      (every? (fn [lam]
                (let [d-lam (hm/hook-length-dimension lam)
                      br (hm/branching-rule lam)
                      sum-d (reduce + (map (fn [[mu m]]
                                             (* m (hm/hook-length-dimension mu)))
                                           br))]
                  (= d-lam sum-d)))
              parts))))
