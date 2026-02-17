(ns scicloj.harmonica.representations-test
  (:require [clojure.test :refer [deftest testing is are]]
            [scicloj.harmonica.core :as hm]
            [scicloj.harmonica.impl.permutation :as perm]
            [scicloj.harmonica.impl.partition :as part]
            [scicloj.harmonica.impl.young-tableaux :as yt]
            [scicloj.harmonica.impl.young-orthogonal :as yo]
            [scicloj.harmonica.impl.riffle :as riffle]
            [scicloj.harmonica.representations :as rep]
            [scicloj.harmonica.protocols :as p]
            [scicloj.harmonica.impl.murnaghan-nakayama :as mn]
            [fastmath.matrix :as fm]))

;; ---------------------------------------------------------------------------
;; Helper: Frobenius norm of difference
;; ---------------------------------------------------------------------------

(defn- matrix-diff-norm
  "Frobenius norm of A - B for RealMatrix."
  [A B]
  (let [diff (.subtract ^org.apache.commons.math3.linear.RealMatrix A
                        ^org.apache.commons.math3.linear.RealMatrix B)]
    (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))))

;; ---------------------------------------------------------------------------
;; Standard Young Tableaux
;; ---------------------------------------------------------------------------

(deftest syt-count-matches-hook-length
  (testing "SYT enumeration count equals hook-length formula for n=1..7"
    (doseq [n (range 1 8)]
      (doseq [lam (part/partitions n)]
        (is (= (long (yt/hook-length-dimension lam))
               (count (yt/standard-young-tableaux lam)))
            (str "SYT count mismatch for " lam))))))

(deftest syt-dimension-sum-squared
  (testing "Σ d_λ² = n! for n=1..7"
    (doseq [n (range 1 8)]
      (let [parts (part/partitions n)
            sum-sq (reduce + (map #(let [d (yt/hook-length-dimension %)] (* d d)) parts))
            factorial (reduce *' (range 1 (inc n)))]
        (is (= factorial sum-sq)
            (str "Σ d² ≠ n! for n=" n))))))

(deftest syt-are-standard
  (testing "all enumerated SYTs have increasing rows and columns"
    (doseq [n (range 1 6)]
      (doseq [lam (part/partitions n)]
        (doseq [syt (yt/standard-young-tableaux lam)]
          ;; Rows increasing
          (doseq [row syt]
            (is (apply < row) (str "non-increasing row in " syt)))
          ;; Columns increasing
          (let [num-rows (count syt)]
            (doseq [r (range 1 num-rows)
                    c (range (count (syt r)))]
              (is (< ((syt (dec r)) c) ((syt r) c))
                  (str "non-increasing column in " syt)))))))))

;; ---------------------------------------------------------------------------
;; Adjacent transposition decomposition
;; ---------------------------------------------------------------------------

(deftest adj-decomposition-reconstructs
  (testing "decomposition reconstructs original permutation for all of S_4"
    (let [G (hm/symmetric-group 4)
          n 4]
      (doseq [sigma (p/elements G)]
        (let [swaps (perm/adjacent-transposition-decomposition sigma)
              recon (reduce (fn [p i]
                              (perm/compose p (perm/transposition n i (inc i))))
                            (perm/identity-perm n)
                            swaps)]
          (is (= sigma recon)
              (str "reconstruction failed for " sigma)))))))

(deftest adj-decomposition-identity
  (testing "identity decomposes to empty sequence"
    (doseq [n [1 2 3 4 5]]
      (is (empty? (perm/adjacent-transposition-decomposition (perm/identity-perm n)))))))

;; ---------------------------------------------------------------------------
;; Young's orthogonal form
;; ---------------------------------------------------------------------------

(deftest trace-matches-character
  (testing "tr(ρ(σ)) = χ_λ(cycle-type(σ)) for all irreps of S_n, n=3,4"
    (doseq [n [3 4]]
      (let [G (hm/symmetric-group n)
            parts (part/partitions n)]
        (doseq [lam parts]
          (let [irrep (hm/irrep lam)]
            (doseq [sigma (p/elements G)]
              (let [tr (fm/trace (hm/rep-matrix irrep sigma))
                    chi (double (mn/chi lam (perm/cycle-type sigma)))]
                (is (< (Math/abs (- tr chi)) 1e-10)
                    (str "trace mismatch: " lam " " sigma))))))))))

(deftest homomorphism-property
  (testing "ρ(g·h) = ρ(g)·ρ(h) for all pairs in S_3, all irreps"
    (let [G (hm/symmetric-group 3)
          elts (vec (p/elements G))
          parts (part/partitions 3)]
      (doseq [lam parts]
        (let [irrep (hm/irrep lam)]
          (doseq [g elts
                  h elts]
            (let [gh (perm/compose g h)
                  rho-gh (hm/rep-matrix irrep gh)
                  product (fm/mulm (hm/rep-matrix irrep g)
                                   (hm/rep-matrix irrep h))]
              (is (< (matrix-diff-norm rho-gh product) 1e-10)
                  (str "homomorphism fails: " lam " " g " " h)))))))))

(deftest matrices-are-orthogonal
  (testing "ρ(σ)ᵀρ(σ) = I for all σ ∈ S_4, all irreps"
    (let [G (hm/symmetric-group 4)
          parts (part/partitions 4)]
      (doseq [lam parts]
        (let [irrep (hm/irrep lam)
              eye (hm/rep-matrix irrep (perm/identity-perm 4))]
          (doseq [sigma (p/elements G)]
            (let [rho (hm/rep-matrix irrep sigma)
                  product (fm/mulm (fm/transpose rho) rho)]
              (is (< (matrix-diff-norm product eye) 1e-10)
                  (str "not orthogonal: " lam " " sigma)))))))))

(deftest rep-dimension-correct
  (testing "rep-dimension matches hook-length formula"
    (doseq [n [3 4 5]]
      (doseq [lam (part/partitions n)]
        (is (= (long (yt/hook-length-dimension lam))
               (hm/rep-dimension (hm/irrep lam)))
            (str "dimension mismatch for " lam))))))

;; ---------------------------------------------------------------------------
;; Matrix Fourier transform
;; ---------------------------------------------------------------------------

(deftest plancherel-identity
  (testing "Plancherel: Σ|f(σ)|² = (1/|G|) Σ d_ρ ||f̂(ρ)||²_F for S_3"
    (let [G (hm/symmetric-group 3)
          parts (part/partitions 3)
          irreps (mapv hm/irrep parts)]
      ;; Test with several functions
      (doseq [f [{[0 1 2] 1.0}
                 (fn [sigma] (double (perm/sign sigma)))
                 (fn [_] 1.0)]]
        (let [f-hats (rep/matrix-fourier-transform-all G f irreps)
              lhs (rep/plancherel-lhs G f)
              rhs (rep/plancherel-rhs G f-hats irreps)]
          (is (< (Math/abs (- lhs rhs)) 1e-10)
              (str "Plancherel fails for f")))))))

(deftest fourier-of-delta-e-is-identity
  (testing "Fourier transform of δ_e gives identity matrices"
    (let [G (hm/symmetric-group 3)
          parts (part/partitions 3)
          irreps (mapv hm/irrep parts)
          f {[0 1 2] 1.0}
          f-hats (rep/matrix-fourier-transform-all G f irreps)]
      (doseq [[ir fh] (map vector irreps f-hats)]
        (let [d (:dimension ir)
              eye (hm/rep-matrix ir (perm/identity-perm 3))]
          (is (< (matrix-diff-norm fh eye) 1e-10)
              "f̂(δ_e) should be identity"))))))

;; ---------------------------------------------------------------------------
;; Riffle shuffles
;; ---------------------------------------------------------------------------

(deftest rising-sequences-basic
  (testing "rising sequences counts"
    (is (= 1 (riffle/rising-sequences [0 1 2])) "identity has 1 rising seq")
    (is (= 3 (riffle/rising-sequences [2 1 0])) "reverse has n rising seqs")
    (is (= 2 (riffle/rising-sequences [1 0 2])) "[1 0 2] has 2 rising seqs")))

(deftest gsr-probabilities-sum-to-one
  (testing "GSR probabilities sum to 1 for S_n, various k"
    (doseq [n [3 4 5]
            k [1 2 3]]
      (let [G (hm/symmetric-group n)
            elts (vec (p/elements G))
            probs (riffle/gsr-distribution-vec elts k)
            total (reduce + (map #(aget ^doubles probs (int %)) (range (count elts))))]
        (is (< (Math/abs (- total 1.0)) 1e-10)
            (str "probabilities don't sum to 1 for n=" n " k=" k))))))

(deftest gsr-identity-has-highest-prob
  (testing "identity permutation has highest probability after 1 shuffle"
    (let [n 4
          G (hm/symmetric-group n)
          id-prob (riffle/gsr-probability (perm/identity-perm n) 1)]
      (doseq [sigma (p/elements G)]
        (is (<= (riffle/gsr-probability sigma 1) (+ id-prob 1e-10))
            (str sigma " has higher prob than identity"))))))

(deftest gsr-tv-distance-decreases
  (testing "TV distance from uniform decreases with more shuffles"
    (let [n 4
          G (hm/symmetric-group n)
          elts (vec (p/elements G))
          n-elts (count elts)
          uniform (/ 1.0 n-elts)]
      (let [tvs (mapv (fn [k]
                        (let [probs (riffle/gsr-distribution-vec elts k)]
                          (* 0.5 (reduce + (map #(Math/abs (- (aget ^doubles probs (int %)) uniform))
                                                (range n-elts))))))
                      (range 1 8))]
        (doseq [i (range (dec (count tvs)))]
          (is (< (tvs (inc i)) (tvs i))
              (str "TV distance not decreasing at k=" (+ i 2))))))))

;; ---------------------------------------------------------------------------
;; class-of
;; ---------------------------------------------------------------------------

(deftest class-of-identity
  (testing "identity element is in the identity class"
    (doseq [n [3 4 5]]
      (let [G (hm/symmetric-group n)
            cls (hm/class-of G (perm/identity-perm n))]
        (is (= (:cycle-type cls) (vec (repeat n 1))))
        (is (= (:size cls) 1))))))

(deftest class-of-cycle-type
  (testing "class-of returns correct cycle type for S_4"
    (let [G (hm/symmetric-group 4)]
      (is (= (:cycle-type (hm/class-of G [1 0 2 3])) [2 1 1]))
      (is (= (:cycle-type (hm/class-of G [1 2 3 0])) [4]))
      (is (= (:cycle-type (hm/class-of G [1 0 3 2])) [2 2])))))

(deftest class-of-dihedral
  (testing "class-of works for dihedral groups"
    (let [G (hm/dihedral-group 5)]
      (is (= (:size (hm/class-of G [:r 0])) 1))
      (is (= (:size (hm/class-of G [:s 0])) 5)))))

;; ---------------------------------------------------------------------------
;; irrep-multiplicities
;; ---------------------------------------------------------------------------

(deftest irrep-decomp-trivial
  (testing "an irrep decomposes as itself with multiplicity 1"
    (doseq [n [3 4]
            lam (part/partitions n)]
      (let [G (hm/symmetric-group n)
            decomp (hm/irrep-multiplicities G (hm/irrep lam))]
        (is (= decomp {lam 1})
            (str "irrep " lam " should decompose as itself"))))))

(deftest irrep-decomp-direct-sum
  (testing "direct sum decomposes into its components"
    (let [G (hm/symmetric-group 4)
          decomp (hm/irrep-multiplicities G
                   (hm/direct-sum (hm/irrep [3 1]) (hm/irrep [2 2])))]
      (is (= decomp {[3 1] 1, [2 2] 1})))))

(deftest irrep-decomp-tensor-product
  (testing "tensor product dimension matches decomposition"
    (let [G (hm/symmetric-group 4)
          rep31 (hm/irrep [3 1])
          tp (hm/tensor-product rep31 rep31)
          decomp (hm/irrep-multiplicities G tp)
          dim-sum (reduce + (map (fn [[lam m]]
                                   (* m (hm/hook-length-dimension lam)))
                                 decomp))]
      (is (= dim-sum (:dimension tp))))))
