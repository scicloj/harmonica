(ns scicloj.reel.representations
  "Representations of finite groups.

   A representation ρ : G → GL(V) is a group homomorphism from a group G
   to the general linear group of a vector space V. For finite groups,
   every irreducible representation is equivalent to a unitary one, and for
   symmetric groups all irreducible representations can be realized over the reals.

   This namespace provides:
   - irrep: construct an irreducible representation from a partition
   - rep-matrix: compute ρ(σ) for a group element σ
   - rep-dimension: dimension of the representation
   - rep-character: character value χ(σ) = tr(ρ(σ))
   - frobenius-norm-sq: ||M||²_F = tr(M Mᵀ)"
  (:require [scicloj.reel.protocols :as p]
            [scicloj.reel.impl.young-orthogonal :as yo]
            [scicloj.reel.impl.young-tableaux :as yt]
            [fastmath.matrix :as fm]))

(defn irrep
  "Construct the irreducible representation of S_n indexed by partition lambda.
   Returns an opaque representation value that can be passed to rep-matrix, etc.

   The representation uses Young's orthogonal form: the basis vectors are
   standard Young tableaux, and the matrices are orthogonal."
  [lambda]
  (yo/irrep-generators lambda))

(defn rep-matrix
  "Compute the representation matrix ρ(σ) for group element sigma.
   Returns a fastmath RealMatrix.

   rep: a representation (from irrep, tensor-product, or direct-sum)
   sigma: a group element"
  [rep sigma]
  (if-let [mfn (:matrix-fn rep)]
    (mfn sigma)
    (yo/representation-matrix rep sigma)))

(defn rep-dimension
  "Dimension of a representation."
  [rep]
  (:dimension rep))

(defn rep-character
  "Character value χ(σ) = tr(ρ(σ)) for a group element sigma."
  [rep sigma]
  (fm/trace (rep-matrix rep sigma)))

(defn rep-generators
  "The generator matrices for adjacent transpositions s_1, ..., s_{n-1}.
   Returns a vector of (n-1) matrices."
  [rep]
  (:generators rep))

(defn frobenius-norm-sq
  "Squared Frobenius norm of a matrix: ||M||²_F = tr(M Mᵀ).
   For real matrices this equals the sum of squared entries."
  [M]
  (fm/trace (fm/mulm M (fm/transpose M))))

(defn frobenius-norm
  "Frobenius norm of a matrix: ||M||_F = sqrt(tr(M Mᵀ))."
  [M]
  (Math/sqrt (frobenius-norm-sq M)))

(defn matrix-fourier-transform
  "Matrix-valued Fourier transform of a function f: S_n → ℝ.

   f̂(ρ) = Σ_{σ∈G} f(σ) · ρ(σ)

   where ρ is an irreducible representation.

   Parameters:
   - irrep: result of (irrep lambda)
   - group: the symmetric group
   - f: a map from permutations to real values, or a function σ → f(σ)

   Returns a d×d RealMatrix."
  [irrep group f]
  (let [d (:dimension irrep)
        elts (p/elements group)]
    (reduce (fn [acc sigma]
              (let [coeff (double (if (map? f) (get f sigma 0.0) (f sigma)))]
                (if (zero? coeff)
                  acc
                  (.add ^org.apache.commons.math3.linear.RealMatrix acc
                        (.scalarMultiply ^org.apache.commons.math3.linear.RealMatrix
                         (rep-matrix irrep sigma) coeff)))))
            (fm/rows->mat (vec (repeat d (vec (repeat d 0.0)))))
            elts)))

(defn matrix-fourier-transform-all
  "Compute the matrix Fourier transform for all irreps of S_n.

   Parameters:
   - group: the symmetric group S_n
   - f: a map from permutations to real values, or a function σ → f(σ)
   - irreps: vector of irrep objects (from irrep-generators)

   Returns a vector of d_λ × d_λ matrices, one per irrep."
  [group f irreps]
  (mapv (fn [ir] (matrix-fourier-transform ir group f)) irreps))

(defn plancherel-lhs
  "Left-hand side of Plancherel identity: Σ_{σ∈G} |f(σ)|²."
  [group f]
  (reduce + (map (fn [sigma]
                   (let [v (double (if (map? f) (get f sigma 0.0) (f sigma)))]
                     (* v v)))
                 (p/elements group))))

(defn plancherel-rhs
  "Right-hand side of Plancherel identity:
   (1/|G|) Σ_ρ d_ρ · ||f̂(ρ)||²_F."
  [group f-hats irreps]
  (let [order (double (p/order group))]
    (* (/ 1.0 order)
       (reduce + (map (fn [ir f-hat]
                        (* (double (:dimension ir))
                           (frobenius-norm-sq f-hat)))
                      irreps f-hats)))))

(defn tensor-product
  "Tensor product ρ₁ ⊗ ρ₂ of two representations.
   The result has dimension d₁·d₂, and (ρ₁⊗ρ₂)(g) = ρ₁(g) ⊗ ρ₂(g)
   where ⊗ is the Kronecker product.

   Both representations must be for the same group.
   Returns a representation map with :dimension and a :matrix-fn."
  [rep1 rep2]
  {:dimension (* (:dimension rep1) (:dimension rep2))
   :rep1 rep1
   :rep2 rep2
   :matrix-fn (fn [sigma]
                (fm/kronecker (rep-matrix rep1 sigma)
                              (rep-matrix rep2 sigma)))})

(defn direct-sum
  "Direct sum ρ₁ ⊕ ρ₂ of two representations.
   The result has dimension d₁+d₂, and (ρ₁⊕ρ₂)(g) is the block diagonal
   matrix with ρ₁(g) and ρ₂(g) on the diagonal.

   Both representations must be for the same group.
   Returns a representation map with :dimension and a :matrix-fn."
  [rep1 rep2]
  (let [d1 (:dimension rep1)
        d2 (:dimension rep2)
        d (+ d1 d2)]
    {:dimension d
     :rep1 rep1
     :rep2 rep2
     :matrix-fn (fn [sigma]
                  (let [^org.apache.commons.math3.linear.RealMatrix M1 (rep-matrix rep1 sigma)
                        ^org.apache.commons.math3.linear.RealMatrix M2 (rep-matrix rep2 sigma)]
                    (fm/rows->mat
                     (vec (concat
                           (mapv (fn [i]
                                   (vec (concat (vec (.getRow M1 i))
                                                (repeat d2 0.0))))
                                 (range d1))
                           (mapv (fn [i]
                                   (vec (concat (repeat d1 0.0)
                                                (vec (.getRow M2 i)))))
                                 (range d2)))))))}))

