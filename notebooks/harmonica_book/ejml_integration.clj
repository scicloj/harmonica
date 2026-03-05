;; # EJML Integration
;;
;; [EJML](https://ejml.org/) (Efficient Java Matrix Library) provides
;; fast complex matrix operations via `ZMatrixRMaj`. Because it stores
;; complex entries as **interleaved** `double[]` — the same layout as
;; [ComplexTensor](complex_tensors.html) — the two can share memory
;; with zero copying.
;;
;; This notebook demonstrates the bridge between lalinea's
;; ComplexTensor and EJML's `ZMatrixRMaj`, and shows how
;; lalinea's high-level API (`la/mmul`, `la/trace`, etc.) dispatches
;; to EJML automatically for complex matrices.
;;
;; **Dependency** (dev/optional):
;;
;; | artifact | version |
;; |:---------|:--------|
;; | `org.ejml/ejml-zdense` | 0.44.0 |

(ns harmonica-book.ejml-integration
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.lalinea.tensor :as t]
   [scicloj.lalinea.elementwise :as el]
   [scicloj.lalinea.linalg :as la]
   [scicloj.harmonica.analysis.representations :as rep]
   [tech.v3.tensor :as tensor]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind])
  (:import [org.ejml.data ZMatrixRMaj]))

;; ## The interleaved layout
;;
;; A ComplexTensor matrix with complex shape $[r, c]$ is backed by a
;; real tensor of shape $[r, c, 2]$. In row-major order, the doubles
;; are interleaved:
;;
;; $$[\operatorname{re}_{00},\; \operatorname{im}_{00},\;
;;    \operatorname{re}_{01},\; \operatorname{im}_{01},\; \ldots]$$
;;
;; EJML's `ZMatrixRMaj` uses the identical layout.

(let [ct (t/complex-tensor
          (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
          (tensor/->tensor [[0.5 1.0] [1.5 2.0]]))]
  {:complex-shape (t/complex-shape ct)
   :raw-doubles (vec (t/->double-array ct))})

;; The interleaved doubles: `re(0,0)=1`, `im(0,0)=0.5`, `re(0,1)=2`,
;; `im(0,1)=1`, etc.

(kind/test-last [(fn [v] (= (:raw-doubles v)
                            [1.0 0.5 2.0 1.0 3.0 1.5 4.0 2.0]))])

;; ## Converting between ComplexTensor and ZMatrixRMaj
;;
;; `complex-tensor->zmat` converts a ComplexTensor to a `ZMatrixRMaj`.

(let [ct (t/complex-tensor
          (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
          (tensor/->tensor [[0.5 1.0] [1.5 2.0]]))
      zm (t/complex-tensor->zmat ct)]
  {:rows (.numRows ^ZMatrixRMaj zm)
   :cols (.numCols ^ZMatrixRMaj zm)
   :entry-00 (let [c (org.ejml.data.Complex_F64.)]
               (.get ^ZMatrixRMaj zm 0 0 c)
               [(.real c) (.imaginary c)])})

(kind/test-last [(fn [v] (and (= (:rows v) 2) (= (:cols v) 2)
                              (= (:entry-00 v) [1.0 0.5])))])

;; The reverse direction, `zmat->complex-tensor`, wraps the
;; `ZMatrixRMaj`'s backing `double[]` directly — zero-copy.

(let [zm (ZMatrixRMaj. 2 2)]
  (.set zm 0 0 5.0 6.0)
  (let [ct (t/zmat->complex-tensor zm)]
    {:identical? (identical? (.data zm) (t/->double-array ct))
     :re (el/re ((ct 0) 0))
     :im (el/im ((ct 0) 0))}))

(kind/test-last [(fn [v] (and (:identical? v)
                              (= (:re v) 5.0) (= (:im v) 6.0)))])

;; Because both views share the same array, mutations through the
;; `ZMatrixRMaj` are immediately visible through the ComplexTensor.

(let [zm (ZMatrixRMaj. 2 2)]
  (.set zm 0 1 99.0 77.0)
  (let [ct (t/zmat->complex-tensor zm)]
    [(el/re ((ct 0) 1)) (el/im ((ct 0) 1))]))

(kind/test-last [= [99.0 77.0]])

;; ## Complex matrix operations via dispatch
;;
;; Lalinea's `la/` functions dispatch on real vs complex tensors.
;; For complex matrices, operations like `la/mmul`, `la/trace`, and
;; `la/det` delegate to EJML automatically — no need to convert
;; to `ZMatrixRMaj` manually.

;; ### Matrix multiply

(def A (t/complex-tensor
        (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
        (tensor/->tensor [[0.5 1.0] [1.5 2.0]])))

;; $A \cdot I = A$

(let [I (t/complex-tensor-real (t/eye 2))
      AI (la/mmul A I)]
  (la/close? A AI))

(kind/test-last [true?])

;; Non-trivial product. For
;; $A = \begin{pmatrix} 1+2i & 3+4i \\ 5+6i & 7+8i \end{pmatrix}$,
;; the $(0,0)$ entry of $A^2$ is
;; $(1+2i)^2 + (3+4i)(5+6i) = (-3+4i) + (-9+38i) = -12+42i$.

(let [B (t/complex-tensor
         (tensor/->tensor [[1.0 3.0] [5.0 7.0]])
         (tensor/->tensor [[2.0 4.0] [6.0 8.0]]))
      B2 (la/mmul B B)
      entry (el/* ((B2 0) 0) (t/complex 1 0))]
  [(el/re ((B2 0) 0)) (el/im ((B2 0) 0))])

(kind/test-last [= [-12.0 42.0]])

;; ### Trace and determinant

(let [B (t/complex-tensor
         (tensor/->tensor [[1.0 3.0] [5.0 7.0]])
         (tensor/->tensor [[2.0 4.0] [6.0 8.0]]))
      tr (la/trace B)
      d (la/det B)]
  {:trace-re (el/re tr) :trace-im (el/im tr)
   :det-re (Math/round (double (el/re d)))
   :det-im (Math/round (double (el/im d)))})

;; $\operatorname{tr}(A) = (1+2i) + (7+8i) = 8+10i$
;;
;; $\det(A) = (1+2i)(7+8i) - (3+4i)(5+6i) = -16i$

(kind/test-last [(fn [v] (and (= (:trace-re v) 8.0) (= (:trace-im v) 10.0)
                              (= (:det-re v) 0) (= (:det-im v) -16)))])

;; ### Frobenius norm
;;
;; The identity $\operatorname{tr}(A^\dagger A) = \|A\|_F^2$ connects
;; the conjugate transpose, trace, and norm.

(let [C (t/complex-tensor
         (tensor/->tensor [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]])
         (tensor/->tensor [[0.1 0.2 0.3] [0.4 0.5 0.6] [0.7 0.8 0.9]]))
      AdA (la/mmul (la/transpose C) C)
      tr-re (double (el/re (la/trace AdA)))
      nf (la/norm C)]
  (< (Math/abs (- tr-re (* nf nf))) 1e-10))

(kind/test-last [true?])

;; ### Conjugate transpose
;;
;; `la/transpose` on a ComplexTensor computes the conjugate transpose $A^\dagger$.

(let [B (t/complex-tensor
         (tensor/->tensor [[1.0 2.0] [4.0 6.0]])
         (tensor/->tensor [[1.0 3.0] [5.0 7.0]]))
      Bd (la/transpose B)]
  {:re (vec (dtype/->double-array (el/re Bd)))
   :im (vec (dtype/->double-array (el/im Bd)))})

;; $A^\dagger = \begin{pmatrix} 1-i & 4-5i \\ 2-3i & 6-7i \end{pmatrix}$

(kind/test-last [(fn [v] (and (= (:re v) [1.0 4.0 2.0 6.0])
                              (= (:im v) [-1.0 -5.0 -3.0 -7.0])))])

;; ### Inverse
;;
;; $A \cdot A^{-1} \approx I$

(let [inv (la/invert A)
      product (la/mmul A inv)
      re-part (el/re product)
      im-part (el/im product)]
  (and (< (el/reduce-max (el/abs (el/- re-part (t/eye 2)))) 1e-10)
       (< (el/reduce-max (el/abs im-part)) 1e-10)))

(kind/test-last [true?])

;; ### Addition and subtraction

(let [X (t/complex-tensor [1.0 2.0] [3.0 4.0])
      Y (t/complex-tensor [5.0 6.0] [7.0 8.0])
      s (el/+ X Y)
      d (el/- X Y)]
  {:sum-re (vec (dtype/->double-array (el/re s)))
   :diff-re (vec (dtype/->double-array (el/re d)))})

(kind/test-last [(fn [v] (and (= (:sum-re v) [6.0 8.0])
                              (= (:diff-re v) [-4.0 -4.0])))])

;; ## Matrix Fourier transform
;;
;; The matrix-valued Fourier transform on a finite group is
;;
;; $$\hat{f}(\rho) = \sum_{\sigma \in G} f(\sigma) \, \rho(\sigma)$$
;;
;; where $f(\sigma)$ is a scalar and $\rho(\sigma)$ is a $d \times d$
;; real matrix (for symmetric groups, irrep matrices are real).

(let [G (hm/symmetric-group 4)
      ir (hm/irrep [3 1])
      d (:dimension ir)
      f-map (zipmap (hm/elements G)
                    (map #(Math/sin (double %)) (range (hm/order G))))
      result (rep/matrix-fourier-transform ir G f-map)]
  {:dimension d
   :frobenius-norm (rep/frobenius-norm result)})

(kind/test-last [(fn [v] (and (= (:dimension v) 3)
                              (> (:frobenius-norm v) 0.0)))])

;; The Plancherel identity connects the spatial and spectral norms:
;;
;; $$\sum_{\sigma \in G} |f(\sigma)|^2
;;   = \frac{1}{|G|} \sum_\rho d_\rho \, \|\hat{f}(\rho)\|_F^2$$

(let [G (hm/symmetric-group 4)
      parts [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]
      irreps (mapv hm/irrep parts)
      f-map (zipmap (hm/elements G)
                    (map #(Math/sin (double %)) (range (hm/order G))))
      f-hats (rep/matrix-fourier-transform-all G f-map irreps)
      lhs (rep/plancherel-lhs G f-map)
      rhs (rep/plancherel-rhs G f-hats irreps)]
  (< (Math/abs (- lhs rhs)) 1e-10))

(kind/test-last [true?])

;; ## Summary
;;
;; Lalinea dispatches complex matrix operations to EJML automatically:
;;
;; - **Zero-copy interop** — `complex-tensor->zmat` and
;;   `zmat->complex-tensor` share the same `double[]`
;;
;; - **Field dispatch** — `la/mmul`, `la/trace`, `la/det`,
;;   `la/invert`, `la/transpose`, `la/norm` all work on ComplexTensors,
;;   delegating to EJML internally
;;
;; - **No manual conversion** — work with ComplexTensors directly;
;;   EJML is an implementation detail
;;
;; See [Complex Tensors](complex_tensors.html) for the ComplexTensor
;; API that this builds on.
