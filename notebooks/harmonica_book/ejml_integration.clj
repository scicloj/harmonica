;; # EJML Integration
;;
;; [EJML](https://ejml.org/) (Efficient Java Matrix Library) provides
;; fast complex matrix operations via `ZMatrixRMaj`. Because it stores
;; complex entries as **interleaved** `double[]` — the same layout as
;; [ComplexTensor](complex_tensors.html) — the two can share memory
;; with zero copying.
;;
;; This notebook demonstrates the bridge between harmonica's
;; `ComplexTensor` and EJML's `ZMatrixRMaj`, verifies correctness
;; of the wrapped operations, and shows the performance advantage
;; for the matrix Fourier transform accumulation pattern
;; $\hat{f}(\rho) = \sum_{\sigma \in G} f(\sigma) \, \rho(\sigma)$.
;;
;; **Dependency** (dev/optional):
;;
;; | artifact | version |
;; |:---------|:--------|
;; | `org.ejml/ejml-zdense` | 0.44.0 |

(ns harmonica-book.ejml-integration
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
   [scicloj.harmonica.linalg.ejml :as ejml]
   [scicloj.harmonica.analysis.representations :as rep]
   [tech.v3.tensor :as tensor]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind])
  (:import [org.ejml.data ZMatrixRMaj]))

;; ## The shared memory layout
;;
;; A ComplexTensor matrix with complex shape $[r, c]$ is backed by a
;; real tensor of shape $[r, c, 2]$. In row-major order, the underlying
;; `double[]` is interleaved:
;;
;; $$[\operatorname{re}_{00},\; \operatorname{im}_{00},\;
;;    \operatorname{re}_{01},\; \operatorname{im}_{01},\; \ldots]$$
;;
;; EJML's `ZMatrixRMaj` uses the identical layout. This means the two
;; types can **share the same backing array** — no allocation, no
;; copying.

(let [ct (cx/complex-tensor
          (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
          (tensor/->tensor [[0.5 1.0] [1.5 2.0]]))]
  {:complex-shape (cx/complex-shape ct)
   :raw-doubles (vec (cx/->double-array ct))})

;; The interleaved doubles: `re(0,0)=1`, `im(0,0)=0.5`, `re(0,1)=2`,
;; `im(0,1)=1`, etc.

(kind/test-last [(fn [v] (= (:raw-doubles v)
                            [1.0 0.5 2.0 1.0 3.0 1.5 4.0 2.0]))])

;; `ct->zmat` wraps a ComplexTensor as a `ZMatrixRMaj`, sharing the
;; **identical** Java `double[]` object.

(let [ct (cx/complex-tensor
          (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
          (tensor/->tensor [[0.5 1.0] [1.5 2.0]]))
      zm (ejml/ct->zmat ct)]
  {:identical? (identical? (cx/->double-array ct) (.data ^ZMatrixRMaj zm))
   :rows (.numRows ^ZMatrixRMaj zm)
   :cols (.numCols ^ZMatrixRMaj zm)})

(kind/test-last [(fn [v] (and (:identical? v)
                              (= (:rows v) 2)
                              (= (:cols v) 2)))])

;; ## Zero-copy round-trip
;;
;; Because both views share the same array, mutations through one
;; are immediately visible through the other.

;; ### EJML mutation visible in ComplexTensor

(let [ct (cx/complex-tensor
          (tensor/->tensor [[1.0 0.0] [0.0 1.0]])
          (tensor/->tensor [[0.0 0.0] [0.0 0.0]]))
      zm (ejml/ct->zmat ct)]
  ;; Mutate via EJML
  (.set ^ZMatrixRMaj zm 0 1 99.0 77.0)
  ;; Read back via ComplexTensor
  (let [elem (ct 0)]
    [(cx/re (elem 1)) (cx/im (elem 1))]))

(kind/test-last [= [99.0 77.0]])

;; ### ComplexTensor mutation visible in EJML

(let [ct (cx/complex-tensor
          (tensor/->tensor [[1.0 0.0] [0.0 1.0]])
          (tensor/->tensor [[0.0 0.0] [0.0 0.0]]))
      zm (ejml/ct->zmat ct)
      arr (cx/->double-array ct)]
  ;; Mutate the raw array (shared by both)
  (aset arr 2 42.0) ;; re(0,1)
  (aset arr 3 13.0) ;; im(0,1)
  ;; Read back via EJML
  (let [c (org.ejml.data.Complex_F64.)]
    (.get ^ZMatrixRMaj zm 0 1 c)
    [(.real c) (.imaginary c)]))

(kind/test-last [= [42.0 13.0]])

;; The reverse direction, `zmat->ct`, also shares the array.

(let [zm (ejml/zmat 2 2)
      _ (.set ^ZMatrixRMaj zm 0 0 5.0 6.0)
      ct (ejml/zmat->ct zm)]
  (identical? (.data ^ZMatrixRMaj zm) (cx/->double-array ct)))

(kind/test-last [true?])

;; ## Complex matrix operations
;;
;; The `scicloj.harmonica.linalg.ejml` namespace wraps EJML's `CommonOps_ZDRM`
;; into Clojure functions that accept and return `ZMatrixRMaj` objects.

;; ### Matrix multiply
;;
;; $A \cdot I = A$

(let [A (ejml/ct->zmat (cx/complex-tensor
                        (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
                        (tensor/->tensor [[0.5 1.0] [1.5 2.0]])))
      I (ejml/zmat-identity 2)
      AI (ejml/zmul A I)]
  (= (vec (.data ^ZMatrixRMaj AI))
     (vec (.data ^ZMatrixRMaj A))))

(kind/test-last [true?])

;; Non-trivial product. For
;; $A = \begin{pmatrix} 1+2i & 3+4i \\ 5+6i & 7+8i \end{pmatrix}$,
;; the $(0,0)$ entry of $A^2$ is
;; $(1+2i)^2 + (3+4i)(5+6i) = (-3+4i) + (-9+38i) = -12+42i$.

(let [A (ejml/ct->zmat (cx/complex-tensor
                        (tensor/->tensor [[1.0 3.0] [5.0 7.0]])
                        (tensor/->tensor [[2.0 4.0] [6.0 8.0]])))
      A2 (ejml/zmul A A)
      result (ejml/zmat->ct A2)]
  [(cx/re ((result 0) 0)) (cx/im ((result 0) 0))])

(kind/test-last [= [-12.0 42.0]])

;; ### Trace and determinant

(let [A (ejml/ct->zmat (cx/complex-tensor
                        (tensor/->tensor [[1.0 3.0] [5.0 7.0]])
                        (tensor/->tensor [[2.0 4.0] [6.0 8.0]])))]
  {:trace (ejml/ztrace A)
   :det (let [[re im] (ejml/zdet A)]
          [(Math/round re) (Math/round im)])})

;; $\operatorname{tr}(A) = (1+2i) + (7+8i) = 8+10i$
;;
;; $\det(A) = (1+2i)(7+8i) - (3+4i)(5+6i) = -16i$

(kind/test-last [(fn [v] (and (= (:trace v) [8.0 10.0])
                              (= (:det v) [0 -16])))])

;; ### Frobenius norm
;;
;; The identity $\operatorname{tr}(A^\dagger A) = \|A\|_F^2$ connects
;; the conjugate transpose, trace, and norm.

(let [A (ejml/ct->zmat (cx/complex-tensor
                        (tensor/->tensor [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]])
                        (tensor/->tensor [[0.1 0.2 0.3] [0.4 0.5 0.6] [0.7 0.8 0.9]])))
      AdA (ejml/zmul (ejml/ztranspose-conj A) A)
      [tr-re _] (ejml/ztrace AdA)
      nf (ejml/znorm-f A)]
  (< (Math/abs (- tr-re (* nf nf))) 1e-10))

(kind/test-last [true?])

;; ### Conjugate transpose
;;
;; For $A = \begin{pmatrix} 1+i & 2+3i \\ 4+5i & 6+7i \end{pmatrix}$,
;; $A^\dagger = \begin{pmatrix} 1-i & 4-5i \\ 2-3i & 6-7i \end{pmatrix}$.

(let [A (ejml/ct->zmat (cx/complex-tensor
                        (tensor/->tensor [[1.0 2.0] [4.0 6.0]])
                        (tensor/->tensor [[1.0 3.0] [5.0 7.0]])))
      Ad (ejml/ztranspose-conj A)
      ct (ejml/zmat->ct Ad)]
  {:re (vec (dtype/->double-array (cx/re ct)))
   :im (vec (dtype/->double-array (cx/im ct)))})

(kind/test-last [(fn [v] (and (= (:re v) [1.0 4.0 2.0 6.0])
                              (= (:im v) [-1.0 -5.0 -3.0 -7.0])))])

;; ### Inverse
;;
;; $A \cdot A^{-1} \approx I$

(let [A (ejml/ct->zmat (cx/complex-tensor
                        (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
                        (tensor/->tensor [[0.5 1.0] [1.5 2.5]])))
      inv (ejml/zinvert A)
      product (ejml/zmul A inv)
      ct (ejml/zmat->ct product)
      re (cx/re ct)
      im (cx/im ct)]
  (and (< (dfn/reduce-max (dfn/abs (dfn/- (dtype/->double-array re)
                                          (double-array [1 0 0 1])))) 1e-10)
       (< (dfn/reduce-max (dfn/abs (dtype/->double-array im))) 1e-10)))

(kind/test-last [true?])

;; ### Addition and subtraction

(let [A (ejml/ct->zmat (cx/complex-tensor
                        (tensor/->tensor [[1.0 2.0]])
                        (tensor/->tensor [[3.0 4.0]])))
      B (ejml/ct->zmat (cx/complex-tensor
                        (tensor/->tensor [[5.0 6.0]])
                        (tensor/->tensor [[7.0 8.0]])))
      sum-ct (ejml/zmat->ct (ejml/zadd A B))
      diff-ct (ejml/zmat->ct (ejml/zsub A B))]
  {:sum-re (vec (dtype/->double-array (cx/re sum-ct)))
   :diff-re (vec (dtype/->double-array (cx/re diff-ct)))})

(kind/test-last [(fn [v] (and (= (:sum-re v) [6.0 8.0])
                              (= (:diff-re v) [-4.0 -4.0])))])

;; ## Accumulation pattern
;;
;; The matrix-valued Fourier transform on a finite group accumulates
;;
;; $$\hat{f}(\rho) = \sum_{\sigma \in G} f(\sigma) \, \rho(\sigma)$$
;;
;; where $f(\sigma)$ is a scalar and $\rho(\sigma)$ is a $d \times d$
;; matrix. The `scale-add-reuse!` function implements the inner loop
;; efficiently: `acc += α · M` using a reusable temp buffer.

;; As a small example, accumulate $3 \cdot I + (-1) \cdot P$ where
;; $P$ is the permutation matrix that swaps two coordinates.

(let [I-zm (ejml/ct->zmat (cx/complex-tensor
                           (tensor/->tensor [[1.0 0.0] [0.0 1.0]])
                           (tensor/->tensor [[0.0 0.0] [0.0 0.0]])))
      P-zm (ejml/ct->zmat (cx/complex-tensor
                           (tensor/->tensor [[0.0 1.0] [1.0 0.0]])
                           (tensor/->tensor [[0.0 0.0] [0.0 0.0]])))
      acc (ejml/zmat 2 2)
      temp (ejml/zmat 2 2)]
  (ejml/scale-add-reuse! acc 3.0 0.0 I-zm temp)
  (ejml/scale-add-reuse! acc -1.0 0.0 P-zm temp)
  (vec (dtype/->double-array (cx/re (ejml/zmat->ct acc)))))

;; $3I - P = \begin{pmatrix} 3 & -1 \\ -1 & 3 \end{pmatrix}$

(kind/test-last [= [3.0 -1.0 -1.0 3.0]])

;; ## Matrix Fourier transform comparison
;;
;; We compute the matrix Fourier transform of a function on $S_4$
;; using both the existing API (Apache Commons Math backend) and EJML,
;; then verify the results match exactly.

(let [G (hm/symmetric-group 4)
      ir (hm/irrep [3 1])
      d (:dimension ir)
      ;; deterministic function
      f-map (zipmap (hm/elements G)
                    (map #(Math/sin (double %)) (range (hm/order G))))
      ;; Existing API
      result-acm (rep/matrix-fourier-transform ir G f-map)
      ;; EJML accumulation
      result-ejml (let [acc (ejml/zmat d d)
                        temp (ejml/zmat d d)]
                    (doseq [sigma (hm/elements G)]
                      (let [coeff (get f-map sigma 0.0)
                            mat (rep/rep-matrix ir sigma)
                            zm (ejml/zmat d d)]
                        (dotimes [i d]
                          (dotimes [j d]
                            (.set ^ZMatrixRMaj zm i j
                                  (double (fm/entry mat i j)) 0.0)))
                        (ejml/scale-add-reuse! acc coeff 0.0 zm temp)))
                    acc)
      ;; Compare element by element
      max-diff (reduce max
                       (for [i (range d) j (range d)]
                         (let [c (org.ejml.data.Complex_F64.)]
                           (.get ^ZMatrixRMaj result-ejml i j c)
                           (Math/abs (- (.real c)
                                        (double (fm/entry result-acm i j)))))))]
  max-diff)

(kind/test-last [(fn [v] (< v 1e-10))])

;; ## Performance comparison
;;
;; Isolating the accumulation loop (with pre-computed representation
;; matrices) reveals EJML's advantage. We benchmark $S_5$ with the
;; $6$-dimensional irrep $\lambda = [3,1,1]$, which has $|S_5| = 120$
;; group elements.

(defn benchmark-accumulation
  "Time n iterations of matrix Fourier accumulation. Returns µs/iter."
  [accumulate-fn n]
  (accumulate-fn) ;; warmup
  (let [t0 (System/nanoTime)]
    (dotimes [_ n] (accumulate-fn))
    (/ (- (System/nanoTime) t0) (* n 1000.0))))

(let [G (hm/symmetric-group 5)
      ir (hm/irrep [3 1 1])
      d (:dimension ir)
      elts (hm/elements G)
      f-map (zipmap elts (map #(Math/sin (double %)) (range (hm/order G))))
      ;; Pre-compute EJML rep matrices
      precomp-z (into {}
                      (map (fn [sigma]
                             (let [mat (rep/rep-matrix ir sigma)
                                   zm (ejml/zmat d d)]
                               (dotimes [i d]
                                 (dotimes [j d]
                                   (.set ^ZMatrixRMaj zm i j
                                         (double (fm/entry mat i j)) 0.0)))
                               [sigma zm]))
                           elts))
      ;; Pre-compute ACM rep matrices
      precomp-acm (into {}
                        (map (fn [sigma]
                               [sigma (rep/rep-matrix ir sigma)])
                             elts))
      ;; EJML accumulation
      ejml-fn (fn []
                (let [acc (ejml/zmat d d)
                      temp (ejml/zmat d d)]
                  (doseq [[sigma ^ZMatrixRMaj zm] precomp-z]
                    (let [coeff (double (get f-map sigma 0.0))]
                      (when-not (zero? coeff)
                        (ejml/scale-add-reuse! acc coeff 0.0 zm temp))))
                  acc))
      ;; ACM accumulation
      acm-fn (fn []
               (reduce
                (fn [^org.apache.commons.math3.linear.RealMatrix acc
                     [sigma mat]]
                  (let [coeff (double (get f-map sigma 0.0))]
                    (if (zero? coeff)
                      acc
                      (.add acc
                            (.scalarMultiply
                             ^org.apache.commons.math3.linear.RealMatrix
                             mat coeff)))))
                (fm/rows->mat (vec (repeat d (vec (repeat d 0.0)))))
                precomp-acm))
      n 2000
      us-ejml (benchmark-accumulation ejml-fn n)
      us-acm (benchmark-accumulation acm-fn n)]
  (kind/table
   {:column-names ["Backend" "$d$" "$|G|$" "µs / iter" "Speedup"]
    :row-vectors [["EJML (ZMatrixRMaj)" d (hm/order G)
                   (format "%.1f" us-ejml)
                   (format "%.1fx" (/ us-acm us-ejml))]
                  ["ACM (RealMatrix)" d (hm/order G)
                   (format "%.1f" us-acm) "1.0x"]]}))

;; The speedup grows with matrix dimension because EJML's internal
;; loops are tighter (primitive `double[]` with no object overhead).
;; See `dev-notes/ejml-integration.md` for benchmarks at $d = 3$
;; ($S_4$), $d = 6$ ($S_5$), and $d = 16$ ($S_6$).

;; ## Summary
;;
;; The `scicloj.harmonica.linalg.ejml` namespace provides:
;;
;; - **Zero-copy interop** — `ct->zmat` and `zmat->ct` share the
;;   same `double[]` between ComplexTensor and `ZMatrixRMaj`
;;
;; - **2–3× faster accumulation** — `scale-add-reuse!` outperforms
;;   Apache Commons Math's `.scalarMultiply` + `.add` for the matrix
;;   Fourier transform inner loop
;;
;; - **Full complex linear algebra** — multiply, conjugate transpose,
;;   trace, determinant, Frobenius norm, inverse, LU/QR/Cholesky
;;
;; **Limitations**: EJML does not provide complex eigenvalue
;; decomposition, complex SVD, or Kronecker product. For these,
;; use the existing fastmath / Apache Commons Math backend.
;;
;; See [Complex Tensors](complex_tensors.html) for the ComplexTensor
;; API that this namespace builds on.
