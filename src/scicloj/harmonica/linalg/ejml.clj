(ns scicloj.harmonica.linalg.ejml
  "Zero-copy interop between ComplexTensor and EJML's ZMatrixRMaj.

   Both structures use the same memory layout: row-major interleaved
   double[] with [re0, im0, re1, im1, ...]. This namespace provides
   functions to convert between them without copying data.

   EJML (Efficient Java Matrix Library) provides fast complex matrix
   operations — multiply, conjugate transpose, trace, Frobenius norm,
   determinant, and (via ejml-zdense) LU/QR/Cholesky decompositions.

   Dependency: org.ejml/ejml-zdense 0.44.0 (dev/optional).

   ## Usage

   ```clojure
   (require '[scicloj.harmonica.linalg.complex :as cx])
   (require '[scicloj.harmonica.linalg.ejml :as ejml])

   ;; Zero-copy ComplexTensor -> ZMatrixRMaj
   (def ct (cx/complex-tensor [[1 0] [0 0]] [[0 0] [0 1]]))
   (def zm (ejml/ct->zmat ct))  ; shared double[]

   ;; EJML operations
   (def product (ejml/zmul zm zm))
   (ejml/ztrace zm)       ; => [re im]
   (ejml/znorm-f zm)      ; => Frobenius norm

   ;; Zero-copy back to ComplexTensor
   (def result (ejml/zmat->ct product))
   ```"
  (:require [scicloj.harmonica.linalg.complex :as cx]
            [tech.v3.tensor :as tensor])
  (:import [org.ejml.data ZMatrixRMaj Complex_F64]
           [org.ejml.dense.row CommonOps_ZDRM NormOps_ZDRM]))

;; ---------------------------------------------------------------------------
;; Zero-copy conversion
;; ---------------------------------------------------------------------------

(defn ct->zmat
  "Zero-copy: ComplexTensor -> ZMatrixRMaj sharing the same double[].

   For a matrix ComplexTensor [r c], creates an r×c ZMatrixRMaj.
   For a vector ComplexTensor [n], creates an n×1 column vector.
   Mutations through either view are visible in the other."
  ^ZMatrixRMaj [ct]
  (let [shape (cx/complex-shape ct)
        [r c] (if (= 1 (count shape))
                [(first shape) 1]
                shape)
        arr (cx/->double-array ct)
        zm (ZMatrixRMaj. (int r) (int c))]
    (.setData zm arr)
    zm))

(defn zmat->ct
  "Zero-copy: ZMatrixRMaj -> ComplexTensor [r c] sharing the same double[].

   Mutations through either view are visible in the other."
  [^ZMatrixRMaj zm]
  (let [r (.numRows zm)
        c (.numCols zm)
        arr (.data zm)]
    (cx/complex-tensor (tensor/reshape (tensor/ensure-tensor arr) [r c 2]))))

(defn zmat
  "Create a new ZMatrixRMaj of size r x c, initialized to zero."
  ^ZMatrixRMaj [r c]
  (ZMatrixRMaj. (int r) (int c)))

(defn zmat-identity
  "Create a ZMatrixRMaj identity matrix of size d x d."
  ^ZMatrixRMaj [d]
  (let [m (ZMatrixRMaj. (int d) (int d))]
    (CommonOps_ZDRM/setIdentity m)
    m))

;; ---------------------------------------------------------------------------
;; Matrix operations
;; ---------------------------------------------------------------------------

(defn zmul
  "Complex matrix multiply: C = A * B. Returns a new ZMatrixRMaj."
  ^ZMatrixRMaj [^ZMatrixRMaj a ^ZMatrixRMaj b]
  (let [c (ZMatrixRMaj. (.numRows a) (.numCols b))]
    (CommonOps_ZDRM/mult a b c)
    c))

(defn zmul!
  "Complex matrix multiply: C = A * B. Writes into pre-allocated C."
  [^ZMatrixRMaj a ^ZMatrixRMaj b ^ZMatrixRMaj c]
  (CommonOps_ZDRM/mult a b c)
  c)

(defn zmul-add!
  "Accumulate: C += A * B."
  [^ZMatrixRMaj a ^ZMatrixRMaj b ^ZMatrixRMaj c]
  (CommonOps_ZDRM/multAdd a b c)
  c)

(defn zmul-add-alpha!
  "Accumulate with scalar: C += (alpha_re + alpha_im*i) * A * B."
  [alpha-re alpha-im ^ZMatrixRMaj a ^ZMatrixRMaj b ^ZMatrixRMaj c]
  (CommonOps_ZDRM/multAdd (double alpha-re) (double alpha-im) a b c)
  c)

(defn zscale!
  "In-place scale: A *= (alpha_re + alpha_im*i)."
  [alpha-re alpha-im ^ZMatrixRMaj a]
  (CommonOps_ZDRM/scale (double alpha-re) (double alpha-im) a)
  a)

(defn zadd
  "Complex matrix addition: C = A + B. Returns a new ZMatrixRMaj."
  ^ZMatrixRMaj [^ZMatrixRMaj a ^ZMatrixRMaj b]
  (let [c (ZMatrixRMaj. (.numRows a) (.numCols a))]
    (CommonOps_ZDRM/add a b c)
    c))

(defn zadd!
  "Complex matrix addition: C = A + B. Writes into C (may alias A or B)."
  [^ZMatrixRMaj a ^ZMatrixRMaj b ^ZMatrixRMaj c]
  (CommonOps_ZDRM/add a b c)
  c)

(defn zsub
  "Complex matrix subtraction: C = A - B. Returns a new ZMatrixRMaj."
  ^ZMatrixRMaj [^ZMatrixRMaj a ^ZMatrixRMaj b]
  (let [c (ZMatrixRMaj. (.numRows a) (.numCols a))]
    (CommonOps_ZDRM/subtract a b c)
    c))

(defn ztranspose-conj
  "Conjugate transpose (Hermitian adjoint): B = A†. Returns a new ZMatrixRMaj."
  ^ZMatrixRMaj [^ZMatrixRMaj a]
  (CommonOps_ZDRM/transposeConjugate a nil))

(defn ztranspose-conj!
  "Conjugate transpose into pre-allocated output."
  [^ZMatrixRMaj a ^ZMatrixRMaj out]
  (CommonOps_ZDRM/transposeConjugate a out)
  out)

(defn ztrace
  "Complex trace. Returns [re im]."
  [^ZMatrixRMaj a]
  (let [^Complex_F64 c (CommonOps_ZDRM/trace a nil)]
    [(.real c) (.imaginary c)]))

(defn zdet
  "Complex determinant. Returns [re im]."
  [^ZMatrixRMaj a]
  (let [^Complex_F64 c (CommonOps_ZDRM/det a)]
    [(.real c) (.imaginary c)]))

(defn znorm-f
  "Frobenius norm: ||A||_F = sqrt(Σ|a_ij|²). Returns a double."
  ^double [^ZMatrixRMaj a]
  (NormOps_ZDRM/normF a))

(defn zinvert
  "Complex matrix inverse. Returns a new ZMatrixRMaj, or nil if singular."
  [^ZMatrixRMaj a]
  (let [inv (ZMatrixRMaj. (.numRows a) (.numCols a))]
    (when (CommonOps_ZDRM/invert a inv)
      inv)))

;; ---------------------------------------------------------------------------
;; Accumulation helper (for Fourier transforms)
;; ---------------------------------------------------------------------------

(defn scale-add!
  "Accumulate: acc += (alpha_re + alpha_im*i) * mat.
   Uses a temporary buffer to avoid mutating mat."
  [^ZMatrixRMaj acc alpha-re alpha-im ^ZMatrixRMaj mat]
  (let [temp (ZMatrixRMaj. (.numRows mat) (.numCols mat))]
    (.setTo temp mat)
    (CommonOps_ZDRM/scale (double alpha-re) (double alpha-im) temp)
    (CommonOps_ZDRM/add acc temp acc))
  acc)

(defn scale-add-reuse!
  "Like scale-add! but reuses a caller-provided temp buffer.
   For hot loops where avoiding allocation matters."
  [^ZMatrixRMaj acc alpha-re alpha-im ^ZMatrixRMaj mat ^ZMatrixRMaj temp]
  (.setTo temp mat)
  (CommonOps_ZDRM/scale (double alpha-re) (double alpha-im) temp)
  (CommonOps_ZDRM/add acc temp acc)
  acc)
