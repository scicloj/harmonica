;; # Complex Tensors
;;
;; **ComplexTensor** is a dtype-next tensor-backed complex number type.
;; It wraps a real tensor whose **last dimension is 2** — interleaved
;; real/imaginary pairs — providing zero-copy access to real and
;; imaginary parts as strided tensor views.
;;
;; | Underlying shape | Complex interpretation | `re` / `im` returns |
;; |:-----------------|:----------------------|:---------------------|
;; | `[2]` | scalar complex number | double |
;; | `[n 2]` | complex vector, length $n$ | `[n]` tensor view |
;; | `[r c 2]` | complex $r \times c$ matrix | `[r c]` tensor view |
;;
;; This namespace has zero dependencies on the rest of harmonica —
;; it depends only on dtype-next and is designed to be extractable
;; into a separate library.

(ns harmonica-book.complex-tensors
  (:require
   [scicloj.harmonica.complex :as cx]
   [tech.v3.tensor :as tensor]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Construction
;;
;; There are three ways to create a ComplexTensor.

;; ### From separate real and imaginary parts
;;
;; The most common constructor takes two arrays (or seqs, or tensors)
;; of the same shape and interleaves them into a single backing tensor.

(cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])

(kind/test-last [(fn [v] (= [3] (cx/complex-shape v)))])

;; Real and imaginary parts are accessible via `re` and `im`.

(let [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  {:re (vec (cx/re ct))
   :im (vec (cx/im ct))})

(kind/test-last [(fn [v] (and (= (:re v) [1.0 2.0 3.0])
                              (= (:im v) [4.0 5.0 6.0])))])

;; ### Wrapping an existing tensor
;;
;; If you already have a `[... 2]` tensor, wrap it directly.
;; This is zero-copy — no data is moved.

(cx/complex-tensor (tensor/->tensor [[1.0 2.0] [3.0 4.0]]))

(kind/test-last [(fn [v] (and (= [2] (cx/complex-shape v))
                              (= [1.0 3.0] (vec (cx/re v)))
                              (= [2.0 4.0] (vec (cx/im v)))))])

;; ### Real-only construction
;;
;; For purely real data, `complex-tensor-real` sets all imaginary parts to zero.

(cx/complex-tensor-real [5.0 6.0 7.0])

(kind/test-last [(fn [v] (and (= [5.0 6.0 7.0] (vec (cx/re v)))
                              (= [0.0 0.0 0.0] (vec (cx/im v)))))])

;; ### Scalar complex numbers
;;
;; A scalar ComplexTensor has underlying shape `[2]`.

(def z (cx/complex-tensor (tensor/->tensor [3.0 4.0])))

(cx/scalar? z)

(kind/test-last [true?])

;; For scalars, `re` and `im` return doubles.

[(cx/re z) (cx/im z)]

(kind/test-last [= [3.0 4.0]])

;; `count` is 0 for scalars and `seq` returns nil.

[(count z) (seq z)]

(kind/test-last [= [0 nil]])

;; ### Matrix construction
;;
;; Passing 2D arrays creates a complex matrix (underlying `[r c 2]`).

(def M (cx/complex-tensor [[1.0 2.0] [3.0 4.0]]
                          [[5.0 6.0] [7.0 8.0]]))

(cx/complex-shape M)

(kind/test-last [= [2 2]])

;; ## Accessing parts
;;
;; `re` and `im` always slice the last axis of the underlying tensor,
;; returning **zero-copy views** — no memory is allocated.

;; For vectors:

(let [ct (cx/complex-tensor [10.0 20.0 30.0] [0.1 0.2 0.3])]
  [(vec (cx/re ct)) (vec (cx/im ct))])

(kind/test-last [= [[10.0 20.0 30.0] [0.1 0.2 0.3]]])

;; For matrices, `re` and `im` return 2D tensor views:

(let [ct (cx/complex-tensor [[1.0 2.0] [3.0 4.0]]
                            [[5.0 6.0] [7.0 8.0]])]
  (vec (dtype/shape (cx/re ct))))

(kind/test-last [= [2 2]])

;; `complex-shape` reports the shape without the trailing 2:

(cx/complex-shape (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0]))

(kind/test-last [= [3]])

;; ## Element access
;;
;; ComplexTensors implement `Counted`, `Indexed`, `IFn`, and `Seqable`.

;; ### Indexing into a vector
;;
;; `(ct i)` returns a scalar ComplexTensor.

(let [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  [(cx/re (ct 0)) (cx/im (ct 0))])

(kind/test-last [= [1.0 4.0]])

(let [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  (cx/scalar? (ct 1)))

(kind/test-last [true?])

;; `nth` with a default works:

(nth (cx/complex-tensor [1.0] [2.0]) 99 :missing)

(kind/test-last [= :missing])

;; ### Indexing into a matrix
;;
;; `(ct i)` on a matrix returns a complex vector (one row).

(let [ct (cx/complex-tensor [[1.0 2.0] [3.0 4.0]]
                            [[5.0 6.0] [7.0 8.0]])
      row0 (ct 0)]
  {:shape (cx/complex-shape row0)
   :re (vec (cx/re row0))
   :im (vec (cx/im row0))})

(kind/test-last [(fn [v] (and (= (:shape v) [2])
                              (= (:re v) [1.0 2.0])
                              (= (:im v) [5.0 6.0])))])

;; Nested access reaches scalars:

(let [ct (cx/complex-tensor [[1.0 2.0] [3.0 4.0]]
                            [[5.0 6.0] [7.0 8.0]])]
  [(cx/re ((ct 1) 1)) (cx/im ((ct 1) 1))])

(kind/test-last [= [4.0 8.0]])

;; ### Seq
;;
;; `seq` returns a lazy sequence of sub-ComplexTensors.

(let [ct (cx/complex-tensor [1.0 2.0] [3.0 4.0])]
  (mapv cx/re (seq ct)))

(kind/test-last [= [1.0 2.0]])

(let [ct (cx/complex-tensor [1.0 2.0] [3.0 4.0])]
  (every? cx/scalar? (seq ct)))

(kind/test-last [true?])

;; ## Arithmetic
;;
;; All arithmetic operates pointwise on the complex elements,
;; using dfn operations on the underlying real/imaginary views.

;; ### Complex multiply
;;
;; $(a+bi)(c+di) = (ac - bd) + (ad + bc)i$

(let [a (cx/complex-tensor [1.0 2.0] [3.0 4.0])
      b (cx/complex-tensor [5.0 6.0] [7.0 8.0])
      c (cx/cmul a b)]
  {:re (vec (cx/re c))
   :im (vec (cx/im c))})

;; $(1+3i)(5+7i) = (5-21) + (7+15)i = -16 + 22i$
;;
;; $(2+4i)(6+8i) = (12-32) + (16+24)i = -20 + 40i$

(kind/test-last [(fn [v] (and (= (:re v) [-16.0 -20.0])
                              (= (:im v) [22.0 40.0])))])

;; ### Complex conjugate
;;
;; $\overline{a+bi} = a - bi$

(let [ct (cx/cconj (cx/complex-tensor [1.0 2.0] [3.0 -4.0]))]
  {:re (vec (cx/re ct))
   :im (vec (cx/im ct))})

(kind/test-last [(fn [v] (and (= (:re v) [1.0 2.0])
                              (= (:im v) [-3.0 4.0])))])

;; ### Real scaling
;;
;; $\alpha(a+bi) = \alpha a + \alpha b \, i$

(let [ct (cx/cscale (cx/complex-tensor [1.0 2.0] [3.0 4.0]) 2.0)]
  {:re (vec (cx/re ct))
   :im (vec (cx/im ct))})

(kind/test-last [(fn [v] (and (= (:re v) [2.0 4.0])
                              (= (:im v) [6.0 8.0])))])

;; ### Magnitude
;;
;; $|a+bi| = \sqrt{a^2 + b^2}$

(let [m (cx/cabs (cx/complex-tensor [3.0 0.0] [4.0 1.0]))]
  [(double (m 0)) (double (m 1))])

;; $|3+4i| = 5$, $|0+i| = 1$

(kind/test-last [(fn [v] (and (< (Math/abs (- (first v) 5.0)) 1e-10)
                              (< (Math/abs (- (second v) 1.0)) 1e-10)))])

;; ## Inner products

;; ### Bilinear dot product
;;
;; $\langle a, b \rangle = \sum_i a_i \cdot b_i$
;;
;; This is the bilinear (not sesquilinear) form.

(let [a (cx/complex-tensor [1.0 0.0] [0.0 1.0])   ;; [1, i]
      b (cx/complex-tensor [0.0 1.0] [1.0 0.0])]  ;; [i, 1]
  (cx/cdot a b))

;; $1 \cdot i + i \cdot 1 = 2i$

(kind/test-last [(fn [[re im]]
                   (and (< (Math/abs re) 1e-10)
                        (< (Math/abs (- im 2.0)) 1e-10)))])

;; ### Hermitian inner product
;;
;; $\langle a, b \rangle_H = \sum_i a_i \cdot \overline{b_i}$
;;
;; The standard physics/math convention: conjugate-linear in the
;; second argument.

(let [a (cx/complex-tensor [1.0 0.0] [0.0 1.0])   ;; [1, i]
      b (cx/complex-tensor [0.0 1.0] [1.0 0.0])]  ;; [i, 1]
  (cx/cdot-conj a b))

;; $1 \cdot \overline{i} + i \cdot \overline{1} = 1 \cdot (-i) + i \cdot 1 = 0$

(kind/test-last [(fn [[re im]]
                   (and (< (Math/abs re) 1e-10)
                        (< (Math/abs im) 1e-10)))])

;; $\langle a, a \rangle_H = \|a\|^2$ is always real and non-negative.

(let [a (cx/complex-tensor [3.0 1.0] [4.0 2.0])
      [re im] (cx/cdot-conj a a)]
  {:norm-sq re :im-part im})

;; $|3+4i|^2 + |1+2i|^2 = 25 + 5 = 30$

(kind/test-last [(fn [v] (and (< (Math/abs (- (:norm-sq v) 30.0)) 1e-10)
                              (< (Math/abs (:im-part v)) 1e-10)))])

;; ## Algebraic identities
;;
;; Thorough verification of the algebraic laws that complex arithmetic
;; must satisfy. We use several test vectors throughout.

(def a (cx/complex-tensor [1.0 -2.0 3.0] [4.0 5.0 -6.0]))
(def b (cx/complex-tensor [-3.0 0.5 2.0] [1.0 -1.5 7.0]))
(def c (cx/complex-tensor [0.0 4.0 -1.0] [2.0 -3.0 0.5]))

(defn approx=
  "Check that two ComplexTensors are approximately equal."
  [x y tol]
  (let [re-diff (dfn/- (cx/re x) (cx/re y))
        im-diff (dfn/- (cx/im x) (cx/im y))
        max-re (dfn/reduce-max (dfn/abs re-diff))
        max-im (dfn/reduce-max (dfn/abs im-diff))]
    (and (< max-re tol) (< max-im tol))))

;; ### Commutativity of multiplication
;;
;; $a \cdot b = b \cdot a$

(approx= (cx/cmul a b) (cx/cmul b a) 1e-10)

(kind/test-last [true?])

;; ### Associativity of multiplication
;;
;; $(a \cdot b) \cdot c = a \cdot (b \cdot c)$

(approx= (cx/cmul (cx/cmul a b) c)
         (cx/cmul a (cx/cmul b c))
         1e-10)

(kind/test-last [true?])

;; ### Multiplicative identity
;;
;; $a \cdot 1 = a$

(let [one (cx/complex-tensor-real [1.0 1.0 1.0])]
  (approx= (cx/cmul a one) a 1e-10))

(kind/test-last [true?])

;; ### Complex conjugate is an involution
;;
;; $\overline{\overline{a}} = a$

(approx= (cx/cconj (cx/cconj a)) a 1e-10)

(kind/test-last [true?])

;; ### Conjugate distributes over multiplication
;;
;; $\overline{a \cdot b} = \overline{a} \cdot \overline{b}$

(approx= (cx/cconj (cx/cmul a b))
         (cx/cmul (cx/cconj a) (cx/cconj b))
         1e-10)

(kind/test-last [true?])

;; ### Multiplication by conjugate gives squared magnitude
;;
;; $a \cdot \overline{a} = |a|^2$ (purely real)

(let [prod (cx/cmul a (cx/cconj a))
      mag-sq (dfn/+ (dfn/* (cx/re a) (cx/re a))
                    (dfn/* (cx/im a) (cx/im a)))]
  (and (< (dfn/reduce-max (dfn/abs (dfn/- (cx/re prod) mag-sq))) 1e-10)
       (< (dfn/reduce-max (dfn/abs (cx/im prod))) 1e-10)))

(kind/test-last [true?])

;; ### Magnitude is multiplicative
;;
;; $|a \cdot b| = |a| \cdot |b|$

(let [lhs (cx/cabs (cx/cmul a b))
      rhs (dfn/* (cx/cabs a) (cx/cabs b))]
  (< (dfn/reduce-max (dfn/abs (dfn/- lhs rhs))) 1e-10))

(kind/test-last [true?])

;; ### Scale distributes over multiply
;;
;; $\alpha(a \cdot b) = (\alpha \, a) \cdot b$

(let [alpha 3.7]
  (approx= (cx/cscale (cx/cmul a b) alpha)
           (cx/cmul (cx/cscale a alpha) b)
           1e-10))

(kind/test-last [true?])

;; ### Conjugate commutes with scaling
;;
;; $\overline{\alpha \, a} = \alpha \, \overline{a}$ (for real $\alpha$)

(let [alpha -2.5]
  (approx= (cx/cconj (cx/cscale a alpha))
           (cx/cscale (cx/cconj a) alpha)
           1e-10))

(kind/test-last [true?])

;; ### Hermitian symmetry
;;
;; $\langle a, b \rangle_H = \overline{\langle b, a \rangle_H}$

(let [[re-ab im-ab] (cx/cdot-conj a b)
      [re-ba im-ba] (cx/cdot-conj b a)]
  (and (< (Math/abs (- re-ab re-ba)) 1e-10)
       (< (Math/abs (+ im-ab im-ba)) 1e-10)))

(kind/test-last [true?])

;; ### Positive definiteness
;;
;; $\langle a, a \rangle_H \geq 0$, with equality iff $a = 0$.

(let [[re-aa im-aa] (cx/cdot-conj a a)]
  (and (>= re-aa 0.0)
       (< (Math/abs im-aa) 1e-10)))

(kind/test-last [true?])

(let [zero (cx/complex-tensor-real [0.0 0.0 0.0])
      [re-00 _] (cx/cdot-conj zero zero)]
  (< (Math/abs re-00) 1e-10))

(kind/test-last [true?])

;; ### Hermitian inner product vs norm
;;
;; $\langle a, a \rangle_H = \sum_i |a_i|^2$

(let [[re-aa _] (cx/cdot-conj a a)
      norm-sq (dfn/sum (dfn/+ (dfn/* (cx/re a) (cx/re a))
                              (dfn/* (cx/im a) (cx/im a))))]
  (< (Math/abs (- re-aa norm-sq)) 1e-10))

(kind/test-last [true?])

;; ### Bilinear dot product symmetry
;;
;; $\langle a, b \rangle = \langle b, a \rangle$ (no conjugation)

(let [[re-ab im-ab] (cx/cdot a b)
      [re-ba im-ba] (cx/cdot b a)]
  (and (< (Math/abs (- re-ab re-ba)) 1e-10)
       (< (Math/abs (- im-ab im-ba)) 1e-10)))

(kind/test-last [true?])

;; ### Relationship between cdot and cdot-conj
;;
;; $\langle a, b \rangle = \langle a, \overline{b} \rangle_H$

(let [[re-dot im-dot] (cx/cdot a b)
      [re-conj im-conj] (cx/cdot-conj a (cx/cconj b))]
  (and (< (Math/abs (- re-dot re-conj)) 1e-10)
       (< (Math/abs (- im-dot im-conj)) 1e-10)))

(kind/test-last [true?])

;; ### Cauchy-Schwarz inequality
;;
;; $|\langle a, b \rangle_H|^2 \leq \langle a, a \rangle_H \cdot \langle b, b \rangle_H$

(let [[re-ab im-ab] (cx/cdot-conj a b)
      [re-aa _] (cx/cdot-conj a a)
      [re-bb _] (cx/cdot-conj b b)
      lhs (+ (* re-ab re-ab) (* im-ab im-ab))
      rhs (* re-aa re-bb)]
  (<= (- lhs 1e-10) rhs))

(kind/test-last [true?])

;; ### Scalar multiply and inner product compatibility
;;
;; $\langle \alpha a, b \rangle_H = \alpha \, \langle a, b \rangle_H$

(let [alpha 3.7
      [re1 im1] (cx/cdot-conj (cx/cscale a alpha) b)
      [re2 im2] (cx/cdot-conj a b)]
  (and (< (Math/abs (- re1 (* alpha re2))) 1e-10)
       (< (Math/abs (- im1 (* alpha im2))) 1e-10)))

(kind/test-last [true?])

;; ## Zero-copy internals
;;
;; ComplexTensor is designed for high-performance interop. The
;; underlying storage is a flat `double[]` in interleaved format
;; `[re₀ im₀ re₁ im₁ ...]`.

;; `->tensor` exposes the backing `[... 2]` tensor:

(let [ct (cx/complex-tensor [1.0 2.0] [3.0 4.0])]
  (vec (dtype/shape (cx/->tensor ct))))

(kind/test-last [= [2 2]])

;; `->double-array` returns the **identical** Java array — no copy.

(let [ct (cx/complex-tensor [1.0 2.0] [3.0 4.0])
      arr (cx/->double-array ct)]
  {:identical? (identical? arr (cx/->double-array ct))
   :values (vec arr)})

;; The interleaved layout `[re₀ im₀ re₁ im₁]`:

(kind/test-last [(fn [v] (and (:identical? v)
                              (= (:values v) [1.0 3.0 2.0 4.0])))])

;; This interleaved format matches EJML's `ZMatrixRMaj` storage,
;; enabling future zero-copy interop with Java linear algebra libraries.

;; ## Matrix ComplexTensors
;;
;; Rank-2 ComplexTensors represent complex matrices. Rows are accessed
;; via indexing, and `re`/`im` return full 2D tensor views.

(def mat (cx/complex-tensor [[1.0 2.0 3.0]
                             [4.0 5.0 6.0]]
                            [[0.1 0.2 0.3]
                             [0.4 0.5 0.6]]))

(cx/complex-shape mat)

(kind/test-last [= [2 3]])

(count mat)

(kind/test-last [= 2])

;; Each row is a complex vector:

(let [row (mat 0)]
  {:shape (cx/complex-shape row)
   :re (vec (cx/re row))})

(kind/test-last [(fn [v] (and (= (:shape v) [3])
                              (= (:re v) [1.0 2.0 3.0])))])

;; `re` and `im` on a matrix return `[r c]` tensors:

(let [re-mat (cx/re mat)
      shape (vec (dtype/shape re-mat))]
  {:shape shape
   :row0 (vec (tensor/select re-mat 0 :all))
   :row1 (vec (tensor/select re-mat 1 :all))})

(kind/test-last [(fn [v] (and (= (:shape v) [2 3])
                              (= (:row0 v) [1.0 2.0 3.0])
                              (= (:row1 v) [4.0 5.0 6.0])))])

;; ## Printing
;;
;; ComplexTensors print with a header line (like dtype-next tensors)
;; followed by the content in nested brackets. The header shows the
;; **complex shape** â the underlying tensor shape without the trailing 2.

;; ### Scalars
;;
;; The general form is `a+bi`, with special cases for cleaner output.

(cx/complex-tensor (tensor/->tensor [3.0 4.0]))

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "3.0+4.0i"))])

;; Negative imaginary parts use a minus sign directly:

(cx/complex-tensor (tensor/->tensor [3.0 -4.0]))

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "3.0-4.0i"))])

;; Purely real values omit the imaginary part:

(cx/complex-tensor (tensor/->tensor [5.0 0.0]))

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "5.0"))])

;; Zero prints as `0.0`:

(cx/complex-tensor (tensor/->tensor [0.0 0.0]))

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "0.0"))])

;; Purely imaginary values omit the real part:

(cx/complex-tensor (tensor/->tensor [0.0 3.0]))

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "3.0i"))])

;; The unit imaginary number prints as `i` (not `1.0i`):

(cx/complex-tensor (tensor/->tensor [0.0 1.0]))

(kind/test-last [(fn [v] (clojure.string/ends-with? (str v) "\ni"))])

(cx/complex-tensor (tensor/->tensor [0.0 -1.0]))

(kind/test-last [(fn [v] (clojure.string/ends-with? (str v) "\n-i"))])

;; Negative real parts work as expected:

(cx/complex-tensor (tensor/->tensor [-2.0 3.0]))

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "-2.0+3.0i"))])

;; ### Vectors
;;
;; Vectors print as bracketed, comma-separated lists. Each element
;; uses the same scalar formatting rules.

(cx/complex-tensor [1.0 3.0] [2.0 4.0])

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "[1.0+2.0i, 3.0+4.0i]"))])

(cx/complex-tensor-real [1.0 2.0])

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "[1.0, 2.0]"))])

;; A mixed vector with real, imaginary, and complex entries:

(cx/complex-tensor [1.0 0.0 -1.0] [2.0 3.0 0.0])

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "[1.0+2.0i, 3.0i, -1.0]"))])

;; Long vectors are truncated after 20 elements:

(cx/complex-tensor-real (vec (range 25.0)))

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "... (25 total)"))])

;; ### Matrices
;;
;; Matrices print with nested brackets and indentation, like dtype-next tensors.

(cx/complex-tensor [[1.0 2.0] [3.0 4.0]]
                   [[5.0 6.0] [7.0 8.0]])

(kind/test-last [(fn [v] (and (clojure.string/includes? (str v) "#ComplexTensor [2 2]")
                              (clojure.string/includes? (str v) "[1.0+5.0i, 2.0+6.0i]")
                              (clojure.string/includes? (str v) "[3.0+7.0i, 4.0+8.0i]")))])

;; ### Higher ranks
;;
;; 3-tensors and beyond also print their full content with deeper nesting.

(cx/complex-tensor [[[1.0 2.0] [3.0 4.0]]
                    [[5.0 6.0] [7.0 8.0]]]
                   [[[0.1 0.2] [0.3 0.4]]
                    [[0.5 0.6] [0.7 0.8]]])

(kind/test-last [(fn [v] (clojure.string/includes? (str v) "#ComplexTensor [2 2 2]"))])

;; ## Summary
;;
;; ComplexTensor provides a tensor-backed complex number type that is:
;;
;; - **Zero-copy**: `re`/`im` return strided views, `->double-array` returns the backing array
;; - **Composable**: Works with all `dfn` operations on its views
;; - **Rank-polymorphic**: Scalars, vectors, matrices, and higher ranks
;; - **Interop-ready**: Interleaved `double[]` matches EJML's `ZMatrixRMaj`
;; - **Self-contained**: Zero dependencies on harmonica internals
