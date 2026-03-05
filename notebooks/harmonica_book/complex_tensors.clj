;; # Complex Tensors
;;
;; **ComplexTensor** is a [dtype-next](https://github.com/cnuernber/dtype-next) tensor-backed complex number type.
;; It wraps a real tensor whose **last dimension is 2** — interleaved
;; real/imaginary pairs — providing zero-copy access to real and
;; imaginary parts as strided tensor views.
;;
;; | Underlying shape | Complex interpretation | `re` / `im` returns |
;; |:-----------------|:----------------------|:---------------------|
;; | `[2]` | scalar complex number | double |
;; | `[n 2]` | complex vector, length n | `[n]` tensor view |
;; | `[r c 2]` | complex r × c matrix | `[r c]` tensor view |
;;
;; ComplexTensor is provided by [La Linea](https://github.com/scicloj/lalinea) —
;; a dtype-next linear algebra library. It depends only on dtype-next
;; and has been extracted from harmonica into its own library.

(ns harmonica-book.complex-tensors
  (:require
   [scicloj.lalinea.tensor :as t]
   [scicloj.lalinea.elementwise :as el]
   [scicloj.lalinea.linalg :as la]
   [tech.v3.tensor :as tensor]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [scicloj.kindly.v4.kind :as kind]))

(defn re-im
  "Extract [re im] pair from a scalar ComplexTensor."
  [ct]
  [(el/re ct) (el/im ct)])
;; ## Construction
;;
;; There are three ways to create a ComplexTensor.

;; ### From separate real and imaginary parts
;;
;; The most common constructor takes two arrays (or seqs, or tensors)
;; of the same shape and interleaves them into a single backing tensor.

(t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])

(kind/test-last [(fn [v] (= [3] (t/complex-shape v)))])

;; Real and imaginary parts are accessible via `re` and `im`.

(let [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  {:re (vec (el/re ct))
   :im (vec (el/im ct))})

(kind/test-last [(fn [v] (and (= (:re v) [1.0 2.0 3.0])
                              (= (:im v) [4.0 5.0 6.0])))])

;; ### Wrapping an existing tensor
;;
;; If you already have a `[... 2]` tensor, wrap it directly.
;; This is zero-copy — no data is moved.

(t/complex-tensor (tensor/->tensor [[1.0 2.0] [3.0 4.0]]))

(kind/test-last [(fn [v] (and (= [2] (t/complex-shape v))
                              (= [1.0 3.0] (vec (el/re v)))
                              (= [2.0 4.0] (vec (el/im v)))))])

;; ### Real-only construction
;;
;; For purely real data, `complex-tensor-real` sets all imaginary parts to zero.

(t/complex-tensor-real [5.0 6.0 7.0])

(kind/test-last [(fn [v] (and (= [5.0 6.0 7.0] (vec (el/re v)))
                              (= [0.0 0.0 0.0] (vec (el/im v)))))])

;; ### Scalar complex numbers
;;
;; A scalar ComplexTensor has underlying shape `[2]`.

(def z (t/complex-tensor (tensor/->tensor [3.0 4.0])))

(t/scalar? z)

(kind/test-last [true?])

;; For scalars, `re` and `im` return doubles.

[(el/re z) (el/im z)]

(kind/test-last [= [3.0 4.0]])

;; `count` is 0 for scalars and `seq` returns nil.

[(count z) (seq z)]

(kind/test-last [= [0 nil]])

;; ### Matrix construction
;;
;; Passing 2D arrays creates a complex matrix (underlying `[r c 2]`).

(def M (t/complex-tensor [[1.0 2.0] [3.0 4.0]]
                         [[5.0 6.0] [7.0 8.0]]))

(t/complex-shape M)

(kind/test-last [= [2 2]])

;; ## Accessing parts
;;
;; `re` and `im` always slice the last axis of the underlying tensor,
;; returning **zero-copy views** — no memory is allocated.

;; For vectors:

(let [ct (t/complex-tensor [10.0 20.0 30.0] [0.1 0.2 0.3])]
  [(vec (el/re ct)) (vec (el/im ct))])

(kind/test-last [= [[10.0 20.0 30.0] [0.1 0.2 0.3]]])

;; For matrices, `re` and `im` return 2D tensor views:

(let [ct (t/complex-tensor [[1.0 2.0] [3.0 4.0]]
                           [[5.0 6.0] [7.0 8.0]])]
  (vec (dtype/shape (el/re ct))))

(kind/test-last [= [2 2]])

;; `complex-shape` reports the shape without the trailing 2:

(t/complex-shape (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0]))

(kind/test-last [= [3]])

;; ## Element access
;;
;; ComplexTensors implement `Counted`, `Indexed`, `IFn`, and `Seqable`.

;; ### Indexing into a vector
;;
;; `(ct i)` returns a scalar ComplexTensor.

(let [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  [(el/re (ct 0)) (el/im (ct 0))])

(kind/test-last [= [1.0 4.0]])

(let [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
  (t/scalar? (ct 1)))

(kind/test-last [true?])

;; `nth` with a default works:

(nth (t/complex-tensor [1.0] [2.0]) 99 :missing)

(kind/test-last [= :missing])

;; ### Indexing into a matrix
;;
;; `(ct i)` on a matrix returns a complex vector (one row).

(let [ct (t/complex-tensor [[1.0 2.0] [3.0 4.0]]
                           [[5.0 6.0] [7.0 8.0]])
      row0 (ct 0)]
  {:shape (t/complex-shape row0)
   :re (vec (el/re row0))
   :im (vec (el/im row0))})

(kind/test-last [(fn [v] (and (= (:shape v) [2])
                              (= (:re v) [1.0 2.0])
                              (= (:im v) [5.0 6.0])))])

;; Nested access reaches scalars:

(let [ct (t/complex-tensor [[1.0 2.0] [3.0 4.0]]
                           [[5.0 6.0] [7.0 8.0]])]
  [(el/re ((ct 1) 1)) (el/im ((ct 1) 1))])

(kind/test-last [= [4.0 8.0]])

;; ### Seq
;;
;; `seq` returns a lazy sequence of sub-ComplexTensors.

(let [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
  (mapv el/re (seq ct)))

(kind/test-last [= [1.0 2.0]])

(let [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
  (every? t/scalar? (seq ct)))

(kind/test-last [true?])

;; ## Arithmetic
;;
;; All arithmetic operates pointwise on the complex elements,
;; using dfn operations on the underlying real/imaginary views.

;; ### Complex multiply
;;
;; $(a+bi)(c+di) = (ac - bd) + (ad + bc)i$

(let [a (t/complex-tensor [1.0 2.0] [3.0 4.0])
      b (t/complex-tensor [5.0 6.0] [7.0 8.0])
      c (el/* a b)]
  {:re (vec (el/re c))
   :im (vec (el/im c))})

;; $(1+3i)(5+7i) = (5-21) + (7+15)i = -16 + 22i$
;;
;; $(2+4i)(6+8i) = (12-32) + (16+24)i = -20 + 40i$

(kind/test-last [(fn [v] (and (= (:re v) [-16.0 -20.0])
                              (= (:im v) [22.0 40.0])))])

;; ### Complex conjugate
;;
;; $\overline{a+bi} = a - bi$

(let [ct (el/conj (t/complex-tensor [1.0 2.0] [3.0 -4.0]))]
  {:re (vec (el/re ct))
   :im (vec (el/im ct))})

(kind/test-last [(fn [v] (and (= (:re v) [1.0 2.0])
                              (= (:im v) [-3.0 4.0])))])

;; ### Real scaling
;;
;; $\alpha(a+bi) = \alpha a + \alpha b \, i$

(let [ct (el/scale (t/complex-tensor [1.0 2.0] [3.0 4.0]) 2.0)]
  {:re (vec (el/re ct))
   :im (vec (el/im ct))})

(kind/test-last [(fn [v] (and (= (:re v) [2.0 4.0])
                              (= (:im v) [6.0 8.0])))])

;; ### Magnitude
;;
;; $|a+bi| = \sqrt{a^2 + b^2}$

(let [m (el/abs (t/complex-tensor [3.0 0.0] [4.0 1.0]))]
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

(let [a (t/complex-tensor [1.0 0.0] [0.0 1.0])   ;; [1, i]
      b (t/complex-tensor [0.0 1.0] [1.0 0.0])]  ;; [i, 1]
  (el/sum (el/* a b)))

;; $1 \cdot i + i \cdot 1 = 2i$

(kind/test-last [(fn [v] (let [re (el/re v) im (el/im v)]
                           (and (< (Math/abs re) 1e-10)
                                (< (Math/abs (- im 2.0)) 1e-10))))])

;; ### Hermitian inner product
;;
;; $\langle a, b \rangle_H = \sum_i a_i \cdot \overline{b_i}$
;;
;; The standard physics/math convention: conjugate-linear in the
;; second argument.

(let [a (t/complex-tensor [1.0 0.0] [0.0 1.0])   ;; [1, i]
      b (t/complex-tensor [0.0 1.0] [1.0 0.0])]  ;; [i, 1]
  (la/dot-conj a b))

;; $1 \cdot \overline{i} + i \cdot \overline{1} = 1 \cdot (-i) + i \cdot 1 = 0$

(kind/test-last [(fn [v] (let [re (el/re v) im (el/im v)]
                           (and (< (Math/abs re) 1e-10)
                                (< (Math/abs im) 1e-10))))])

;; $\langle a, a \rangle_H = \|a\|^2$ is always real and non-negative.

(let [a (t/complex-tensor [3.0 1.0] [4.0 2.0])
      [re im] (re-im (la/dot-conj a a))]
  {:norm-sq re :im-part im})

;; $|3+4i|^2 + |1+2i|^2 = 25 + 5 = 30$

(kind/test-last [(fn [v] (and (< (Math/abs (- (:norm-sq v) 30.0)) 1e-10)
                              (< (Math/abs (:im-part v)) 1e-10)))])

;; ## Algebraic identities
;;
;; Thorough verification of the algebraic laws that complex arithmetic
;; must satisfy. We use several test vectors throughout.

(def a (t/complex-tensor [1.0 -2.0 3.0] [4.0 5.0 -6.0]))
(def b (t/complex-tensor [-3.0 0.5 2.0] [1.0 -1.5 7.0]))
(def c (t/complex-tensor [0.0 4.0 -1.0] [2.0 -3.0 0.5]))

(defn approx=
  "Check that two ComplexTensors are approximately equal."
  [x y tol]
  (let [re-diff (dfn/- (el/re x) (el/re y))
        im-diff (dfn/- (el/im x) (el/im y))
        max-re (dfn/reduce-max (dfn/abs re-diff))
        max-im (dfn/reduce-max (dfn/abs im-diff))]
    (and (< max-re tol) (< max-im tol))))

;; ### Commutativity of multiplication
;;
;; $a \cdot b = b \cdot a$

(approx= (el/* a b) (el/* b a) 1e-10)

(kind/test-last [true?])

;; ### Associativity of multiplication
;;
;; $(a \cdot b) \cdot c = a \cdot (b \cdot c)$

(approx= (el/* (el/* a b) c)
         (el/* a (el/* b c))
         1e-10)

(kind/test-last [true?])

;; ### Multiplicative identity
;;
;; $a \cdot 1 = a$

(let [one (t/complex-tensor-real [1.0 1.0 1.0])]
  (approx= (el/* a one) a 1e-10))

(kind/test-last [true?])

;; ### Complex conjugate is an involution
;;
;; $\overline{\overline{a}} = a$

(approx= (el/conj (el/conj a)) a 1e-10)

(kind/test-last [true?])

;; ### Conjugate distributes over multiplication
;;
;; $\overline{a \cdot b} = \overline{a} \cdot \overline{b}$

(approx= (el/conj (el/* a b))
         (el/* (el/conj a) (el/conj b))
         1e-10)

(kind/test-last [true?])

;; ### Multiplication by conjugate gives squared magnitude
;;
;; $a \cdot \overline{a} = |a|^2$ (purely real)

(let [prod (el/* a (el/conj a))
      mag-sq (dfn/+ (dfn/* (el/re a) (el/re a))
                    (dfn/* (el/im a) (el/im a)))]
  (and (< (dfn/reduce-max (dfn/abs (dfn/- (el/re prod) mag-sq))) 1e-10)
       (< (dfn/reduce-max (dfn/abs (el/im prod))) 1e-10)))

(kind/test-last [true?])

;; ### Magnitude is multiplicative
;;
;; $|a \cdot b| = |a| \cdot |b|$

(let [lhs (el/abs (el/* a b))
      rhs (dfn/* (el/abs a) (el/abs b))]
  (< (dfn/reduce-max (dfn/abs (dfn/- lhs rhs))) 1e-10))

(kind/test-last [true?])

;; ### Scale distributes over multiply
;;
;; $\alpha(a \cdot b) = (\alpha \, a) \cdot b$

(let [alpha 3.7]
  (approx= (el/scale (el/* a b) alpha)
           (el/* (el/scale a alpha) b)
           1e-10))

(kind/test-last [true?])

;; ### Conjugate commutes with scaling
;;
;; $\overline{\alpha \, a} = \alpha \, \overline{a}$ (for real $\alpha$)

(let [alpha -2.5]
  (approx= (el/conj (el/scale a alpha))
           (el/scale (el/conj a) alpha)
           1e-10))

(kind/test-last [true?])

;; ### Hermitian symmetry
;;
;; $\langle a, b \rangle_H = \overline{\langle b, a \rangle_H}$

(let [[re-ab im-ab] (re-im (la/dot-conj a b))
      [re-ba im-ba] (re-im (la/dot-conj b a))]
  (and (< (Math/abs (- re-ab re-ba)) 1e-10)
       (< (Math/abs (+ im-ab im-ba)) 1e-10)))

(kind/test-last [true?])

;; ### Positive definiteness
;;
;; $\langle a, a \rangle_H \geq 0$, with equality iff $a = 0$.

(let [[re-aa im-aa] (re-im (la/dot-conj a a))]
  (and (>= re-aa 0.0)
       (< (Math/abs im-aa) 1e-10)))

(kind/test-last [true?])

(let [zero (t/complex-tensor-real [0.0 0.0 0.0])
      [re-00 _] (re-im (la/dot-conj zero zero))]
  (< (Math/abs re-00) 1e-10))

(kind/test-last [true?])

;; ### Hermitian inner product vs norm
;;
;; $\langle a, a \rangle_H = \sum_i |a_i|^2$

(let [[re-aa _] (re-im (la/dot-conj a a))
      norm-sq (dfn/sum (dfn/+ (dfn/* (el/re a) (el/re a))
                              (dfn/* (el/im a) (el/im a))))]
  (< (Math/abs (- re-aa norm-sq)) 1e-10))

(kind/test-last [true?])

;; ### Bilinear dot product symmetry
;;
;; $\langle a, b \rangle = \langle b, a \rangle$ (no conjugation)

(let [[re-ab im-ab] (re-im (el/sum (el/* a b)))
      [re-ba im-ba] (re-im (el/sum (el/* b a)))]
  (and (< (Math/abs (- re-ab re-ba)) 1e-10)
       (< (Math/abs (- im-ab im-ba)) 1e-10)))

(kind/test-last [true?])

;; ### Relationship between cdot and cdot-conj
;;
;; $\langle a, b \rangle = \langle a, \overline{b} \rangle_H$

(let [[re-dot im-dot] (re-im (el/sum (el/* a b)))
      [re-conj im-conj] (re-im (la/dot-conj a (el/conj b)))]
  (and (< (Math/abs (- re-dot re-conj)) 1e-10)
       (< (Math/abs (- im-dot im-conj)) 1e-10)))

(kind/test-last [true?])

;; ### Cauchy-Schwarz inequality
;;
;; $|\langle a, b \rangle_H|^2 \leq \langle a, a \rangle_H \cdot \langle b, b \rangle_H$

(let [[re-ab im-ab] (re-im (la/dot-conj a b))
      [re-aa _] (re-im (la/dot-conj a a))
      [re-bb _] (re-im (la/dot-conj b b))
      lhs (+ (* re-ab re-ab) (* im-ab im-ab))
      rhs (* re-aa re-bb)]
  (<= (- lhs 1e-10) rhs))

(kind/test-last [true?])

;; ### Scalar multiply and inner product compatibility
;;
;; $\langle \alpha a, b \rangle_H = \alpha \, \langle a, b \rangle_H$

(let [alpha 3.7
      [re1 im1] (re-im (la/dot-conj (el/scale a alpha) b))
      [re2 im2] (re-im (la/dot-conj a b))]
  (and (< (Math/abs (- re1 (* alpha re2))) 1e-10)
       (< (Math/abs (- im1 (* alpha im2))) 1e-10)))

(kind/test-last [true?])

;; ## Zero-copy internals
;;
;; ComplexTensor is designed for high-performance interop. The
;; underlying storage is a flat `double[]` in interleaved format
;; `[re₀ im₀ re₁ im₁ ...]`.

;; `->tensor` exposes the backing `[... 2]` tensor:

(let [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
  (vec (dtype/shape (t/->tensor ct))))

(kind/test-last [= [2 2]])

;; `->double-array` returns the interleaved `double[]` backing the tensor.

(let [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
  (vec (t/->double-array ct)))

;; The interleaved layout `[re₀ im₀ re₁ im₁]`:

(kind/test-last [= [1.0 3.0 2.0 4.0]])

;; This interleaved format matches EJML's `ZMatrixRMaj` storage —
;; see [EJML Integration](ejml_integration.html).

;; ## Matrix ComplexTensors
;;
;; Rank-2 ComplexTensors represent complex matrices. Rows are accessed
;; via indexing, and `re`/`im` return full 2D tensor views.

(def mat (t/complex-tensor [[1.0 2.0 3.0]
                            [4.0 5.0 6.0]]
                           [[0.1 0.2 0.3]
                            [0.4 0.5 0.6]]))

(t/complex-shape mat)

(kind/test-last [= [2 3]])

(count mat)

(kind/test-last [= 2])

;; Each row is a complex vector:

(let [row (mat 0)]
  {:shape (t/complex-shape row)
   :re (vec (el/re row))})

(kind/test-last [(fn [v] (and (= (:shape v) [3])
                              (= (:re v) [1.0 2.0 3.0])))])

;; `re` and `im` on a matrix return `[r c]` tensors:

(let [re-mat (el/re mat)
      shape (vec (dtype/shape re-mat))]
  {:shape shape
   :row0 (vec (tensor/select re-mat 0 :all))
   :row1 (vec (tensor/select re-mat 1 :all))})

(kind/test-last [(fn [v] (and (= (:shape v) [2 3])
                              (= (:row0 v) [1.0 2.0 3.0])
                              (= (:row1 v) [4.0 5.0 6.0])))])

;; ## Printing
;;
;; ComplexTensors print with a compact summary showing the element type
;; and complex shape (the underlying tensor shape without the trailing 2).

(str (t/complex 3.0 4.0))

(kind/test-last [(fn [v] (clojure.string/includes? v "ComplexTensor"))])

(str (t/complex-tensor [1.0 2.0] [3.0 4.0]))

(kind/test-last [= "ComplexTensor<float64>[2]"])

(str (t/complex-tensor [[1.0 2.0] [3.0 4.0]]
                       [[5.0 6.0] [7.0 8.0]]))

(kind/test-last [= "ComplexTensor<float64>[2 2]"])

;; To see the complex values, use `format-cx` from the harmonica API
;; or inspect `re`/`im` views directly.

;; ## Summary
;;
;; ComplexTensor provides a tensor-backed complex number type that is:
;;
;; - **Zero-copy**: `re`/`im` return strided views, `->double-array` returns the backing array
;; - **Composable**: Works with all `dfn` operations on its views
;; - **Rank-polymorphic**: Scalars, vectors, matrices, and higher ranks
;; - **Interop-ready**: Interleaved `double[]` matches EJML's `ZMatrixRMaj`
;; - **Self-contained**: Provided by La Linea, zero dependencies on harmonica internals
