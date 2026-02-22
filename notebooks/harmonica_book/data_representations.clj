;; # Data Representations
;;
;; This chapter looks under the hood at how harmonica represents
;; mathematical objects as Clojure data. If you want to contribute,
;; port the library, or simply understand what your REPL is printing,
;; this is the place to start.
;;
;; **Design principle:** groups are first-class values (defrecords
;; implementing protocols), and group elements are plain Clojure data —
;; integers, vectors, tagged pairs. There are no wrapper types around
;; elements; the group object carries the algebra.

(ns harmonica-book.data-representations
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
   [scicloj.harmonica.protocols :as p]
   [scicloj.harmonica.combinatorics.permutation :as perm]
   [scicloj.harmonica.combinatorics.partition :as part]
   [scicloj.harmonica.combinatorics.young-tableaux :as yt]
   [scicloj.harmonica.combinatorics.murnaghan-nakayama :as mn]
   [tech.v3.tensor :as tensor]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Groups as protocol values
;;
;; The algebraic structure is defined by four Clojure protocols:
;;
;; | Protocol | Methods | Purpose |
;; |:---------|:--------|:--------|
;; | `Group` | `op`, `inv`, `id` | Binary operation, inverse, identity |
;; | `FiniteGroup` | `elements`, `order` | Enumerate elements, count them |
;; | `GroupStructure` | `conjugacy-classes` | Structural decomposition |
;; | `GroupType` | `group-type` | Keyword tag for multimethod dispatch |
;;
;; Each group family is a `defrecord` implementing these protocols.
;; The same functions — `hm/op`, `hm/inv`, `hm/id`, `hm/elements`,
;; `hm/order` — work on any group.

;; ### Cyclic group $\mathbb{Z}/n\mathbb{Z}$
;;
;; Elements are integers `0, 1, ..., n-1`. The operation is addition mod $n$.

(def Z6 (hm/cyclic-group 6))

(type Z6)

(kind/test-last [(fn [t] (= (.getSimpleName ^Class t) "CyclicGroup"))])

;; The group stores only its order. Elements are generated on the fly.

(select-keys Z6 [:n])

(kind/test-last [= {:n 6}])

(hm/elements Z6)

(kind/test-last [= (range 6)])

;; The operation, inverse, and identity are exactly what you'd expect:

[(hm/op Z6 2 3)
 (hm/inv Z6 2)
 (hm/id Z6)]

(kind/test-last [= [5 4 0]])

;; Because cyclic groups are abelian, each element is its own
;; conjugacy class.

(count (hm/conjugacy-classes Z6))

(kind/test-last [= 6])

;; ### Symmetric group $S_n$
;;
;; Elements are **0-indexed one-line notation** vectors:
;; `[σ(0) σ(1) ... σ(n-1)]`. The vector `[2 0 1]` means
;; $0 \mapsto 2$, $1 \mapsto 0$, $2 \mapsto 1$.
;;
;; Composition is right-to-left: `(op G σ τ)` $= \sigma \circ \tau$
;; where $(\sigma \circ \tau)(i) = \sigma(\tau(i))$.

(def S4 (hm/symmetric-group 4))

(type S4)

(kind/test-last [(fn [t] (= (.getSimpleName ^Class t) "SymmetricGroup"))])

(hm/order S4)

(kind/test-last [= 24])

;; Composition of two permutations:

(hm/op S4 [1 0 3 2] [2 3 0 1])

(kind/test-last [= [3 2 1 0]])

;; The identity is `[0 1 2 3]` and inverses swap the mapping:

[(hm/id S4)
 (hm/inv S4 [1 2 3 0])]

(kind/test-last [= [[0 1 2 3] [3 0 1 2]]])

;; Conjugacy classes of $S_n$ are indexed by **cycle type** —
;; the partition giving the lengths of disjoint cycles.
;; $S_4$ has 5 classes, one for each partition of 4:

(mapv :cycle-type (hm/conjugacy-classes S4))

(kind/test-last [= [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]])

;; ### Dihedral group $D_n$
;;
;; The symmetries of a regular $n$-gon. Order $2n$. Elements are
;; tagged pairs:
;;
;; - `[:r k]` — rotation by $2\pi k / n$
;; - `[:s k]` — reflection (rotation $r^k$ followed by base reflection $s$)
;;
;; The algebra follows from the presentation
;; $r^n = s^2 = e$, $s r s = r^{-1}$.

(def D4 (hm/dihedral-group 4))

(hm/order D4)

(kind/test-last [= 8])

;; Rotation then reflection:

(hm/op D4 [:r 1] [:s 0])

(kind/test-last [= [:s 1]])

;; Two reflections give a rotation:

(hm/op D4 [:s 2] [:s 0])

(kind/test-last [= [:r 2]])

;; Reflections are involutions ($s^2 = e$):

(hm/inv D4 [:s 3])

(kind/test-last [= [:s 3]])

;; ### Product group $G_1 \times G_2$
;;
;; Elements are pairs `[g h]`; all operations are componentwise.

(def Z2xZ3 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3)))

(hm/order Z2xZ3)

(kind/test-last [= 6])

(hm/op Z2xZ3 [1 2] [1 1])

(kind/test-last [= [0 0]])

;; ### Uniform interface
;;
;; The same protocol functions work on every group type.
;; Here we verify the group axioms generically:

(defn group-axioms-hold? [G]
  (let [e (hm/id G)
        elts (take 20 (hm/elements G))]
    (and
     ;; Identity: e * g = g
     (every? #(= (hm/op G e %) %) elts)
     ;; Inverse: g * g^{-1} = e
     (every? #(= (hm/op G % (hm/inv G %)) e) elts))))

(mapv group-axioms-hold? [Z6 S4 D4 Z2xZ3])

(kind/test-last [= [true true true true]])

;; ## Permutations
;;
;; Permutations are plain vectors. The representation is 0-indexed
;; one-line notation: `sigma[i]` is the image of `i`.

;; **Composition** is a single `mapv` — apply $\tau$ first, then $\sigma$:

(let [sigma [2 0 1]
      tau   [1 2 0]]
  (mapv sigma tau))

(kind/test-last [= [0 1 2]])

;; **Inverse** uses a mutable `int-array` for O(n) construction —
;; if $\sigma(i) = j$ then $\sigma^{-1}(j) = i$:

(perm/inverse [2 0 1])

(kind/test-last [= [1 2 0]])

;; **Cycle decomposition** walks the permutation using a `boolean-array`
;; to track visited positions. Fixed points (1-cycles) are omitted:

(perm/cycles [1 3 0 2])

(kind/test-last [(fn [cs] (= (set (map set cs)) #{#{0 1 3 2}}))])

;; **Cycle type** is the partition of cycle lengths (including
;; fixed points), sorted descending:

(perm/cycle-type [1 2 0 3 4])

(kind/test-last [= [3 1 1]])

;; **Sign** counts the total number of transpositions in the
;; cycle decomposition:

[(perm/sign [0 1 2 3])
 (perm/sign [1 0 2 3])
 (perm/sign [1 2 0 3])]

(kind/test-last [= [1 -1 1]])

;; ## Partitions and Young tableaux
;;
;; A **partition** of $n$ is a descending vector of positive
;; integers summing to $n$. Partitions index the conjugacy classes
;; and the irreducible representations of $S_n$.

(hm/partitions 5)

(kind/test-last [(fn [ps] (and (= 7 (count ps))
                               (= [5] (first ps))
                               (= [1 1 1 1 1] (last ps))))])

;; The **conjugate** (transpose) swaps rows and columns in the
;; Young diagram:

(part/conjugate [4 2 1])

(kind/test-last [= [3 2 1 1]])

;; A **Standard Young Tableau** (SYT) fills the diagram with
;; $1, 2, \ldots, n$ so that entries increase along rows and down
;; columns. SYTs index the basis vectors of irreducible
;; representations. They are enumerated by depth-first backtracking:

(hm/standard-young-tableaux [3 2])

(kind/test-last [(fn [syts] (= 5 (count syts)))])

;; The number of SYTs equals the dimension of the corresponding
;; irrep, computed by the **hook-length formula**:
;;
;; $$d_\lambda = \frac{n!}{\prod_{(i,j) \in \lambda} h(i,j)}$$

(hm/hook-length-dimension [3 2])

(kind/test-last [= 5])

;; Both methods agree:

(= (count (hm/standard-young-tableaux [4 3 1]))
   (hm/hook-length-dimension [4 3 1]))

(kind/test-last [true?])

;; ## Character tables
;;
;; A character table is a Clojure map with five keys:

(def ct-s4 (hm/character-table S4))

(sort (keys ct-s4))

(kind/test-last [= [:class-sizes :classes :group :irrep-labels :table]])

;; For $S_4$, irrep labels are the 5 partitions of 4:

(:irrep-labels ct-s4)

(kind/test-last [= [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]])

;; Classes are also partitions (cycle types), with the identity
;; class `[1 1 1 1]` first:

(:classes ct-s4)

(kind/test-last [(fn [cs] (= [1 1 1 1] (first cs)))])

;; The `:table` is a **ComplexTensor** matrix — a `[5 × 5]` complex
;; matrix whose real part contains the character values (all integers
;; for $S_n$):

(cx/complex-shape (:table ct-s4))

(kind/test-last [= [5 5]])

(:table ct-s4)

;; Character table computation is a **multimethod** dispatching on
;; `group-type`:
;;
;; | Group type | Algorithm |
;; |:-----------|:----------|
;; | `:cyclic` | DFT matrix: $\chi_j(k) = \omega^{jk}$ where $\omega = e^{2\pi i/n}$ |
;; | `:symmetric` | Murnaghan-Nakayama rule (integer-valued) |
;; | `:dihedral` | Closed-form trig expressions |
;; | `:product` | Kronecker product of factor character tables |

;; ## ComplexTensor
;;
;; ComplexTensor is the library's complex number type, backed by
;; dtype-next tensors. The core idea: a complex tensor of shape
;; $[d_1, d_2, \ldots]$ is stored as a **real** tensor of shape
;; $[d_1, d_2, \ldots, 2]$ with interleaved real/imaginary pairs.

;; A complex vector of length 3:

(def v (cx/complex-tensor [1.0 2.0 3.0] [0.5 -0.5 1.0]))

v

;; The underlying storage is a `[3, 2]` real tensor:

(dtype/shape (cx/->tensor v))

(kind/test-last [= [3 2]])

;; `re` and `im` return **zero-copy tensor views** — slicing the
;; last axis at index 0 or 1:

[(vec (cx/re v)) (vec (cx/im v))]

(kind/test-last [= [[1.0 2.0 3.0] [0.5 -0.5 1.0]]])

;; This interleaved layout is deliberate:
;;
;; - **Cache-friendly**: real and imaginary parts of the same element
;;   sit in adjacent memory
;; - **EJML compatible**: EJML's `ZMatrixRMaj` uses the same
;;   interleaved `double[]` layout, enabling zero-copy sharing
;; - **dtype-next integration**: protocol extensions make `dfn/+` and
;;   `dfn/-` work directly on ComplexTensors (operating on the raw
;;   interleaved buffer)
;;
;; One subtlety: `dfn/*` on two ComplexTensors does **element-wise real**
;; multiply (on the flat interleaved buffer), not complex multiply.
;; Use `cx/cmul` for complex multiplication:

(let [a (cx/complex 3.0 4.0)
      b (cx/complex 1.0 2.0)]
  {:cmul-re (cx/re (cx/cmul a b))
   :cmul-im (cx/im (cx/cmul a b))})

(kind/test-last [(fn [m] (and (= -5.0 (:cmul-re m))
                              (= 10.0 (:cmul-im m))))])

;; ComplexTensors implement `IFn`, `Indexed`, `Seqable`, and `Counted`,
;; so they work naturally with Clojure's indexing and sequence operations:

(let [ct-row ((:table ct-s4) 0)]
  {:first-value (cx/re (((:table ct-s4) 0) 0))
   :count (count ((:table ct-s4) 0))})

(kind/test-last [(fn [m] (and (= 1.0 (:first-value m))
                              (= 5 (:count m))))])

;; ## Representations
;;
;; An irreducible representation of $S_n$ is built from
;; [Young's orthogonal form](https://en.wikipedia.org/wiki/Young%27s_orthogonal_form).
;; The result is a Clojure map:

(def ir-31 (hm/irrep [3 1]))

(sort (keys ir-31))

(kind/test-last [= [:dimension :generators :lambda :syts]])

;; The four fields:
;;
;; | Key | Type | Content |
;; |:----|:-----|:--------|
;; | `:lambda` | vector | The partition labeling this irrep |
;; | `:dimension` | long | Number of SYTs = dimension of the representation |
;; | `:syts` | vector of SYTs | The basis (each SYT is a vector of row vectors) |
;; | `:generators` | vector of `RealMatrix` | One matrix per adjacent transposition $s_i$ |

(:lambda ir-31)

(kind/test-last [= [3 1]])

(:dimension ir-31)

(kind/test-last [= 3])

;; The basis vectors are the 3 standard Young tableaux of shape `[3, 1]`:

(:syts ir-31)

(kind/test-last [(fn [syts] (= 3 (count syts)))])

;; The generators are $(n-1) = 3$ real orthogonal matrices,
;; one for each adjacent transposition $s_1 = (1\;2)$,
;; $s_2 = (2\;3)$, $s_3 = (3\;4)$:

(count (:generators ir-31))

(kind/test-last [= 3])

;; To get $\rho(\sigma)$ for any permutation $\sigma$, the library
;; decomposes $\sigma$ into adjacent transpositions and multiplies
;; the corresponding generators:

(hm/rep-matrix ir-31 [1 0 2 3])

;; **Composite representations** (tensor product, direct sum) use a
;; different mechanism: a `:matrix-fn` closure that computes $\rho(\sigma)$
;; on demand:

(let [rep1 (hm/irrep [3 1])
      rep2 (hm/irrep [2 2])
      tp (hm/tensor-product rep1 rep2)]
  {:has-matrix-fn (some? (:matrix-fn tp))
   :dimension (:dimension tp)})

(kind/test-last [(fn [m] (and (:has-matrix-fn m) (= 6 (:dimension m))))])

;; This two-track dispatch lets generator-based irreps and
;; closure-based composites share the same `rep-matrix` function.

;; ## Fourier transform pipeline
;;
;; The Fourier transform on a finite abelian group works on
;; split real/imaginary arrays using dtype-next operations.
;; The pipeline is:
;;
;; 1. Extract `re` and `im` views from the character table and signal
;; 2. Compute $\hat{f}(k) = \sum_g f(g) \cdot \overline{\chi_k(g)}$
;;    as `(ac + bd)` and `(bc - ad)` using `dfn/*` and `dfn/sum`
;; 3. Wrap the result pair back into a ComplexTensor
;;
;; No intermediate complex objects are created — everything stays
;; in split-real form until the final assembly.

(let [G (hm/cyclic-group 8)
      ct (hm/character-table G)
      signal (cx/complex-tensor-real [1 0 1 0 1 0 1 0])]
  (hm/fourier-transform ct signal))

;; The inverse transform reverses the process, with a $1/|G|$ scaling factor.
;; Convolution composes: forward transform both inputs, pointwise complex multiply,
;; inverse transform. The entire pipeline involves three passes through the data,
;; each using `dtype/make-reader` for lazy evaluation and `dfn` for vectorized arithmetic.

;; ## What comes next
;;
;; The next chapter, [Algorithms](algorithms.html), walks through
;; the key algorithms — Murnaghan-Nakayama, Young's orthogonal form,
;; bubble-sort decomposition — with step-by-step worked examples.
