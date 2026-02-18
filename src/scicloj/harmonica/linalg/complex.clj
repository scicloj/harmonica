(ns scicloj.harmonica.linalg.complex
  "Complex tensors backed by dtype-next.

   A ComplexTensor wraps a real tensor whose last dimension is 2
   (interleaved re/im pairs). The `re` and `im` functions always
   slice the last axis, returning zero-copy tensor views.

   Supported ranks:
     [2]       scalar complex number
     [n 2]     complex vector of length n
     [r c 2]   complex r×c matrix
     [... 2]   arbitrary rank

   This namespace has zero dependencies on the rest of harmonica
   and is designed to be extractable into a separate library."
  (:require [tech.v3.tensor :as tensor]
            [tech.v3.datatype :as dtype]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.datatype.protocols :as dtype-proto]))

;; ---------------------------------------------------------------------------
;; Protocol
;; ---------------------------------------------------------------------------

(defprotocol PComplex
  (re [x] "Real part(s). Returns a tensor view (last dim removed) or a double for scalars.")
  (im [x] "Imaginary part(s). Returns a tensor view (last dim removed) or a double for scalars."))

;; ---------------------------------------------------------------------------
;; Internal helpers
;; ---------------------------------------------------------------------------

(defn- select-part
  "Select real (idx=0) or imaginary (idx=1) part from a tensor whose last dim is 2."
  [tensor idx]
  (let [ndims (count (dtype/shape tensor))]
    (apply tensor/select tensor
           (concat (repeat (dec ndims) :all) [idx]))))

(defn- format-complex
  "Format a single complex number as a string."
  [re im]
  (cond
    (zero? im) (str re)
    (zero? re) (if (== im 1.0) "i"
                   (if (== im -1.0) "-i"
                       (str im "i")))
    (neg? im) (str re im "i")
    :else (str re "+" im "i")))

(def ^:private ^:const max-print-elems
  "Maximum elements to show per dimension before truncating."
  20)

(defn- format-row
  "Format a [n 2] tensor row as a bracketed string of complex numbers."
  [tensor]
  (let [n (first (dtype/shape tensor))
        show-n (min n max-print-elems)]
    (str "[" (clojure.string/join ", "
                                  (map (fn [i]
                                         (let [row (tensor i)]
                                           (format-complex (double (row 0)) (double (row 1)))))
                                       (range show-n)))
         (when (> n max-print-elems) (str ", ... (" n " total)"))
         "]")))

(defn- rprint-complex
  "Recursively print a complex tensor with nested brackets.
   tensor has shape [d1 d2 ... dn 2]. Slices leading dims until
   we reach [n 2] (a vector) or [2] (a scalar)."
  [^StringBuilder sb tensor prefix]
  (let [s (dtype/shape tensor)
        ndims (count s)]
    (cond
      ;; Scalar [2]
      (= ndims 1)
      (.append sb (format-complex (double (tensor 0)) (double (tensor 1))))

      ;; Vector [n 2] — leaf row
      (= ndims 2)
      (.append sb (format-row tensor))

      ;; Matrix or higher — recurse
      :else
      (let [n (first s)
            show-n (min n max-print-elems)
            inner-prefix (str prefix " ")]
        (.append sb "[")
        (dotimes [i show-n]
          (when (> i 0)
            (.append sb "\n")
            (.append sb inner-prefix))
          (rprint-complex sb (tensor i) inner-prefix))
        (when (> n max-print-elems)
          (.append sb "\n")
          (.append sb inner-prefix)
          (.append sb (str "... (" n " rows total)")))
        (.append sb "]")))))

(defn- interleave-into!
  "Write re-data and im-data into an interleaved [... 2] tensor via strided views."
  [tensor re-data im-data]
  (let [ndims (count (dtype/shape tensor))
        re-view (apply tensor/select tensor (concat (repeat (dec ndims) :all) [0]))
        im-view (apply tensor/select tensor (concat (repeat (dec ndims) :all) [1]))]
    (dtype/copy! (dtype/->reader (tensor/ensure-tensor re-data))
                 (dtype/->reader re-view))
    (dtype/copy! (dtype/->reader (tensor/ensure-tensor im-data))
                 (dtype/->reader im-view)))
  tensor)

;; ---------------------------------------------------------------------------
;; ComplexTensor deftype
;; ---------------------------------------------------------------------------

(declare ->ComplexTensor)

(deftype ComplexTensor [tensor]
  PComplex
  (re [_]
    (if (= 1 (count (dtype/shape tensor)))
      (double (tensor 0))
      (select-part tensor 0)))
  (im [_]
    (if (= 1 (count (dtype/shape tensor)))
      (double (tensor 1))
      (select-part tensor 1)))

  clojure.lang.Counted
  (count [_]
    (let [s (dtype/shape tensor)]
      (if (<= (count s) 1)
        0
        (long (first s)))))

  clojure.lang.Indexed
  (nth [_ i]
    (->ComplexTensor (tensor i)))
  (nth [this i not-found]
    (if (and (>= i 0) (< i (.count this)))
      (.nth this i)
      not-found))

  clojure.lang.IFn
  (invoke [this i]
    (.nth this (int i)))
  (applyTo [this args]
    (let [n (clojure.lang.RT/boundedLength args 1)]
      (case n
        1 (.invoke this (first args))
        (throw (clojure.lang.ArityException. n (str (class this)))))))

  clojure.lang.Seqable
  (seq [this]
    (when (pos? (.count this))
      (map #(.nth this %) (range (.count this)))))

  Object
  (toString [_]
    (let [sb (StringBuilder.)
          complex-shape (vec (butlast (dtype/shape tensor)))]
      (.append sb "#ComplexTensor ")
      (.append sb (str complex-shape))
      (.append sb "\n")
      (rprint-complex sb tensor "")
      (.toString sb))))

(defmethod print-method ComplexTensor [^ComplexTensor ct ^java.io.Writer w]
  (.write w (.toString ct)))

;; ---------------------------------------------------------------------------
;; Constructors
;; ---------------------------------------------------------------------------

(defn complex-tensor
  "Create a ComplexTensor.

   Arities:
     (complex-tensor tensor)       — wrap an existing tensor with last dim = 2
     (complex-tensor re-data im-data) — from separate real and imaginary parts

   re-data and im-data can be: double arrays, seqs, dtype readers, or tensors.
   They must have the same shape.

   When wrapping a 1-arg input whose last dimension is not 2 (e.g., a flat
   reader from dfn operations on ComplexTensors), it is reshaped to [n/2 2]."
  ([tensor-or-re]
   (let [t (if (instance? ComplexTensor tensor-or-re)
             (.-tensor ^ComplexTensor tensor-or-re)
             (tensor/ensure-tensor tensor-or-re))
         shape (vec (dtype/shape t))]
     (if (= 2 (last shape))
       (->ComplexTensor t)
       ;; Flat input (e.g., from dfn ops on ComplexTensors) — reshape to [n/2, 2]
       (let [n (long (reduce * shape))]
         (when-not (even? n)
           (throw (ex-info (str "Cannot interpret odd-length data as complex: " n)
                           {:shape shape :n n})))
         (->ComplexTensor (tensor/reshape t [(/ n 2) 2]))))))
  ([re-data im-data]
   (let [re-t (tensor/ensure-tensor re-data)
         im-t (tensor/ensure-tensor im-data)
         re-shape (vec (dtype/shape re-t))
         im-shape (vec (dtype/shape im-t))]
     (when-not (= re-shape im-shape)
       (throw (ex-info (str "Shape mismatch: re=" re-shape " im=" im-shape)
                       {:re-shape re-shape :im-shape im-shape})))
     (let [full-shape (conj re-shape 2)
           n-elements (reduce * full-shape)
           backing (double-array n-elements)
           t (tensor/reshape (tensor/ensure-tensor backing) full-shape)]
       (interleave-into! t re-data im-data)
       (->ComplexTensor t)))))

(defn complex-tensor-real
  "Create a ComplexTensor from real data only (imaginary parts = 0)."
  [re-data]
  (let [re-t (tensor/ensure-tensor re-data)
        re-shape (vec (dtype/shape re-t))
        full-shape (conj re-shape 2)
        n-elements (reduce * full-shape)
        backing (double-array n-elements)
        t (tensor/reshape (tensor/ensure-tensor backing) full-shape)]
    (dtype/copy! (dtype/->reader re-t)
                 (dtype/->reader (select-part t 0)))
    (->ComplexTensor t)))

;; ---------------------------------------------------------------------------
;; Accessors
;; ---------------------------------------------------------------------------

(defn ->tensor
  "Access the underlying [... 2] tensor."
  [^ComplexTensor ct]
  (.-tensor ct))

(defn ->double-array
  "Access the underlying interleaved double[].
   Returns the identical array object (zero-copy)."
  [^ComplexTensor ct]
  (dtype/->double-array (.buffer (.-tensor ct))))

(defn complex-shape
  "The complex shape (underlying shape without trailing 2)."
  [^ComplexTensor ct]
  (vec (butlast (dtype/shape (.-tensor ct)))))

(defn scalar?
  "True if this ComplexTensor represents a scalar complex number."
  [^ComplexTensor ct]
  (= 1 (count (dtype/shape (.-tensor ct)))))

;; ---------------------------------------------------------------------------
;; Complex arithmetic
;; ---------------------------------------------------------------------------

(defn complex
  "Create a scalar ComplexTensor from real and imaginary parts."
  [re im]
  (->ComplexTensor (tensor/->tensor [(double re) (double im)])))

(defn cmul
  "Pointwise complex multiply: (a+bi)(c+di) = (ac-bd) + (ad+bc)i"
  [^ComplexTensor a ^ComplexTensor b]
  (let [ar (re a) ai (im a)
        br (re b) bi (im b)]
    (if (and (scalar? a) (scalar? b))
      (let [ar (double ar) ai (double ai)
            br (double br) bi (double bi)]
        (complex (- (* ar br) (* ai bi))
                 (+ (* ar bi) (* ai br))))
      (complex-tensor (dfn/- (dfn/* ar br) (dfn/* ai bi))
                      (dfn/+ (dfn/* ar bi) (dfn/* ai br))))))

(defn cconj
  "Complex conjugate: negate imaginary part."
  [^ComplexTensor ct]
  (complex-tensor (re ct) (dfn/* -1.0 (im ct))))

(defn cscale
  "Scale by a real scalar."
  [^ComplexTensor ct alpha]
  (let [a (double alpha)]
    (complex-tensor (dfn/* a (re ct)) (dfn/* a (im ct)))))

(defn cabs
  "Element-wise complex magnitude: sqrt(re² + im²).
   Returns a real tensor (or double for scalar)."
  [^ComplexTensor ct]
  (let [r (re ct) i (im ct)]
    (dfn/sqrt (dfn/+ (dfn/* r r) (dfn/* i i)))))

(defn cdot
  "Complex dot product: Σ a_i * b_i.
   Returns a [re im] pair."
  [^ComplexTensor a ^ComplexTensor b]
  (let [ar (re a) ai (im a)
        br (re b) bi (im b)]
    [(- (dfn/sum (dfn/* ar br)) (dfn/sum (dfn/* ai bi)))
     (+ (dfn/sum (dfn/* ar bi)) (dfn/sum (dfn/* ai br)))]))

(defn cdot-conj
  "Hermitian inner product: Σ a_i * conj(b_i).
   Returns a [re im] pair."
  [^ComplexTensor a ^ComplexTensor b]
  (let [ar (re a) ai (im a)
        br (re b) bi (im b)]
    [(+ (dfn/sum (dfn/* ar br)) (dfn/sum (dfn/* ai bi)))
     (- (dfn/sum (dfn/* ai br)) (dfn/sum (dfn/* ar bi)))]))

(defn cadd
  "Pointwise complex addition."
  [^ComplexTensor a ^ComplexTensor b]
  (if (and (scalar? a) (scalar? b))
    (complex (+ (double (re a)) (double (re b)))
             (+ (double (im a)) (double (im b))))
    (complex-tensor (dfn/+ (re a) (re b))
                    (dfn/+ (im a) (im b)))))

(defn csub
  "Pointwise complex subtraction."
  [^ComplexTensor a ^ComplexTensor b]
  (if (and (scalar? a) (scalar? b))
    (complex (- (double (re a)) (double (re b)))
             (- (double (im a)) (double (im b))))
    (complex-tensor (dfn/- (re a) (re b))
                    (dfn/- (im a) (im b)))))

(defn csum
  "Complex-aware summation. Returns a scalar ComplexTensor."
  [^ComplexTensor ct]
  (complex (dfn/sum (re ct)) (dfn/sum (im ct))))

;; ---------------------------------------------------------------------------
;; dtype-next protocol extensions
;; ---------------------------------------------------------------------------
;; Exposing the interleaved tensor as a dtype-next reader allows dfn/+, dfn/-
;; to work directly on ComplexTensors. Note that dfn/* on two ComplexTensors
;; does element-wise (not complex) multiply — use cmul for that.

(extend-type ComplexTensor
  dtype-proto/PElemwiseDatatype
  (elemwise-datatype [ct] :float64)

  dtype-proto/PECount
  (ecount [ct] (dtype/ecount (->tensor ct)))

  dtype-proto/PShape
  (shape [ct] (dtype/shape (->tensor ct)))

  dtype-proto/PToReader
  (convertible-to-reader? [ct] true)
  (->reader [ct] (dtype/->reader (->tensor ct))))
