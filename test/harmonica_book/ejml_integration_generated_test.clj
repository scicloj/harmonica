(ns
 harmonica-book.ejml-integration-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [scicloj.harmonica.linalg.ejml :as ejml]
  [scicloj.harmonica.analysis.representations :as rep]
  [tech.v3.tensor :as tensor]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]])
 (:import [org.ejml.data ZMatrixRMaj]))


(def
 v3_l47
 (let
  [ct
   (cx/complex-tensor
    (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
    (tensor/->tensor [[0.5 1.0] [1.5 2.0]]))]
  {:complex-shape (cx/complex-shape ct),
   :raw-doubles (vec (cx/->double-array ct))}))


(deftest
 t5_l56
 (is
  ((fn [v] (= (:raw-doubles v) [1.0 0.5 2.0 1.0 3.0 1.5 4.0 2.0]))
   v3_l47)))


(def
 v7_l62
 (let
  [ct
   (cx/complex-tensor
    (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
    (tensor/->tensor [[0.5 1.0] [1.5 2.0]]))
   zm
   (ejml/ct->zmat ct)]
  {:identical? (identical? (cx/->double-array ct) (.data zm)),
   :rows (.numRows zm),
   :cols (.numCols zm)}))


(deftest
 t8_l70
 (is
  ((fn [v] (and (:identical? v) (= (:rows v) 2) (= (:cols v) 2)))
   v7_l62)))


(def
 v10_l81
 (let
  [ct
   (cx/complex-tensor
    (tensor/->tensor [[1.0 0.0] [0.0 1.0]])
    (tensor/->tensor [[0.0 0.0] [0.0 0.0]]))
   zm
   (ejml/ct->zmat ct)]
  (.set zm 0 1 99.0 77.0)
  (let [elem (ct 0)] [(cx/re (elem 1)) (cx/im (elem 1))])))


(deftest t11_l91 (is (= v10_l81 [99.0 77.0])))


(def
 v13_l95
 (let
  [ct
   (cx/complex-tensor
    (tensor/->tensor [[1.0 0.0] [0.0 1.0]])
    (tensor/->tensor [[0.0 0.0] [0.0 0.0]]))
   zm
   (ejml/ct->zmat ct)
   arr
   (cx/->double-array ct)]
  (aset arr 2 42.0)
  (aset arr 3 13.0)
  (let
   [c (org.ejml.data.Complex_F64.)]
   (.get zm 0 1 c)
   [(.real c) (.imaginary c)])))


(deftest t14_l108 (is (= v13_l95 [42.0 13.0])))


(def
 v16_l112
 (let
  [zm (ejml/zmat 2 2) _ (.set zm 0 0 5.0 6.0) ct (ejml/zmat->ct zm)]
  (identical? (.data zm) (cx/->double-array ct))))


(deftest t17_l117 (is (true? v16_l112)))


(def
 v19_l128
 (let
  [A
   (ejml/ct->zmat
    (cx/complex-tensor
     (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
     (tensor/->tensor [[0.5 1.0] [1.5 2.0]])))
   I
   (ejml/zmat-identity 2)
   AI
   (ejml/zmul A I)]
  (= (vec (.data AI)) (vec (.data A)))))


(deftest t20_l136 (is (true? v19_l128)))


(def
 v22_l143
 (let
  [A
   (ejml/ct->zmat
    (cx/complex-tensor
     (tensor/->tensor [[1.0 3.0] [5.0 7.0]])
     (tensor/->tensor [[2.0 4.0] [6.0 8.0]])))
   A2
   (ejml/zmul A A)
   result
   (ejml/zmat->ct A2)]
  [(cx/re ((result 0) 0)) (cx/im ((result 0) 0))]))


(deftest t23_l150 (is (= v22_l143 [-12.0 42.0])))


(def
 v25_l154
 (let
  [A
   (ejml/ct->zmat
    (cx/complex-tensor
     (tensor/->tensor [[1.0 3.0] [5.0 7.0]])
     (tensor/->tensor [[2.0 4.0] [6.0 8.0]])))]
  {:trace (ejml/ztrace A),
   :det
   (let [[re im] (ejml/zdet A)] [(Math/round re) (Math/round im)])}))


(deftest
 t27_l165
 (is
  ((fn [v] (and (= (:trace v) [8.0 10.0]) (= (:det v) [0 -16])))
   v25_l154)))


(def
 v29_l173
 (let
  [A
   (ejml/ct->zmat
    (cx/complex-tensor
     (tensor/->tensor [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]])
     (tensor/->tensor [[0.1 0.2 0.3] [0.4 0.5 0.6] [0.7 0.8 0.9]])))
   AdA
   (ejml/zmul (ejml/ztranspose-conj A) A)
   [tr-re _]
   (ejml/ztrace AdA)
   nf
   (ejml/znorm-f A)]
  (< (Math/abs (- tr-re (* nf nf))) 1.0E-10)))


(deftest t30_l181 (is (true? v29_l173)))


(def
 v32_l188
 (let
  [A
   (ejml/ct->zmat
    (cx/complex-tensor
     (tensor/->tensor [[1.0 2.0] [4.0 6.0]])
     (tensor/->tensor [[1.0 3.0] [5.0 7.0]])))
   Ad
   (ejml/ztranspose-conj A)
   ct
   (ejml/zmat->ct Ad)]
  {:re (vec (dtype/->double-array (cx/re ct))),
   :im (vec (dtype/->double-array (cx/im ct)))}))


(deftest
 t33_l196
 (is
  ((fn
    [v]
    (and
     (= (:re v) [1.0 4.0 2.0 6.0])
     (= (:im v) [-1.0 -5.0 -3.0 -7.0])))
   v32_l188)))


(def
 v35_l203
 (let
  [A
   (ejml/ct->zmat
    (cx/complex-tensor
     (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
     (tensor/->tensor [[0.5 1.0] [1.5 2.5]])))
   inv
   (ejml/zinvert A)
   product
   (ejml/zmul A inv)
   ct
   (ejml/zmat->ct product)
   re
   (cx/re ct)
   im
   (cx/im ct)]
  (and
   (<
    (dfn/reduce-max
     (dfn/abs
      (dfn/- (dtype/->double-array re) (double-array [1 0 0 1]))))
    1.0E-10)
   (< (dfn/reduce-max (dfn/abs (dtype/->double-array im))) 1.0E-10))))


(deftest t36_l215 (is (true? v35_l203)))


(def
 v38_l219
 (let
  [A
   (ejml/ct->zmat
    (cx/complex-tensor
     (tensor/->tensor [[1.0 2.0]])
     (tensor/->tensor [[3.0 4.0]])))
   B
   (ejml/ct->zmat
    (cx/complex-tensor
     (tensor/->tensor [[5.0 6.0]])
     (tensor/->tensor [[7.0 8.0]])))
   sum-ct
   (ejml/zmat->ct (ejml/zadd A B))
   diff-ct
   (ejml/zmat->ct (ejml/zsub A B))]
  {:sum-re (vec (dtype/->double-array (cx/re sum-ct))),
   :diff-re (vec (dtype/->double-array (cx/re diff-ct)))}))


(deftest
 t39_l230
 (is
  ((fn
    [v]
    (and (= (:sum-re v) [6.0 8.0]) (= (:diff-re v) [-4.0 -4.0])))
   v38_l219)))


(def
 v41_l246
 (let
  [I-zm
   (ejml/ct->zmat
    (cx/complex-tensor
     (tensor/->tensor [[1.0 0.0] [0.0 1.0]])
     (tensor/->tensor [[0.0 0.0] [0.0 0.0]])))
   P-zm
   (ejml/ct->zmat
    (cx/complex-tensor
     (tensor/->tensor [[0.0 1.0] [1.0 0.0]])
     (tensor/->tensor [[0.0 0.0] [0.0 0.0]])))
   acc
   (ejml/zmat 2 2)
   temp
   (ejml/zmat 2 2)]
  (ejml/scale-add-reuse! acc 3.0 0.0 I-zm temp)
  (ejml/scale-add-reuse! acc -1.0 0.0 P-zm temp)
  (vec (dtype/->double-array (cx/re (ejml/zmat->ct acc))))))


(deftest t43_l260 (is (= v41_l246 [3.0 -1.0 -1.0 3.0])))


(def
 v45_l268
 (let
  [G
   (hm/symmetric-group 4)
   ir
   (hm/irrep [3 1])
   d
   (:dimension ir)
   f-map
   (zipmap
    (hm/elements G)
    (map
     (fn* [p1__100225#] (Math/sin (double p1__100225#)))
     (range (hm/order G))))
   result-acm
   (rep/matrix-fourier-transform ir G f-map)
   result-ejml
   (let
    [acc (ejml/zmat d d) temp (ejml/zmat d d)]
    (doseq
     [sigma (hm/elements G)]
     (let
      [coeff
       (get f-map sigma 0.0)
       mat
       (rep/rep-matrix ir sigma)
       zm
       (ejml/zmat d d)]
      (dotimes
       [i d]
       (dotimes [j d] (.set zm i j (double (fm/entry mat i j)) 0.0)))
      (ejml/scale-add-reuse! acc coeff 0.0 zm temp)))
    acc)
   max-diff
   (reduce
    max
    (for
     [i (range d) j (range d)]
     (let
      [c (org.ejml.data.Complex_F64.)]
      (.get result-ejml i j c)
      (Math/abs (- (.real c) (double (fm/entry result-acm i j)))))))]
  max-diff))


(deftest t46_l298 (is ((fn [v] (< v 1.0E-10)) v45_l268)))


(def
 v48_l307
 (defn
  benchmark-accumulation
  "Time n iterations of matrix Fourier accumulation. Returns µs/iter."
  [accumulate-fn n]
  (accumulate-fn)
  (let
   [t0 (System/nanoTime)]
   (dotimes [_ n] (accumulate-fn))
   (/ (- (System/nanoTime) t0) (* n 1000.0)))))


(def
 v49_l315
 (let
  [G
   (hm/symmetric-group 5)
   ir
   (hm/irrep [3 1 1])
   d
   (:dimension ir)
   elts
   (hm/elements G)
   f-map
   (zipmap
    elts
    (map
     (fn* [p1__100226#] (Math/sin (double p1__100226#)))
     (range (hm/order G))))
   precomp-z
   (into
    {}
    (map
     (fn
      [sigma]
      (let
       [mat (rep/rep-matrix ir sigma) zm (ejml/zmat d d)]
       (dotimes
        [i d]
        (dotimes [j d] (.set zm i j (double (fm/entry mat i j)) 0.0)))
       [sigma zm]))
     elts))
   precomp-acm
   (into {} (map (fn [sigma] [sigma (rep/rep-matrix ir sigma)]) elts))
   ejml-fn
   (fn
    []
    (let
     [acc (ejml/zmat d d) temp (ejml/zmat d d)]
     (doseq
      [[sigma zm] precomp-z]
      (let
       [coeff (double (get f-map sigma 0.0))]
       (when-not
        (zero? coeff)
        (ejml/scale-add-reuse! acc coeff 0.0 zm temp))))
     acc))
   acm-fn
   (fn
    []
    (reduce
     (fn
      [acc [sigma mat]]
      (let
       [coeff (double (get f-map sigma 0.0))]
       (if (zero? coeff) acc (.add acc (.scalarMultiply mat coeff)))))
     (fm/rows->mat (vec (repeat d (vec (repeat d 0.0)))))
     precomp-acm))
   n
   2000
   us-ejml
   (benchmark-accumulation ejml-fn n)
   us-acm
   (benchmark-accumulation acm-fn n)]
  (kind/table
   {:column-names ["Backend" "d" "|G|" "µs / iter" "Speedup"],
    :row-vectors
    [["EJML (ZMatrixRMaj)"
      d
      (hm/order G)
      (format "%.1f" us-ejml)
      (format "%.1fx" (/ us-acm us-ejml))]
     ["ACM (RealMatrix)"
      d
      (hm/order G)
      (format "%.1f" us-acm)
      "1.0x"]]})))
