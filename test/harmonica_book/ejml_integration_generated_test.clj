(ns
 harmonica-book.ejml-integration-generated-test
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
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]])
 (:import [org.ejml.data ZMatrixRMaj]))


(def
 v3_l45
 (let
  [ct
   (t/complex-tensor
    (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
    (tensor/->tensor [[0.5 1.0] [1.5 2.0]]))]
  {:complex-shape (t/complex-shape ct),
   :raw-doubles (vec (t/->double-array ct))}))


(deftest
 t5_l54
 (is
  ((fn [v] (= (:raw-doubles v) [1.0 0.5 2.0 1.0 3.0 1.5 4.0 2.0]))
   v3_l45)))


(def
 v7_l61
 (let
  [ct
   (t/complex-tensor
    (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
    (tensor/->tensor [[0.5 1.0] [1.5 2.0]]))
   zm
   (t/complex-tensor->zmat ct)]
  {:rows (.numRows zm),
   :cols (.numCols zm),
   :entry-00
   (let
    [c (org.ejml.data.Complex_F64.)]
    (.get zm 0 0 c)
    [(.real c) (.imaginary c)])}))


(deftest
 t8_l71
 (is
  ((fn
    [v]
    (and (= (:rows v) 2) (= (:cols v) 2) (= (:entry-00 v) [1.0 0.5])))
   v7_l61)))


(def
 v10_l77
 (let
  [zm (ZMatrixRMaj. 2 2)]
  (.set zm 0 0 5.0 6.0)
  (let
   [ct (t/zmat->complex-tensor zm)]
   {:identical? (identical? (.data zm) (t/->double-array ct)),
    :re (el/re ((ct 0) 0)),
    :im (el/im ((ct 0) 0))})))


(deftest
 t11_l84
 (is
  ((fn [v] (and (:identical? v) (= (:re v) 5.0) (= (:im v) 6.0)))
   v10_l77)))


(def
 v13_l90
 (let
  [zm (ZMatrixRMaj. 2 2)]
  (.set zm 0 1 99.0 77.0)
  (let
   [ct (t/zmat->complex-tensor zm)]
   [(el/re ((ct 0) 1)) (el/im ((ct 0) 1))])))


(deftest t14_l95 (is (= v13_l90 [99.0 77.0])))


(def
 v16_l106
 (def
  A
  (t/complex-tensor
   (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
   (tensor/->tensor [[0.5 1.0] [1.5 2.0]]))))


(def
 v18_l112
 (let
  [I (t/complex-tensor-real (t/eye 2)) AI (la/mmul A I)]
  (la/close? A AI)))


(deftest t19_l116 (is (true? v18_l112)))


(def
 v21_l123
 (let
  [B
   (t/complex-tensor
    (tensor/->tensor [[1.0 3.0] [5.0 7.0]])
    (tensor/->tensor [[2.0 4.0] [6.0 8.0]]))
   B2
   (la/mmul B B)
   entry
   (el/* ((B2 0) 0) (t/complex 1 0))]
  [(el/re ((B2 0) 0)) (el/im ((B2 0) 0))]))


(deftest t22_l130 (is (= v21_l123 [-12.0 42.0])))


(def
 v24_l134
 (let
  [B
   (t/complex-tensor
    (tensor/->tensor [[1.0 3.0] [5.0 7.0]])
    (tensor/->tensor [[2.0 4.0] [6.0 8.0]]))
   tr
   (la/trace B)
   d
   (la/det B)]
  {:trace-re (el/re tr),
   :trace-im (el/im tr),
   :det-re (Math/round (double (el/re d))),
   :det-im (Math/round (double (el/im d)))}))


(deftest
 t26_l147
 (is
  ((fn
    [v]
    (and
     (= (:trace-re v) 8.0)
     (= (:trace-im v) 10.0)
     (= (:det-re v) 0)
     (= (:det-im v) -16)))
   v24_l134)))


(def
 v28_l155
 (let
  [C
   (t/complex-tensor
    (tensor/->tensor [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]])
    (tensor/->tensor [[0.1 0.2 0.3] [0.4 0.5 0.6] [0.7 0.8 0.9]]))
   AdA
   (la/mmul (la/transpose C) C)
   tr-re
   (double (el/re (la/trace AdA)))
   nf
   (la/norm C)]
  (< (Math/abs (- tr-re (* nf nf))) 1.0E-10)))


(deftest t29_l163 (is (true? v28_l155)))


(def
 v31_l169
 (let
  [B
   (t/complex-tensor
    (tensor/->tensor [[1.0 2.0] [4.0 6.0]])
    (tensor/->tensor [[1.0 3.0] [5.0 7.0]]))
   Bd
   (la/transpose B)]
  {:re (vec (dtype/->double-array (el/re Bd))),
   :im (vec (dtype/->double-array (el/im Bd)))}))


(deftest
 t33_l178
 (is
  ((fn
    [v]
    (and
     (= (:re v) [1.0 4.0 2.0 6.0])
     (= (:im v) [-1.0 -5.0 -3.0 -7.0])))
   v31_l169)))


(def
 v35_l185
 (let
  [inv
   (la/invert A)
   product
   (la/mmul A inv)
   re-part
   (el/re product)
   im-part
   (el/im product)]
  (and
   (< (el/reduce-max (el/abs (el/- re-part (t/eye 2)))) 1.0E-10)
   (< (el/reduce-max (el/abs im-part)) 1.0E-10))))


(deftest t36_l192 (is (true? v35_l185)))


(def
 v38_l196
 (let
  [X
   (t/complex-tensor [1.0 2.0] [3.0 4.0])
   Y
   (t/complex-tensor [5.0 6.0] [7.0 8.0])
   s
   (el/+ X Y)
   d
   (el/- X Y)]
  {:sum-re (vec (dtype/->double-array (el/re s))),
   :diff-re (vec (dtype/->double-array (el/re d)))}))


(deftest
 t39_l203
 (is
  ((fn
    [v]
    (and (= (:sum-re v) [6.0 8.0]) (= (:diff-re v) [-4.0 -4.0])))
   v38_l196)))


(def
 v41_l215
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
     (fn* [p1__84871#] (Math/sin (double p1__84871#)))
     (range (hm/order G))))
   result
   (rep/matrix-fourier-transform ir G f-map)]
  {:dimension d, :frobenius-norm (rep/frobenius-norm result)}))


(deftest
 t42_l224
 (is
  ((fn [v] (and (= (:dimension v) 3) (> (:frobenius-norm v) 0.0)))
   v41_l215)))


(def
 v44_l232
 (let
  [G
   (hm/symmetric-group 4)
   parts
   [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]
   irreps
   (mapv hm/irrep parts)
   f-map
   (zipmap
    (hm/elements G)
    (map
     (fn* [p1__84872#] (Math/sin (double p1__84872#)))
     (range (hm/order G))))
   f-hats
   (rep/matrix-fourier-transform-all G f-map irreps)
   lhs
   (rep/plancherel-lhs G f-map)
   rhs
   (rep/plancherel-rhs G f-hats irreps)]
  (< (Math/abs (- lhs rhs)) 1.0E-10)))


(deftest t45_l242 (is (true? v44_l232)))
