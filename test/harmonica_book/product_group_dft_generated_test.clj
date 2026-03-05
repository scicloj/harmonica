(ns
 harmonica-book.product-group-dft-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l28 (def G1 (hm/cyclic-group 4)))


(def v4_l29 (def G2 (hm/cyclic-group 3)))


(def v5_l30 (def G (hm/product-group G1 G2)))


(def v6_l32 (hm/order G))


(deftest t7_l34 (is (= v6_l32 12)))


(def v9_l38 (take 6 (hm/elements G)))


(def v11_l42 (hm/op G [1 2] [3 1]))


(deftest t12_l44 (is (= v11_l42 [0 0])))


(def v14_l57 (def ct (hm/character-table G)))
