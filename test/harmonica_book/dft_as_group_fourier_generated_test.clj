(ns
 harmonica-book.dft-as-group-fourier-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [harmonica-book.book-helpers :refer [allclose?]]
  [fastmath.transform :as t]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [tech.v3.datatype.convolve :as dt-conv]
  [tech.v3.tensor :as tensor]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l34
 (def
  temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3 3 4 8 13 18 23 26 25 20 14 8 4]))


(def
 v4_l38
 (->
  (tc/dataset {:month (range 24), :temp temperatures})
  (plotly/base
   {:=x :month,
    :=y :temp,
    :=title "Monthly temperatures — two years of data",
    :=x-title "month",
    :=y-title "°C"})
  (plotly/layer-line)
  (plotly/layer-point {:=mark-size 5})
  plotly/plot))


(def v6_l56 (def G (hm/cyclic-group 24)))


(def v7_l58 (hm/elements G))


(deftest t8_l60 (is (= v7_l58 (range 24))))


(def v10_l65 (hm/op G 15 9))


(deftest t11_l67 (is (= v10_l65 0)))


(def v12_l70 (hm/op G 18 10))


(deftest t13_l72 (is (= v12_l70 4)))


(def v15_l77 (hm/inv G 15))


(deftest t16_l79 (is (= v15_l77 9)))


(def v18_l111 (def ct (hm/character-table G)))
