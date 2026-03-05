(ns
 harmonica-book.algorithms-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [scicloj.harmonica.protocols :as p]
  [scicloj.harmonica.combinatorics.permutation :as perm]
  [scicloj.harmonica.combinatorics.partition :as part]
  [scicloj.harmonica.combinatorics.young-tableaux :as yt]
  [scicloj.harmonica.combinatorics.murnaghan-nakayama :as mn]
  [scicloj.harmonica.combinatorics.young-orthogonal :as yo]
  [scicloj.harmonica.combinatorics.riffle :as riffle]
  [scicloj.harmonica.analysis.representations :as rep]
  [tech.v3.datatype.functional :as dfn]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l56 (vec (mn/partition-seq [3 2])))


(deftest t4_l58 (is (= v3_l56 [true true false true false])))


(def v6_l62 (vec (mn/partition-seq [4 2 1])))


(deftest t7_l64 (is (= v6_l62 [true false true false true true false])))


(def v9_l81 (mn/chi [3 2] [2 2 1]))


(deftest t10_l83 (is (= v9_l81 1)))


(def
 v12_l87
 (let
  [ct
   (hm/character-table (hm/symmetric-group 5))
   row-idx
   (.indexOf (:irrep-labels ct) [3 2])
   col-idx
   (.indexOf (:classes ct) [2 2 1])]
  (cx/re (((:table ct) row-idx) col-idx))))
