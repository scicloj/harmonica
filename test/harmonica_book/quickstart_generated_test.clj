(ns
 harmonica-book.quickstart-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l32 (defn rotation-action [n] (fn [g x] (mod (+ x g) n))))


(def
 v4_l35
 (let
  [G
   (hm/cyclic-group 8)
   ci
   (hm/cycle-index G (rotation-action 8) (range 8))]
  (hm/polya-count ci 3)))


(deftest t5_l39 (is (= v4_l35 834)))


(def
 v7_l45
 (defn
  dihedral-action
  [n]
  (fn [[t k] x] (case t :r (mod (+ x k) n) :s (mod (- k x) n)))))


(def
 v8_l51
 (let
  [G
   (hm/dihedral-group 8)
   ci
   (hm/cycle-index G (dihedral-action 8) (range 8))]
  (hm/polya-count ci 3)))


(deftest t9_l55 (is (= v8_l51 498)))


(def v11_l69 (def G (hm/cyclic-group 24)))


(def v12_l70 (def ct (hm/character-table G)))
