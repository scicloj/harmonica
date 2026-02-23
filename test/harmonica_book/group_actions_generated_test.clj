(ns
 harmonica-book.group-actions-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l27 (defn rotation-action [n] (fn [g x] (mod (+ x g) n))))


(def
 v5_l33
 (let
  [G (hm/cyclic-group 5) act (rotation-action 5)]
  (hm/orbit G act 0)))


(deftest t6_l37 (is (= v5_l33 #{0 1 4 3 2})))


(def
 v8_l41
 (let
  [G (hm/cyclic-group 5) act (rotation-action 5)]
  (count (hm/orbits G act (range 5)))))
