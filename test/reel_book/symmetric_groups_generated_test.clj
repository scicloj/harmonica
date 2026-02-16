(ns
 reel-book.symmetric-groups-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l26 (def G (reel/symmetric-group 4)))


(def v4_l28 (reel/order G))


(deftest t5_l30 (is (= v4_l28 24)))


(def v7_l35 (reel/id G))


(deftest t8_l37 (is (= v7_l35 [0 1 2 3])))


(def v10_l42 (reel/transposition 4 1 3))


(deftest t11_l44 (is (= v10_l42 [0 3 2 1])))


(def v13_l51 (reel/op G [1 0 2 3] [0 1 3 2]))


(deftest t14_l53 (is (= v13_l51 [1 0 3 2])))


(def v16_l58 (reel/inv G [1 2 3 0]))


(deftest t17_l60 (is (= v16_l58 [3 0 1 2])))


(def v18_l63 (reel/op G [1 2 3 0] (reel/inv G [1 2 3 0])))


(deftest t19_l65 (is (= v18_l63 [0 1 2 3])))


(def v21_l74 (reel/cycles [1 2 3 0]))


(deftest t22_l76 (is (= v21_l74 [[0 1 2 3]])))


(def v24_l81 (reel/cycles [0 3 2 1]))


(deftest t25_l83 (is (= v24_l81 [[1 3]])))


(def v27_l88 (reel/cycles [0 1 2 3]))


(deftest t28_l90 (is (= v27_l88 [])))


(def v30_l95 (reel/cycles [1 0 3 2]))


(deftest t31_l97 (is (= v30_l95 [[0 1] [2 3]])))


(def v33_l106 (reel/cycle-type [1 2 3 0]))


(deftest t34_l108 (is (= v33_l106 [4])))


(def v35_l111 (reel/cycle-type [1 0 3 2]))


(deftest t36_l113 (is (= v35_l111 [2 2])))


(def v37_l116 (reel/cycle-type [1 0 2 3]))


(deftest t38_l118 (is (= v37_l116 [2 1 1])))


(def v40_l124 (reel/sign [0 1 2 3]))


(deftest t41_l126 (is (= v40_l124 1)))


(def v42_l129 (reel/sign [1 0 2 3]))


(deftest t43_l131 (is (= v42_l129 -1)))


(def
 v45_l137
 (let
  [sigma [1 2 0 3] tau [0 1 3 2]]
  (* (reel/sign sigma) (reel/sign tau))))


(deftest
 t46_l141
 (is (= v45_l137 (reel/sign (reel/op G [1 2 0 3] [0 1 3 2])))))


(def v48_l150 (reel/partitions 4))


(deftest t49_l152 (is (= v48_l150 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def
 v51_l157
 (kind/table
  {:column-names ["$n$" "$p(n)$"],
   :row-vectors
   (mapv (fn [n] [n (count (reel/partitions n))]) (range 1 11))}))


(def v53_l168 (def classes (reel/conjugacy-classes G)))


(def
 v54_l170
 (kind/table
  {:column-names ["Cycle type" "Class size"],
   :row-vectors (mapv (fn [c] [(:cycle-type c) (:size c)]) classes)}))


(def v56_l176 (reduce + (map :size classes)))


(deftest t57_l178 (is (= v56_l176 24)))


(def v59_l183 (count classes))


(deftest t60_l185 (is (= v59_l183 (count (reel/partitions 4)))))


(def
 v62_l194
 (kind/table
  {:column-names ["$n$" "$|S_n|$" "# classes"],
   :row-vectors
   (mapv
    (fn
     [n]
     (let
      [G (reel/symmetric-group n)]
      [n (reel/order G) (count (reel/conjugacy-classes G))]))
    (range 1 9))}))
