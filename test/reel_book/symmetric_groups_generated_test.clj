(ns
 reel-book.symmetric-groups-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.reel.impl.permutation :as perm]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l27 (def G (reel/symmetric-group 4)))


(def v4_l29 (reel/order G))


(deftest t5_l31 (is (= v4_l29 24)))


(def v7_l36 (reel/id G))


(deftest t8_l38 (is (= v7_l36 [0 1 2 3])))


(def v10_l43 (perm/transposition 4 1 3))


(deftest t11_l45 (is (= v10_l43 [0 3 2 1])))


(def v13_l52 (reel/op G [1 0 2 3] [0 1 3 2]))


(deftest t14_l54 (is (= v13_l52 [1 0 3 2])))


(def v16_l59 (reel/inv G [1 2 3 0]))


(deftest t17_l61 (is (= v16_l59 [3 0 1 2])))


(def v18_l64 (reel/op G [1 2 3 0] (reel/inv G [1 2 3 0])))


(deftest t19_l66 (is (= v18_l64 [0 1 2 3])))


(def v21_l75 (reel/cycles [1 2 3 0]))


(deftest t22_l77 (is (= v21_l75 [[0 1 2 3]])))


(def v24_l82 (reel/cycles [0 3 2 1]))


(deftest t25_l84 (is (= v24_l82 [[1 3]])))


(def v27_l89 (reel/cycles [0 1 2 3]))


(deftest t28_l91 (is (= v27_l89 [])))


(def v30_l96 (reel/cycles [1 0 3 2]))


(deftest t31_l98 (is (= v30_l96 [[0 1] [2 3]])))


(def v33_l107 (reel/cycle-type [1 2 3 0]))


(deftest t34_l109 (is (= v33_l107 [4])))


(def v35_l112 (reel/cycle-type [1 0 3 2]))


(deftest t36_l114 (is (= v35_l112 [2 2])))


(def v37_l117 (reel/cycle-type [1 0 2 3]))


(deftest t38_l119 (is (= v37_l117 [2 1 1])))


(def v40_l125 (reel/sign [0 1 2 3]))


(deftest t41_l127 (is (= v40_l125 1)))


(def v42_l130 (reel/sign [1 0 2 3]))


(deftest t43_l132 (is (= v42_l130 -1)))


(def
 v45_l138
 (let
  [sigma [1 2 0 3] tau [0 1 3 2]]
  (* (reel/sign sigma) (reel/sign tau))))


(deftest
 t46_l142
 (is (= v45_l138 (reel/sign (reel/op G [1 2 0 3] [0 1 3 2])))))


(def v48_l151 (reel/partitions 4))


(deftest t49_l153 (is (= v48_l151 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def
 v51_l158
 (kind/table
  {:column-names ["$n$" "$p(n)$"],
   :row-vectors
   (mapv (fn [n] [n (count (reel/partitions n))]) (range 1 11))}))


(def v53_l169 (def classes (reel/conjugacy-classes G)))


(def
 v54_l171
 (kind/table
  {:column-names ["Cycle type" "Class size"],
   :row-vectors (mapv (fn [c] [(:cycle-type c) (:size c)]) classes)}))


(def v56_l177 (reduce + (map :size classes)))


(deftest t57_l179 (is (= v56_l177 24)))


(def v59_l184 (count classes))


(deftest t60_l186 (is (= v59_l184 (count (reel/partitions 4)))))


(def
 v62_l195
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
