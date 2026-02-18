(ns
 harmonica-book.symmetric-groups-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l32 (def G (hm/symmetric-group 4)))


(def v4_l34 (hm/order G))


(deftest t5_l36 (is (= v4_l34 24)))


(def v7_l41 (hm/id G))


(deftest t8_l43 (is (= v7_l41 [0 1 2 3])))


(def v10_l48 (hm/transposition 4 1 3))


(deftest t11_l50 (is (= v10_l48 [0 3 2 1])))


(def v13_l57 (hm/op G [1 0 2 3] [0 1 3 2]))


(deftest t14_l59 (is (= v13_l57 [1 0 3 2])))


(def v16_l64 (hm/inv G [1 2 3 0]))


(deftest t17_l66 (is (= v16_l64 [3 0 1 2])))


(def v18_l69 (hm/op G [1 2 3 0] (hm/inv G [1 2 3 0])))


(deftest t19_l71 (is (= v18_l69 [0 1 2 3])))


(def
 v21_l79
 (let
  [sigma [1 2 0 3] tau [0 1 3 2]]
  [(hm/op G sigma tau) (hm/op G tau sigma)]))


(deftest t22_l84 (is ((fn [v] (not= (first v) (second v))) v21_l79)))


(def v24_l93 (hm/cycles [1 2 3 0]))


(deftest t25_l95 (is (= v24_l93 [[0 1 2 3]])))


(def v27_l100 (hm/cycles [0 3 2 1]))


(deftest t28_l102 (is (= v27_l100 [[1 3]])))


(def v30_l107 (hm/cycles [0 1 2 3]))


(deftest t31_l109 (is (= v30_l107 [])))


(def v33_l114 (hm/cycles [1 0 3 2]))


(deftest t34_l116 (is (= v33_l114 [[0 1] [2 3]])))


(def v36_l126 (hm/cycle-type [1 2 3 0]))


(deftest t37_l128 (is (= v36_l126 [4])))


(def v38_l131 (hm/cycle-type [1 0 3 2]))


(deftest t39_l133 (is (= v38_l131 [2 2])))


(def v40_l136 (hm/cycle-type [1 0 2 3]))


(deftest t41_l138 (is (= v40_l136 [2 1 1])))


(def v43_l146 (hm/sign [0 1 2 3]))


(deftest t44_l148 (is (= v43_l146 1)))


(def v45_l151 (hm/sign [1 0 2 3]))


(deftest t46_l153 (is (= v45_l151 -1)))


(def
 v48_l158
 (let
  [sigma [1 2 0 3] tau [0 1 3 2]]
  (* (hm/sign sigma) (hm/sign tau))))


(deftest
 t49_l162
 (is (= v48_l158 (hm/sign (hm/op G [1 2 0 3] [0 1 3 2])))))


(def v51_l173 (hm/partitions 4))


(deftest t52_l175 (is (= v51_l173 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def
 v54_l180
 (kind/table
  {:column-names ["$n$" "$p(n)$"],
   :row-vectors
   (mapv (fn [n] [n (count (hm/partitions n))]) (range 1 11))}))


(def v56_l192 (def classes (hm/conjugacy-classes G)))


(def
 v57_l194
 (kind/table
  {:column-names ["Cycle type" "Class size"],
   :row-vectors (mapv (fn [c] [(:cycle-type c) (:size c)]) classes)}))


(def v59_l200 (reduce + (map :size classes)))


(deftest t60_l202 (is (= v59_l200 24)))


(def v62_l208 (count classes))


(deftest t63_l210 (is (= v62_l208 (count (hm/partitions 4)))))


(def
 v65_l221
 (kind/table
  {:column-names ["$n$" "$|S_n|$" "# classes"],
   :row-vectors
   (mapv
    (fn
     [n]
     (let
      [G (hm/symmetric-group n)]
      [n (hm/order G) (count (hm/conjugacy-classes G))]))
    (range 1 9))}))


(def
 v67_l234
 (->
  (tc/dataset
   {:n (range 1 9),
    :log-order
    (mapv
     (fn [n] (Math/log10 (double (hm/order (hm/symmetric-group n)))))
     (range 1 9)),
    :num-classes
    (mapv
     (fn [n] (count (hm/conjugacy-classes (hm/symmetric-group n))))
     (range 1 9))})
  (plotly/base
   {:=x :n,
    :=y :log-order,
    :=x-title "n",
    :=y-title "log₁₀(|Sₙ|)",
    :=title "Factorial growth of Sₙ"})
  (plotly/layer-line)
  (plotly/layer-point {:=mark-size 8})
  plotly/plot))
