(ns
 harmonica-book.symmetric-groups-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l31 (def G (hm/symmetric-group 4)))


(def v4_l33 G)


(def v5_l35 (hm/order G))


(deftest t6_l37 (is (= v5_l35 24)))


(def v8_l42 (hm/id G))


(deftest t9_l44 (is (= v8_l42 [0 1 2 3])))


(def v11_l49 (hm/transposition 4 1 3))


(deftest t12_l51 (is (= v11_l49 [0 3 2 1])))


(def v14_l58 (hm/op G [1 0 2 3] [0 1 3 2]))


(deftest t15_l60 (is (= v14_l58 [1 0 3 2])))


(def v17_l65 (hm/inv G [1 2 3 0]))


(deftest t18_l67 (is (= v17_l65 [3 0 1 2])))


(def v19_l70 (hm/op G [1 2 3 0] (hm/inv G [1 2 3 0])))


(deftest t20_l72 (is (= v19_l70 [0 1 2 3])))


(def
 v22_l80
 (let
  [sigma [1 2 0 3] tau [0 1 3 2]]
  [(hm/op G sigma tau) (hm/op G tau sigma)]))


(deftest t23_l85 (is ((fn [v] (not= (first v) (second v))) v22_l80)))


(def v25_l94 (hm/cycles [1 2 3 0]))


(deftest t26_l96 (is (= v25_l94 [[0 1 2 3]])))


(def v28_l101 (hm/cycles [0 3 2 1]))


(deftest t29_l103 (is (= v28_l101 [[1 3]])))


(def v31_l108 (hm/cycles [0 1 2 3]))


(deftest t32_l110 (is (= v31_l108 [])))


(def v34_l115 (hm/cycles [1 0 3 2]))


(deftest t35_l117 (is (= v34_l115 [[0 1] [2 3]])))


(def v37_l125 (kind/hiccup (hm/cycle-diagram-svg [1 2 3 0])))


(def v39_l129 (kind/hiccup (hm/cycle-diagram-svg [1 0 3 2])))


(def v41_l133 (kind/hiccup (hm/cycle-diagram-svg [2 3 4 1 0 5])))


(def v43_l142 (hm/cycle-type [1 2 3 0]))


(deftest t44_l144 (is (= v43_l142 [4])))


(def v45_l147 (hm/cycle-type [1 0 3 2]))


(deftest t46_l149 (is (= v45_l147 [2 2])))


(def v47_l152 (hm/cycle-type [1 0 2 3]))


(deftest t48_l154 (is (= v47_l152 [2 1 1])))


(def v50_l162 (hm/sign [0 1 2 3]))


(deftest t51_l164 (is (= v50_l162 1)))


(def v52_l167 (hm/sign [1 0 2 3]))


(deftest t53_l169 (is (= v52_l167 -1)))


(def
 v55_l174
 (let
  [sigma [1 2 0 3] tau [0 1 3 2]]
  (* (hm/sign sigma) (hm/sign tau))))


(deftest
 t56_l178
 (is (= v55_l174 (hm/sign (hm/op G [1 2 0 3] [0 1 3 2])))))


(def v58_l189 (hm/partitions 4))


(deftest t59_l191 (is (= v58_l189 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def
 v61_l196
 (kind/table
  {:column-names ["n" "p(n)"],
   :row-vectors
   (mapv (fn [n] [n (count (hm/partitions n))]) (range 1 11))}))


(def
 v63_l208
 (kind/hiccup
  (into
   [:div
    {:style
     "display: flex; flex-wrap: wrap; gap: 24px; align-items: flex-end;"}]
   (for
    [p (hm/partitions 5)]
    [:div
     {:style "text-align: center;"}
     (hm/young-diagram-svg p)
     [:div
      {:style
       "margin-top: 4px; font-family: monospace; font-size: 13px;"}
      (str p)]]))))


(def v65_l222 (hm/partition-conjugate [4 2 1]))


(deftest t66_l224 (is (= v65_l222 [3 2 1 1])))


(def v67_l226 (hm/partition-conjugate [3 3]))


(deftest t68_l228 (is (= v67_l226 [2 2 2])))


(def
 v70_l232
 (kind/hiccup
  (let
   [p [4 2 1] pc (hm/partition-conjugate p)]
   [:div
    {:style "display: flex; gap: 40px; align-items: flex-end;"}
    [:div
     {:style "text-align: center;"}
     (hm/young-diagram-svg p)
     [:div
      {:style "margin-top: 4px; font-family: monospace;"}
      (str "λ = " p)]]
    [:div
     {:style
      "text-align: center; font-size: 24px; align-self: center;"}
     "↔"]
    [:div
     {:style "text-align: center;"}
     (hm/young-diagram-svg pc :fill "#e67e22")
     [:div
      {:style "margin-top: 4px; font-family: monospace;"}
      (str "λ' = " pc)]]])))


(def
 v72_l246
 (= [4 2 1] (hm/partition-conjugate (hm/partition-conjugate [4 2 1]))))


(deftest t73_l248 (is (true? v72_l246)))


(def v75_l266 (kind/hiccup (hm/young-hooks-svg [4 2 1])))


(def v77_l270 (kind/hiccup (hm/young-hooks-svg [3 2 2])))


(def
 v79_l274
 (let
  [results
   (for
    [lambda (hm/partitions 5)]
    (=
     (hm/hook-length-dimension lambda)
     (count (hm/standard-young-tableaux lambda))))]
  (every? true? results)))


(deftest t80_l280 (is (true? v79_l274)))


(def
 v82_l284
 (kind/table
  {:column-names ["λ" "f(λ) (hook-length)" "# SYT (enumerated)"],
   :row-vectors
   (mapv
    (fn
     [lambda]
     [(str lambda)
      (hm/hook-length-dimension lambda)
      (count (hm/standard-young-tableaux lambda))])
    (hm/partitions 5))}))


(def v84_l300 (hm/standard-young-tableaux [2 1]))


(deftest t85_l302 (is (= v84_l300 [[[1 2] [3]] [[1 3] [2]]])))


(def v86_l304 (count (hm/standard-young-tableaux [3 2])))


(deftest t87_l306 (is (= v86_l304 5)))


(def
 v89_l310
 (kind/hiccup
  (into
   [:div
    {:style
     "display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start;"}]
   (for
    [t (hm/standard-young-tableaux [3 2])]
    [:div {:style "text-align: center;"} (hm/syt-svg t)]))))


(def v91_l323 (def classes (hm/conjugacy-classes G)))


(def
 v92_l325
 (kind/table
  {:column-names ["Cycle type" "Class size"],
   :row-vectors (mapv (fn [c] [(:cycle-type c) (:size c)]) classes)}))


(def v94_l331 (reduce + (map :size classes)))


(deftest t95_l333 (is (= v94_l331 24)))


(def v97_l339 (count classes))


(deftest t98_l341 (is (= v97_l339 (count (hm/partitions 4)))))


(def
 v100_l351
 (let
  [n 5 dims (mapv hm/hook-length-dimension (hm/partitions n))]
  (reduce + (map (fn* [p1__89663#] (* p1__89663# p1__89663#)) dims))))


(deftest t101_l355 (is (= v100_l351 120)))


(def
 v103_l365
 (kind/table
  {:column-names ["n" "|Sₙ|" "# classes"],
   :row-vectors
   (mapv
    (fn
     [n]
     (let
      [G (hm/symmetric-group n)]
      [n (hm/order G) (count (hm/conjugacy-classes G))]))
    (range 1 9))}))


(def
 v105_l378
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
