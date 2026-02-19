(ns
 harmonica-book.symmetric-groups-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l31 (def G (hm/symmetric-group 4)))


(def v4_l33 (hm/order G))


(deftest t5_l35 (is (= v4_l33 24)))


(def v7_l40 (hm/id G))


(deftest t8_l42 (is (= v7_l40 [0 1 2 3])))


(def v10_l47 (hm/transposition 4 1 3))


(deftest t11_l49 (is (= v10_l47 [0 3 2 1])))


(def v13_l56 (hm/op G [1 0 2 3] [0 1 3 2]))


(deftest t14_l58 (is (= v13_l56 [1 0 3 2])))


(def v16_l63 (hm/inv G [1 2 3 0]))


(deftest t17_l65 (is (= v16_l63 [3 0 1 2])))


(def v18_l68 (hm/op G [1 2 3 0] (hm/inv G [1 2 3 0])))


(deftest t19_l70 (is (= v18_l68 [0 1 2 3])))


(def
 v21_l78
 (let
  [sigma [1 2 0 3] tau [0 1 3 2]]
  [(hm/op G sigma tau) (hm/op G tau sigma)]))


(deftest t22_l83 (is ((fn [v] (not= (first v) (second v))) v21_l78)))


(def v24_l92 (hm/cycles [1 2 3 0]))


(deftest t25_l94 (is (= v24_l92 [[0 1 2 3]])))


(def v27_l99 (hm/cycles [0 3 2 1]))


(deftest t28_l101 (is (= v27_l99 [[1 3]])))


(def v30_l106 (hm/cycles [0 1 2 3]))


(deftest t31_l108 (is (= v30_l106 [])))


(def v33_l113 (hm/cycles [1 0 3 2]))


(deftest t34_l115 (is (= v33_l113 [[0 1] [2 3]])))


(def v36_l123 (kind/hiccup (hm/cycle-diagram-svg [1 2 3 0])))


(def v38_l127 (kind/hiccup (hm/cycle-diagram-svg [1 0 3 2])))


(def v40_l131 (kind/hiccup (hm/cycle-diagram-svg [2 3 4 1 0 5])))


(def v42_l140 (hm/cycle-type [1 2 3 0]))


(deftest t43_l142 (is (= v42_l140 [4])))


(def v44_l145 (hm/cycle-type [1 0 3 2]))


(deftest t45_l147 (is (= v44_l145 [2 2])))


(def v46_l150 (hm/cycle-type [1 0 2 3]))


(deftest t47_l152 (is (= v46_l150 [2 1 1])))


(def v49_l160 (hm/sign [0 1 2 3]))


(deftest t50_l162 (is (= v49_l160 1)))


(def v51_l165 (hm/sign [1 0 2 3]))


(deftest t52_l167 (is (= v51_l165 -1)))


(def
 v54_l172
 (let
  [sigma [1 2 0 3] tau [0 1 3 2]]
  (* (hm/sign sigma) (hm/sign tau))))


(deftest
 t55_l176
 (is (= v54_l172 (hm/sign (hm/op G [1 2 0 3] [0 1 3 2])))))


(def v57_l187 (hm/partitions 4))


(deftest t58_l189 (is (= v57_l187 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def
 v60_l194
 (kind/table
  {:column-names ["$n$" "$p(n)$"],
   :row-vectors
   (mapv (fn [n] [n (count (hm/partitions n))]) (range 1 11))}))


(def
 v62_l206
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


(def v64_l220 (hm/partition-conjugate [4 2 1]))


(deftest t65_l222 (is (= v64_l220 [3 2 1 1])))


(def v66_l224 (hm/partition-conjugate [3 3]))


(deftest t67_l226 (is (= v66_l224 [2 2 2])))


(def
 v69_l230
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
 v71_l244
 (= [4 2 1] (hm/partition-conjugate (hm/partition-conjugate [4 2 1]))))


(deftest t72_l246 (is (true? v71_l244)))


(def v74_l264 (kind/hiccup (hm/young-hooks-svg [4 2 1])))


(def v76_l268 (kind/hiccup (hm/young-hooks-svg [3 2 2])))


(def
 v78_l272
 (let
  [results
   (for
    [lambda (hm/partitions 5)]
    (=
     (hm/hook-length-dimension lambda)
     (count (hm/standard-young-tableaux lambda))))]
  (every? true? results)))


(deftest t79_l278 (is (true? v78_l272)))


(def
 v81_l282
 (kind/table
  {:column-names
   ["$\\lambda$" "$f^\\lambda$ (hook-length)" "# SYT (enumerated)"],
   :row-vectors
   (mapv
    (fn
     [lambda]
     [(str lambda)
      (hm/hook-length-dimension lambda)
      (count (hm/standard-young-tableaux lambda))])
    (hm/partitions 5))}))


(def v83_l298 (hm/standard-young-tableaux [2 1]))


(deftest t84_l300 (is (= v83_l298 [[[1 2] [3]] [[1 3] [2]]])))


(def v85_l302 (count (hm/standard-young-tableaux [3 2])))


(deftest t86_l304 (is (= v85_l302 5)))


(def
 v88_l308
 (kind/hiccup
  (into
   [:div
    {:style
     "display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start;"}]
   (for
    [t (hm/standard-young-tableaux [3 2])]
    [:div {:style "text-align: center;"} (hm/syt-svg t)]))))


(def v90_l321 (def classes (hm/conjugacy-classes G)))


(def
 v91_l323
 (kind/table
  {:column-names ["Cycle type" "Class size"],
   :row-vectors (mapv (fn [c] [(:cycle-type c) (:size c)]) classes)}))


(def v93_l329 (reduce + (map :size classes)))


(deftest t94_l331 (is (= v93_l329 24)))


(def v96_l337 (count classes))


(deftest t97_l339 (is (= v96_l337 (count (hm/partitions 4)))))


(def
 v99_l349
 (let
  [n 5 dims (mapv hm/hook-length-dimension (hm/partitions n))]
  (reduce
   +
   (map (fn* [p1__117342#] (* p1__117342# p1__117342#)) dims))))


(deftest t100_l353 (is (= v99_l349 120)))


(def
 v102_l363
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
 v104_l376
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
