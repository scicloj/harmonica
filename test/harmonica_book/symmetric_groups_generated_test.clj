(ns
 harmonica-book.symmetric-groups-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l28 (def G (hm/symmetric-group 4)))


(def v4_l30 (hm/order G))


(deftest t5_l32 (is (= v4_l30 24)))


(def v7_l37 (hm/id G))


(deftest t8_l39 (is (= v7_l37 [0 1 2 3])))


(def v10_l44 (hm/transposition 4 1 3))


(deftest t11_l46 (is (= v10_l44 [0 3 2 1])))


(def v13_l53 (hm/op G [1 0 2 3] [0 1 3 2]))


(deftest t14_l55 (is (= v13_l53 [1 0 3 2])))


(def v16_l60 (hm/inv G [1 2 3 0]))


(deftest t17_l62 (is (= v16_l60 [3 0 1 2])))


(def v18_l65 (hm/op G [1 2 3 0] (hm/inv G [1 2 3 0])))


(deftest t19_l67 (is (= v18_l65 [0 1 2 3])))


(def
 v21_l75
 (let
  [sigma [1 2 0 3] tau [0 1 3 2]]
  [(hm/op G sigma tau) (hm/op G tau sigma)]))


(deftest t22_l80 (is ((fn [v] (not= (first v) (second v))) v21_l75)))


(def v24_l89 (hm/cycles [1 2 3 0]))


(deftest t25_l91 (is (= v24_l89 [[0 1 2 3]])))


(def v27_l96 (hm/cycles [0 3 2 1]))


(deftest t28_l98 (is (= v27_l96 [[1 3]])))


(def v30_l103 (hm/cycles [0 1 2 3]))


(deftest t31_l105 (is (= v30_l103 [])))


(def v33_l110 (hm/cycles [1 0 3 2]))


(deftest t34_l112 (is (= v33_l110 [[0 1] [2 3]])))


(def v36_l120 (kind/hiccup (hm/cycle-diagram-svg [1 2 3 0])))


(def v38_l124 (kind/hiccup (hm/cycle-diagram-svg [1 0 3 2])))


(def v40_l128 (kind/hiccup (hm/cycle-diagram-svg [2 3 4 1 0 5])))


(def v42_l137 (hm/cycle-type [1 2 3 0]))


(deftest t43_l139 (is (= v42_l137 [4])))


(def v44_l142 (hm/cycle-type [1 0 3 2]))


(deftest t45_l144 (is (= v44_l142 [2 2])))


(def v46_l147 (hm/cycle-type [1 0 2 3]))


(deftest t47_l149 (is (= v46_l147 [2 1 1])))


(def v49_l157 (hm/sign [0 1 2 3]))


(deftest t50_l159 (is (= v49_l157 1)))


(def v51_l162 (hm/sign [1 0 2 3]))


(deftest t52_l164 (is (= v51_l162 -1)))


(def
 v54_l169
 (let
  [sigma [1 2 0 3] tau [0 1 3 2]]
  (* (hm/sign sigma) (hm/sign tau))))


(deftest
 t55_l173
 (is (= v54_l169 (hm/sign (hm/op G [1 2 0 3] [0 1 3 2])))))


(def v57_l184 (hm/partitions 4))


(deftest t58_l186 (is (= v57_l184 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def
 v60_l191
 (kind/table
  {:column-names ["$n$" "$p(n)$"],
   :row-vectors
   (mapv (fn [n] [n (count (hm/partitions n))]) (range 1 11))}))


(def
 v62_l203
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


(def v64_l217 (hm/partition-conjugate [4 2 1]))


(deftest t65_l219 (is (= v64_l217 [3 2 1 1])))


(def v66_l221 (hm/partition-conjugate [3 3]))


(deftest t67_l223 (is (= v66_l221 [2 2 2])))


(def
 v69_l227
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
 v71_l241
 (= [4 2 1] (hm/partition-conjugate (hm/partition-conjugate [4 2 1]))))


(deftest t72_l243 (is (true? v71_l241)))


(def v74_l261 (kind/hiccup (hm/young-hooks-svg [4 2 1])))


(def v76_l265 (kind/hiccup (hm/young-hooks-svg [3 2 2])))


(def
 v78_l269
 (let
  [results
   (for
    [lambda (hm/partitions 5)]
    (=
     (hm/hook-length-dimension lambda)
     (count (hm/standard-young-tableaux lambda))))]
  (every? true? results)))


(deftest t79_l275 (is (true? v78_l269)))


(def
 v81_l279
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


(def v83_l295 (hm/standard-young-tableaux [2 1]))


(deftest t84_l297 (is (= v83_l295 [[[1 2] [3]] [[1 3] [2]]])))


(def v85_l299 (count (hm/standard-young-tableaux [3 2])))


(deftest t86_l301 (is (= v85_l299 5)))


(def
 v88_l305
 (kind/hiccup
  (into
   [:div
    {:style
     "display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start;"}]
   (for
    [t (hm/standard-young-tableaux [3 2])]
    [:div {:style "text-align: center;"} (hm/syt-svg t)]))))


(def v90_l318 (def classes (hm/conjugacy-classes G)))


(def
 v91_l320
 (kind/table
  {:column-names ["Cycle type" "Class size"],
   :row-vectors (mapv (fn [c] [(:cycle-type c) (:size c)]) classes)}))


(def v93_l326 (reduce + (map :size classes)))


(deftest t94_l328 (is (= v93_l326 24)))


(def v96_l334 (count classes))


(deftest t97_l336 (is (= v96_l334 (count (hm/partitions 4)))))


(def
 v99_l346
 (let
  [n 5 dims (mapv hm/hook-length-dimension (hm/partitions n))]
  (reduce + (map (fn* [p1__74125#] (* p1__74125# p1__74125#)) dims))))


(deftest t100_l350 (is (= v99_l346 120)))


(def
 v102_l360
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
 v104_l373
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
