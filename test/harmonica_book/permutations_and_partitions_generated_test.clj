(ns
 harmonica-book.permutations-and-partitions-generated-test
 (:require
  [scicloj.harmonica.core :as hm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l28
 (let
  [G (hm/symmetric-group 5) elts (vec (hm/elements G))]
  (every?
   (fn [[s t]] (= (hm/sign (hm/op G s t)) (* (hm/sign s) (hm/sign t))))
   (for [a elts b elts] [a b]))))


(deftest t4_l35 (is (true? v3_l28)))


(def
 v6_l42
 (let
  [results
   (for
    [n (range 1 7)]
    (let
     [G (hm/symmetric-group n) e (hm/id G)]
     (every?
      (fn
       [sigma]
       (let
        [si (hm/inv G sigma)]
        (and (= (hm/op G sigma si) e) (= (hm/op G si sigma) e))))
      (hm/elements G))))]
  (every? true? results)))


(deftest t7_l53 (is (true? v6_l42)))


(def
 v9_l61
 (let
  [results
   (for
    [n (range 1 7)]
    (let
     [G
      (hm/symmetric-group n)
      id-perm
      (hm/id G)
      make-swap
      (fn [i] (let [v (vec (range n))] (assoc v i (inc i) (inc i) i)))]
     (every?
      (fn
       [sigma]
       (let
        [swaps
         (hm/adjacent-transposition-decomposition sigma)
         reconstructed
         (reduce (fn [p i] (hm/op G p (make-swap i))) id-perm swaps)]
        (= sigma reconstructed)))
      (hm/elements G))))]
  (every? true? results)))


(deftest t10_l76 (is (true? v9_l61)))


(def
 v12_l83
 (let
  [results
   (for
    [n (range 3 7)]
    (let
     [G (hm/symmetric-group n) classes (hm/conjugacy-classes G)]
     (every?
      (fn
       [cls]
       (= 1 (count (set (map hm/cycle-type (:elements cls))))))
      classes)))]
  (every? true? results)))


(deftest t13_l92 (is (true? v12_l83)))


(def
 v15_l99
 (let
  [G (hm/symmetric-group 4) elts (vec (hm/elements G))]
  (every?
   (fn
    [[a b c]]
    (= (hm/op G (hm/op G a b) c) (hm/op G a (hm/op G b c))))
   (for [a elts b elts c elts] [a b c]))))


(deftest t16_l106 (is (true? v15_l99)))


(def
 v18_l113
 (defn
  perm-order
  "Compute the order of a permutation by repeated composition."
  [G sigma]
  (let
   [e (hm/id G)]
   (loop
    [k 1 current sigma]
    (if (= current e) k (recur (inc k) (hm/op G current sigma)))))))


(def
 v20_l125
 (let
  [G (hm/symmetric-group 5)]
  (every?
   (fn
    [sigma]
    (let
     [ct
      (hm/cycle-type sigma)
      expected
      (reduce
       (fn
        [a b]
        (/ (* a b) (biginteger (.gcd (biginteger a) (biginteger b)))))
       (map biginteger ct))]
     (= (perm-order G sigma) (long expected))))
   (hm/elements G))))


(deftest t21_l133 (is (true? v20_l125)))


(def v23_l143 (hm/partitions 1))


(deftest t24_l145 (is (= v23_l143 [[1]])))


(def v25_l147 (hm/partitions 4))


(deftest t26_l149 (is (= v25_l147 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v27_l151 (hm/partitions 5))


(deftest
 t28_l153
 (is
  (= v27_l151 [[5] [4 1] [3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]])))


(def
 v30_l157
 (kind/table
  {:column-names ["$n$" "$p(n)$"],
   :row-vectors
   (mapv (fn [n] [n (count (hm/partitions n))]) (range 1 16))}))


(def
 v32_l163
 (let
  [results
   (for
    [n (range 1 13)]
    (every?
     (fn [p] (and (every? pos-int? p) (apply >= p) (= n (reduce + p))))
     (hm/partitions n)))]
  (every? true? results)))


(deftest t33_l172 (is (true? v32_l163)))


(def
 v35_l183
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


(def v37_l197 (hm/partition-conjugate [4 2 1]))


(deftest t38_l199 (is (= v37_l197 [3 2 1 1])))


(def v39_l201 (hm/partition-conjugate [3 3]))


(deftest t40_l203 (is (= v39_l201 [2 2 2])))


(def v41_l205 (hm/partition-conjugate [5]))


(deftest t42_l207 (is (= v41_l205 [1 1 1 1 1])))


(def
 v44_l211
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
 v46_l225
 (let
  [results
   (for
    [n (range 1 11) p (hm/partitions n)]
    (= p (hm/partition-conjugate (hm/partition-conjugate p))))]
  (every? true? results)))


(deftest t47_l231 (is (true? v46_l225)))


(def
 v49_l235
 (let
  [results
   (for
    [n (range 1 11) p (hm/partitions n)]
    (= (reduce + p) (reduce + (hm/partition-conjugate p))))]
  (every? true? results)))


(deftest t50_l241 (is (true? v49_l235)))


(def
 v52_l257
 (defn
  hook-lengths
  "Compute all hook lengths for a partition."
  [lambda]
  (let
   [conj (hm/partition-conjugate lambda)]
   (for
    [i (range (count lambda)) j (range (nth lambda i))]
    (+ (- (nth lambda i) j) (- (nth conj j) i) -1)))))


(def v54_l273 (kind/hiccup (hm/young-hooks-svg [4 2 1])))


(def v56_l277 (kind/hiccup (hm/young-hooks-svg [3 2 2])))


(def v58_l284 (defn factorial [n] (reduce *' (range 1 (inc n)))))


(def
 v59_l286
 (let
  [results
   (for
    [n (range 1 8) lambda (hm/partitions n)]
    (let
     [hooks
      (hook-lengths lambda)
      hook-product
      (reduce *' hooks)
      formula-dim
      (/ (factorial n) hook-product)
      enum-dim
      (count (hm/standard-young-tableaux lambda))
      lib-dim
      (hm/hook-length-dimension lambda)]
     (and (= formula-dim enum-dim) (= formula-dim lib-dim))))]
  (every? true? results)))


(deftest t60_l298 (is (true? v59_l286)))


(def
 v62_l302
 (kind/table
  {:column-names
   ["Partition λ"
    "Hook product"
    "f^λ = 5!/hooks"
    "# SYT (enumerated)"],
   :row-vectors
   (mapv
    (fn
     [lambda]
     (let
      [hooks
       (hook-lengths lambda)
       hp
       (reduce *' hooks)
       dim
       (/ (factorial 5) hp)
       enum
       (count (hm/standard-young-tableaux lambda))]
      [(str lambda) (str hp) (str dim) (str enum)]))
    (hm/partitions 5))}))


(def
 v64_l319
 (let
  [results
   (for
    [n (range 1 9)]
    (let
     [dims
      (map hm/hook-length-dimension (hm/partitions n))
      sum-sq
      (reduce
       +
       (map (fn* [p1__72918#] (* p1__72918# p1__72918#)) dims))]
     (= sum-sq (factorial n))))]
  (every? true? results)))


(deftest t65_l326 (is (true? v64_l319)))


(def v67_l335 (hm/standard-young-tableaux [2 1]))


(deftest t68_l337 (is (= v67_l335 [[[1 2] [3]] [[1 3] [2]]])))


(def v69_l339 (count (hm/standard-young-tableaux [3 2])))


(deftest t70_l341 (is (= v69_l339 5)))


(def
 v72_l347
 (kind/hiccup
  (into
   [:div
    {:style
     "display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start;"}]
   (for
    [t (hm/standard-young-tableaux [3 2])]
    [:div {:style "text-align: center;"} (hm/syt-svg t)]))))


(def
 v74_l361
 (defn
  class-size-formula
  "Compute conjugacy class size from the partition formula."
  [n mu]
  (let
   [freq (frequencies mu)]
   (/
    (factorial n)
    (reduce
     *'
     (map
      (fn [[k ak]] (*' (reduce *' (repeat ak k)) (factorial ak)))
      freq))))))


(def
 v76_l374
 (let
  [results
   (for
    [n (range 2 8)]
    (let
     [G (hm/symmetric-group n) classes (hm/conjugacy-classes G)]
     (and
      (every?
       (fn
        [cls]
        (let
         [ct (hm/cycle-type (:representative cls))]
         (= (:size cls) (class-size-formula n ct))))
       classes)
      (= (reduce + (map :size classes)) (factorial n)))))]
  (every? true? results)))


(deftest t77_l388 (is (true? v76_l374)))


(def
 v79_l392
 (let
  [G (hm/symmetric-group 5) classes (hm/conjugacy-classes G)]
  (kind/table
   {:column-names ["Cycle type" "Class size" "Formula"],
    :row-vectors
    (mapv
     (fn
      [cls]
      (let
       [ct (hm/cycle-type (:representative cls))]
       [(str ct) (:size cls) (class-size-formula 5 ct)]))
     classes)})))


(def v81_l412 (kind/hiccup (hm/cycle-diagram-svg [1 2 3 0])))


(def v83_l416 (kind/hiccup (hm/cycle-diagram-svg [1 0 3 2])))


(def v85_l420 (kind/hiccup (hm/cycle-diagram-svg [2 3 4 1 0 5])))
