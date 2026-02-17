(ns
 reel-book.permutations-and-partitions-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l28
 (let
  [G (reel/symmetric-group 5) elts (vec (reel/elements G))]
  (every?
   (fn
    [[s t]]
    (= (reel/sign (reel/op G s t)) (* (reel/sign s) (reel/sign t))))
   (for [a elts b elts] [a b]))))


(deftest t4_l35 (is (true? v3_l28)))


(def
 v6_l42
 (let
  [results
   (for
    [n (range 1 7)]
    (let
     [G (reel/symmetric-group n) e (reel/id G)]
     (every?
      (fn
       [sigma]
       (let
        [si (reel/inv G sigma)]
        (and (= (reel/op G sigma si) e) (= (reel/op G si sigma) e))))
      (reel/elements G))))]
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
      (reel/symmetric-group n)
      id-perm
      (reel/id G)
      make-swap
      (fn [i] (let [v (vec (range n))] (assoc v i (inc i) (inc i) i)))]
     (every?
      (fn
       [sigma]
       (let
        [swaps
         (reel/adjacent-transposition-decomposition sigma)
         reconstructed
         (reduce (fn [p i] (reel/op G p (make-swap i))) id-perm swaps)]
        (= sigma reconstructed)))
      (reel/elements G))))]
  (every? true? results)))


(deftest t10_l76 (is (true? v9_l61)))


(def
 v12_l83
 (let
  [results
   (for
    [n (range 3 7)]
    (let
     [G (reel/symmetric-group n) classes (reel/conjugacy-classes G)]
     (every?
      (fn
       [cls]
       (= 1 (count (set (map reel/cycle-type (:elements cls))))))
      classes)))]
  (every? true? results)))


(deftest t13_l92 (is (true? v12_l83)))


(def
 v15_l99
 (let
  [G (reel/symmetric-group 4) elts (vec (reel/elements G))]
  (every?
   (fn
    [[a b c]]
    (= (reel/op G (reel/op G a b) c) (reel/op G a (reel/op G b c))))
   (for [a elts b elts c elts] [a b c]))))


(deftest t16_l106 (is (true? v15_l99)))


(def
 v18_l113
 (defn
  perm-order
  "Compute the order of a permutation by repeated composition."
  [G sigma]
  (let
   [e (reel/id G)]
   (loop
    [k 1 current sigma]
    (if (= current e) k (recur (inc k) (reel/op G current sigma)))))))


(def
 v20_l125
 (let
  [G (reel/symmetric-group 5)]
  (every?
   (fn
    [sigma]
    (let
     [ct
      (reel/cycle-type sigma)
      expected
      (reduce
       (fn
        [a b]
        (/ (* a b) (biginteger (.gcd (biginteger a) (biginteger b)))))
       (map biginteger ct))]
     (= (perm-order G sigma) (long expected))))
   (reel/elements G))))


(deftest t21_l133 (is (true? v20_l125)))


(def v23_l143 (reel/partitions 1))


(deftest t24_l145 (is (= v23_l143 [[1]])))


(def v25_l147 (reel/partitions 4))


(deftest t26_l149 (is (= v25_l147 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v27_l151 (reel/partitions 5))


(deftest
 t28_l153
 (is
  (= v27_l151 [[5] [4 1] [3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]])))


(def
 v30_l157
 (kind/table
  {:column-names ["$n$" "$p(n)$"],
   :row-vectors
   (mapv (fn [n] [n (count (reel/partitions n))]) (range 1 16))}))


(def
 v32_l163
 (let
  [results
   (for
    [n (range 1 13)]
    (every?
     (fn [p] (and (every? pos-int? p) (apply >= p) (= n (reduce + p))))
     (reel/partitions n)))]
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
    [p (reel/partitions 5)]
    [:div
     {:style "text-align: center;"}
     (reel/young-diagram-svg p)
     [:div
      {:style
       "margin-top: 4px; font-family: monospace; font-size: 13px;"}
      (str p)]]))))
