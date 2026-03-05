(ns
 harmonica-book.riffle-shuffle-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [fastmath.matrix :as fm]
  [clojure.test :refer [deftest is]]))


(def v3_l41 (hm/standard-young-tableaux [3 1]))


(def v5_l45 (hm/hook-length-dimension [3 1]))


(deftest t6_l47 (is (= v5_l45 3)))


(def v8_l51 (count (hm/standard-young-tableaux [3 1])))


(deftest t9_l53 (is (= v8_l51 3)))


(def
 v11_l58
 (let
  [ir (hm/irrep [3 1]) gens (hm/rep-generators ir)]
  (kind/table
   {:column-names ["Generator" "Matrix"],
    :row-vectors
    (mapv (fn [i g] [(str "$s_" (inc i) "$") (str g)]) (range) gens)})))


(def
 v13_l74
 (let
  [n 5 parts (hm/partitions n)]
  (kind/table
   {:column-names ["λ" "dₗ" "dₗ²"],
    :row-vectors
    (conj
     (mapv
      (fn
       [lam]
       (let [d (hm/hook-length-dimension lam)] [(str lam) d (* d d)]))
      parts)
     ["**Total**"
      ""
      (reduce
       +
       (map
        (fn*
         [p1__76479#]
         (let [d (hm/hook-length-dimension p1__76479#)] (* d d)))
        parts))])})))


(deftest
 t14_l87
 (is
  ((fn
    [_]
    (let
     [parts (hm/partitions 5)]
     (=
      120
      (reduce
       +
       (map
        (fn*
         [p1__76480#]
         (let [d (hm/hook-length-dimension p1__76480#)] (* d d)))
        parts)))))
   v13_l74)))


(def v16_l112 (hm/rising-sequences [2 0 3 1]))


(deftest t17_l114 (is (= v16_l112 2)))


(def v19_l118 (hm/rising-sequences (hm/identity-perm 6)))


(deftest t20_l120 (is (= v19_l118 1)))


(def v22_l125 (hm/gsr-probability (hm/identity-perm 4) 1))


(def
 v24_l129
 (let
  [G
   (hm/symmetric-group 4)
   elts
   (vec (hm/elements G))
   probs
   (hm/gsr-distribution-vec elts 2)]
  (<
   (Math/abs
    (-
     (reduce
      +
      (map (fn* [p1__76481#] (probs p1__76481#)) (range (count elts))))
     1.0))
   1.0E-10)))


(deftest t25_l134 (is (true? v24_l129)))


(def
 v27_l143
 (let
  [n
   5
   G
   (hm/symmetric-group n)
   elts
   (vec (hm/elements G))
   n-elts
   (count elts)
   uniform
   (/ 1.0 n-elts)
   tv-data
   (mapv
    (fn
     [k]
     (let
      [probs
       (hm/gsr-distribution-vec elts k)
       tv
       (*
        0.5
        (reduce
         +
         (map
          (fn* [p1__76482#] (Math/abs (- (probs p1__76482#) uniform)))
          (range n-elts))))]
      {:k k, :tv tv}))
    (range 1 15))]
  (->
   (tc/dataset tv-data)
   (plotly/base {:=x :k, :=y :tv})
   (plotly/layer-line {:=mark-color "steelblue"})
   (plotly/layer-point {:=mark-color "steelblue", :=mark-size 8})
   (plotly/update-data
    (fn
     [d]
     (assoc
      d
      :=layout
      {:title "TV distance from uniform after k riffle shuffles (S₅)",
       :xaxis {:title "k (number of shuffles)"},
       :yaxis {:title "Total variation distance", :range [0 1.05]}})))
   plotly/plot)))


(def
 v29_l168
 (let
  [n
   5
   G
   (hm/symmetric-group n)
   elts
   (vec (hm/elements G))
   n-elts
   (count elts)
   uniform
   (/ 1.0 n-elts)
   probs
   (hm/gsr-distribution-vec elts 1)
   tv
   (*
    0.5
    (reduce
     +
     (map
      (fn* [p1__76483#] (Math/abs (- (probs p1__76483#) uniform)))
      (range n-elts))))]
  (> tv 0.5)))


(deftest t30_l177 (is (true? v29_l168)))


(def
 v32_l181
 (let
  [n
   5
   G
   (hm/symmetric-group n)
   elts
   (vec (hm/elements G))
   n-elts
   (count elts)
   uniform
   (/ 1.0 n-elts)
   probs
   (hm/gsr-distribution-vec elts 14)
   tv
   (*
    0.5
    (reduce
     +
     (map
      (fn* [p1__76484#] (Math/abs (- (probs p1__76484#) uniform)))
      (range n-elts))))]
  (< tv 0.01)))


(deftest t33_l190 (is (true? v32_l181)))


(def
 v35_l200
 (let
  [n
   4
   G
   (hm/symmetric-group n)
   parts
   (hm/partitions n)
   irreps
   (mapv hm/irrep parts)
   k
   2
   f
   (fn [sigma] (hm/gsr-probability sigma k))
   f-hats
   (hm/matrix-fourier-transform-all G f irreps)]
  (kind/table
   {:column-names ["λ" "dₗ" "‖Q̂(ρₗ)‖"],
    :row-vectors
    (mapv
     (fn
      [lam ir fh]
      [(str lam)
       (:dimension ir)
       (format "%.6f" (hm/frobenius-norm fh))])
     parts
     irreps
     f-hats)})))


(def
 v37_l222
 (let
  [n
   4
   G
   (hm/symmetric-group n)
   parts
   (hm/partitions n)
   irreps
   (mapv hm/irrep parts)
   f
   (fn [sigma] (hm/gsr-probability sigma 2))
   f-hats
   (hm/matrix-fourier-transform-all G f irreps)
   lhs
   (reduce
    +
    (map (fn [sigma] (let [v (f sigma)] (* v v))) (hm/elements G)))
   rhs
   (*
    (/ 1.0 (hm/order G))
    (reduce
     +
     (map
      (fn
       [ir f-hat]
       (*
        (double (hm/rep-dimension ir))
        (fm/trace (fm/mulm (fm/transpose f-hat) f-hat))))
      irreps
      f-hats)))]
  {:lhs lhs, :rhs rhs, :difference (Math/abs (- lhs rhs))}))


(deftest
 t38_l237
 (is ((fn [result] (< (:difference result) 1.0E-10)) v37_l222)))


(def
 v40_l245
 (let
  [tv-data
   (vec
    (for
     [n [4 5 6] k (range 1 15)]
     (let
      [G
       (hm/symmetric-group n)
       elts
       (vec (hm/elements G))
       n-elts
       (count elts)
       uniform
       (/ 1.0 n-elts)
       probs
       (hm/gsr-distribution-vec elts k)
       tv
       (*
        0.5
        (reduce
         +
         (map
          (fn* [p1__76485#] (Math/abs (- (probs p1__76485#) uniform)))
          (range n-elts))))]
      {:k k, :tv tv, :n (str "n=" n)})))]
  (->
   (tc/dataset tv-data)
   (plotly/base {:=x :k, :=y :tv, :=color :n})
   (plotly/layer-line)
   (plotly/layer-point {:=mark-size 6})
   (plotly/update-data
    (fn
     [d]
     (assoc
      d
      :=layout
      {:title "Riffle shuffle cutoff phenomenon",
       :xaxis {:title "k (number of shuffles)"},
       :yaxis {:title "Total variation distance", :range [0 1.05]}})))
   plotly/plot)))
