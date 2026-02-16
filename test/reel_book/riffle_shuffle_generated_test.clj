(ns
 reel-book.riffle-shuffle-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.reel.impl.partition :as part]
  [scicloj.reel.impl.permutation :as perm]
  [scicloj.reel.impl.riffle :as riffle]
  [scicloj.reel.representations :as rep]
  [scicloj.reel.protocols :as p]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [fastmath.matrix :as fm]
  [clojure.test :refer [deftest is]]))


(def v3_l46 (reel/standard-young-tableaux [3 1]))


(def v5_l50 (reel/hook-length-dimension [3 1]))


(def
 v7_l55
 (let
  [ir (reel/irrep [3 1]) gens (reel/rep-generators ir)]
  (kind/table
   {:column-names ["Generator" "Matrix"],
    :row-vectors
    (mapv (fn [i g] [(str "$s_" (inc i) "$") (str g)]) (range) gens)})))


(def
 v9_l71
 (let
  [n 5 parts (part/partitions n)]
  (kind/table
   {:column-names ["$\\lambda$" "$d_\\lambda$" "$d_\\lambda^2$"],
    :row-vectors
    (conj
     (mapv
      (fn
       [lam]
       (let
        [d (reel/hook-length-dimension lam)]
        [(str lam) d (* d d)]))
      parts)
     ["**Total**"
      ""
      (reduce
       +
       (map
        (fn*
         [p1__63249#]
         (let [d (reel/hook-length-dimension p1__63249#)] (* d d)))
        parts))])})))


(deftest
 t10_l84
 (is
  (fn
   v9_l71
   [_]
   (let
    [parts (part/partitions 5)]
    (=
     120
     (reduce
      +
      (map
       (fn*
        [p1__63250#]
        (let [d (reel/hook-length-dimension p1__63250#)] (* d d)))
       parts)))))))


(def v12_l110 (reel/rising-sequences [2 0 3 1]))


(deftest t13_l112 (is (fn v12_l110 [v] (= 3 v))))


(def v15_l117 (reel/gsr-probability (perm/identity-perm 4) 1))


(def
 v17_l126
 (let
  [n
   5
   G
   (reel/symmetric-group n)
   elts
   (vec (p/elements G))
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
       (reel/gsr-distribution-vec elts k)
       tv
       (*
        0.5
        (reduce
         +
         (map
          (fn*
           [p1__63251#]
           (Math/abs (- (aget probs (int p1__63251#)) uniform)))
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
 v19_l157
 (let
  [n
   4
   G
   (reel/symmetric-group n)
   parts
   (part/partitions n)
   irreps
   (mapv reel/irrep parts)
   k
   2
   f
   (fn [sigma] (reel/gsr-probability sigma k))
   f-hats
   (reel/matrix-fourier-transform-all G f irreps)]
  (kind/table
   {:column-names
    ["$\\lambda$" "$d_\\lambda$" "$\\|\\hat{Q}(\\rho_\\lambda)\\|_F$"],
    :row-vectors
    (mapv
     (fn
      [lam ir fh]
      [(str lam)
       (:dimension ir)
       (format "%.6f" (reel/frobenius-norm fh))])
     parts
     irreps
     f-hats)})))


(def
 v21_l179
 (let
  [n
   4
   G
   (reel/symmetric-group n)
   parts
   (part/partitions n)
   irreps
   (mapv reel/irrep parts)
   f
   (fn [sigma] (reel/gsr-probability sigma 2))
   f-hats
   (reel/matrix-fourier-transform-all G f irreps)
   lhs
   (rep/plancherel-lhs G f)
   rhs
   (rep/plancherel-rhs G f-hats irreps)]
  {:lhs lhs, :rhs rhs, :difference (Math/abs (- lhs rhs))}))


(deftest
 t22_l189
 (is (fn v21_l179 [result] (< (:difference result) 1.0E-10))))


(def
 v24_l197
 (let
  [tv-data
   (vec
    (for
     [n [4 5 6] k (range 1 15)]
     (let
      [G
       (reel/symmetric-group n)
       elts
       (vec (p/elements G))
       n-elts
       (count elts)
       uniform
       (/ 1.0 n-elts)
       probs
       (reel/gsr-distribution-vec elts k)
       tv
       (*
        0.5
        (reduce
         +
         (map
          (fn*
           [p1__63252#]
           (Math/abs (- (aget probs (int p1__63252#)) uniform)))
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
