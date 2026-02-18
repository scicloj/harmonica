(ns
 harmonica-book.riffle-shuffle-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.representations :as rep]
  [scicloj.harmonica.protocols :as p]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [fastmath.matrix :as fm]
  [clojure.test :refer [deftest is]]))


(def v3_l43 (hm/standard-young-tableaux [3 1]))


(def v5_l47 (hm/hook-length-dimension [3 1]))


(def
 v7_l52
 (let
  [ir (hm/irrep [3 1]) gens (hm/rep-generators ir)]
  (kind/table
   {:column-names ["Generator" "Matrix"],
    :row-vectors
    (mapv (fn [i g] [(str "$s_" (inc i) "$") (str g)]) (range) gens)})))


(def
 v9_l68
 (let
  [n 5 parts (hm/partitions n)]
  (kind/table
   {:column-names ["$\\lambda$" "$d_\\lambda$" "$d_\\lambda^2$"],
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
         [p1__76213#]
         (let [d (hm/hook-length-dimension p1__76213#)] (* d d)))
        parts))])})))


(deftest
 t10_l81
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
         [p1__76214#]
         (let [d (hm/hook-length-dimension p1__76214#)] (* d d)))
        parts)))))
   v9_l68)))


(def v12_l106 (hm/rising-sequences [2 0 3 1]))


(deftest t13_l108 (is (= v12_l106 2)))


(def v15_l113 (hm/gsr-probability (hm/identity-perm 4) 1))


(def
 v17_l122
 (let
  [n
   5
   G
   (hm/symmetric-group n)
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
       (hm/gsr-distribution-vec elts k)
       tv
       (*
        0.5
        (reduce
         +
         (map
          (fn*
           [p1__76215#]
           (Math/abs (- (aget probs (int p1__76215#)) uniform)))
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
 v19_l153
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
   {:column-names
    ["$\\lambda$" "$d_\\lambda$" "$\\|\\hat{Q}(\\rho_\\lambda)\\|_F$"],
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
 v21_l175
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
   (rep/plancherel-lhs G f)
   rhs
   (rep/plancherel-rhs G f-hats irreps)]
  {:lhs lhs, :rhs rhs, :difference (Math/abs (- lhs rhs))}))


(deftest
 t22_l185
 (is ((fn [result] (< (:difference result) 1.0E-10)) v21_l175)))


(def
 v24_l193
 (let
  [tv-data
   (vec
    (for
     [n [4 5 6] k (range 1 15)]
     (let
      [G
       (hm/symmetric-group n)
       elts
       (vec (p/elements G))
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
          (fn*
           [p1__76216#]
           (Math/abs (- (aget probs (int p1__76216#)) uniform)))
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
