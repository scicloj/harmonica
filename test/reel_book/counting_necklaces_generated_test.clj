(ns
 reel-book.counting-necklaces-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.reel.protocols :as p]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l32
 (let
  [n
   4
   k
   2
   domain
   (for [a (range k) b (range k) c (range k) d (range k)] [a b c d])
   G
   (reel/cyclic-group n)
   act
   (fn
    [g coloring]
    (mapv
     (fn* [p1__82774#] (coloring (mod (+ p1__82774# (long g)) n)))
     (range n)))
   orbs
   (reel/orbits G act domain)]
  (kind/table
   {:column-names ["Orbit #" "Size" "Representative"],
    :row-vectors
    (mapv
     (fn [i orb] [(inc i) (count orb) (str (first (sort orb)))])
     (range)
     (sort-by (fn* [p1__82775#] (first (sort p1__82775#))) orbs))})))


(def
 v5_l57
 (let
  [n
   6
   k
   2
   domain
   (let
    [bits (range k)]
    (for [a bits b bits c bits d bits e bits f bits] [a b c d e f]))
   G
   (reel/cyclic-group n)
   act
   (fn
    [g coloring]
    (mapv
     (fn* [p1__82776#] (coloring (mod (+ p1__82776# (long g)) n)))
     (range n)))
   fix-counts
   (mapv
    (fn
     [g]
     {:element g, :fixed (count (reel/fixed-points act g domain))})
    (p/elements G))]
  (kind/table
   {:column-names ["$g$" "$|\\text{Fix}(g)|$"],
    :row-vectors
    (mapv
     (fn [{:keys [element fixed]}] [(str element) fixed])
     fix-counts)})))


(def
 v7_l77
 (let
  [n
   6
   k
   2
   domain
   (let
    [bits (range k)]
    (for [a bits b bits c bits d bits e bits f bits] [a b c d e f]))
   G
   (reel/cyclic-group n)
   act
   (fn
    [g coloring]
    (mapv
     (fn* [p1__82777#] (coloring (mod (+ p1__82777# (long g)) n)))
     (range n)))]
  (reel/burnside-count G act domain)))


(deftest t8_l87 (is (= v7_l77 14)))


(def
 v10_l96
 (let
  [n
   6
   G
   (reel/cyclic-group n)
   act
   (fn [g x] (mod (+ (long x) (long g)) n))
   ci
   (reel/cycle-index G act (range n))]
  (kind/table
   {:column-names ["Cycle type" "Coefficient"],
    :row-vectors
    (mapv
     (fn [[ct coeff]] [(str ct) (str coeff)])
     (sort-by (comp count first) ci))})))


(def
 v12_l109
 (let
  [n
   6
   G
   (reel/cyclic-group n)
   act
   (fn [g x] (mod (+ (long x) (long g)) n))
   ci
   (reel/cycle-index G act (range n))]
  (kind/table
   {:column-names ["$k$ (colors)" "Necklaces"],
    :row-vectors
    (mapv (fn [k] [k (reel/polya-count ci k)]) (range 2 8))})))


(def
 v14_l123
 (let
  [results
   (mapv
    (fn
     [n]
     (let
      [G-c
       (reel/cyclic-group n)
       G-d
       (reel/dihedral-group n)
       act-c
       (fn
        [g coloring]
        (mapv
         (fn* [p1__82778#] (coloring (mod (+ p1__82778# (long g)) n)))
         (range n)))
       act-d
       (fn
        [[t k] coloring]
        (case
         t
         :r
         (mapv
          (fn* [p1__82779#] (coloring (mod (+ p1__82779# (long k)) n)))
          (range n))
         :s
         (mapv
          (fn* [p1__82780#] (coloring (mod (- (long k) p1__82780#) n)))
          (range n))))
       domain
       (let
        [bits (range 2)]
        (loop
         [i 0 d [[]]]
         (if
          (= i n)
          d
          (recur (inc i) (for [prev d c bits] (conj prev c))))))
       necklaces
       (reel/burnside-count G-c act-c domain)
       bracelets
       (reel/burnside-count G-d act-d domain)]
      {:n n, :necklaces necklaces, :bracelets bracelets}))
    (range 3 10))]
  (kind/table
   {:column-names ["$n$" "Necklaces ($C_n$)" "Bracelets ($D_n$)"],
    :row-vectors
    (mapv
     (fn [{:keys [n necklaces bracelets]}] [n necklaces bracelets])
     results)})))


(def
 v16_l156
 (let
  [data
   (vec
    (for
     [n (range 3 21) group-type [:cyclic :dihedral]]
     (let
      [G
       (if
        (= group-type :cyclic)
        (reel/cyclic-group n)
        (reel/dihedral-group n))
       act
       (if
        (= group-type :cyclic)
        (fn [g x] (mod (+ (long x) (long g)) n))
        (fn
         [[t k] x]
         (case
          t
          :r
          (mod (+ (long x) (long k)) n)
          :s
          (mod (- (long k) (long x)) n))))
       ci
       (reel/cycle-index G act (range n))]
      {:n n,
       :count (long (reel/polya-count ci 2)),
       :type (if (= group-type :cyclic) "Necklaces" "Bracelets")})))]
  (->
   (tc/dataset data)
   (plotly/base {:=x :n, :=y :count, :=color :type})
   (plotly/layer-line)
   (plotly/layer-point {:=mark-size 6})
   (plotly/update-data
    (fn
     [d]
     (assoc
      d
      :=layout
      {:title "Binary necklaces and bracelets",
       :xaxis {:title "n (number of beads)"},
       :yaxis {:title "Count", :type "log"}})))
   plotly/plot)))


(def
 v18_l192
 (let
  [G
   (reel/symmetric-group 4)
   cube-cycle-index
   {[1 1 1 1 1 1] 1/24,
    [1 1 4] 1/4,
    [1 1 2 2] 1/8,
    [3 3] 1/3,
    [2 2 2] 1/4}]
  (kind/table
   {:column-names ["$k$ (colors)" "Distinct cube colorings"],
    :row-vectors
    (mapv
     (fn [k] [k (reel/polya-count cube-cycle-index k)])
     (range 1 8))})))
