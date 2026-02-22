(ns
 harmonica-book.counting-necklaces-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l25
 (defn rotation-action [n] (fn [g x] (mod (+ (long x) (long g)) n))))


(def
 v4_l28
 (defn
  dihedral-vertex-action
  [n]
  (fn
   [[t k] x]
   (case
    t
    :r
    (mod (+ (long x) (long k)) n)
    :s
    (mod (- (long k) (long x)) n)))))


(def
 v6_l48
 (let
  [n
   4
   G
   (hm/cyclic-group n)
   {:keys [domain act]}
   (hm/coloring-action (rotation-action n) n 2)
   orbs
   (hm/orbits G act domain)]
  (kind/table
   {:column-names ["Orbit #" "Size" "Representative"],
    :row-vectors
    (mapv
     (fn [i orb] [(inc i) (count orb) (str (first (sort orb)))])
     (range)
     (sort-by (fn* [p1__73530#] (first (sort p1__73530#))) orbs))})))


(def
 v8_l60
 (let
  [n
   4
   G
   (hm/cyclic-group n)
   {:keys [domain act]}
   (hm/coloring-action (rotation-action n) n 2)
   orbs
   (hm/orbits G act domain)]
  (count orbs)))


(deftest t9_l66 (is (= v8_l60 6)))


(def
 v11_l77
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   {:keys [domain act]}
   (hm/coloring-action (rotation-action n) n 2)
   fix-counts
   (mapv
    (fn
     [g]
     {:element g, :fixed (count (hm/fixed-points act g domain))})
    (hm/elements G))]
  (kind/table
   {:column-names ["$g$" "$|\\text{Fix}(g)|$"],
    :row-vectors
    (mapv
     (fn [{:keys [element fixed]}] [(str element) fixed])
     fix-counts)})))


(def
 v13_l92
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   {:keys [domain act]}
   (hm/coloring-action (rotation-action n) n 2)]
  (hm/burnside-count G act domain)))


(deftest t14_l97 (is (= v13_l92 14)))


(def
 v16_l106
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   ci
   (hm/cycle-index G (rotation-action n) (range n))]
  (kind/table
   {:column-names ["Cycle type" "Coefficient"],
    :row-vectors
    (mapv
     (fn [[ct coeff]] [(str ct) (str coeff)])
     (sort-by (comp count first) ci))})))


(def
 v18_l118
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   ci
   (hm/cycle-index G (rotation-action n) (range n))]
  (reduce + (vals ci))))


(deftest t19_l123 (is (= v18_l118 1)))


(def
 v21_l128
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   ci
   (hm/cycle-index G (rotation-action n) (range n))]
  (kind/table
   {:column-names ["$k$ (colors)" "Necklaces"],
    :row-vectors
    (mapv (fn [k] [k (hm/polya-count ci k)]) (range 2 8))})))


(def
 v23_l138
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   ci
   (hm/cycle-index G (rotation-action n) (range n))]
  (hm/polya-count ci 2)))


(deftest t24_l143 (is (= v23_l138 14)))


(def
 v25_l145
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   ci
   (hm/cycle-index G (rotation-action n) (range n))]
  (hm/polya-count ci 3)))


(deftest t26_l150 (is (= v25_l145 130)))


(def
 v28_l157
 (let
  [results
   (mapv
    (fn
     [n]
     (let
      [G-c
       (hm/cyclic-group n)
       G-d
       (hm/dihedral-group n)
       {domain-c :domain, act-c :act}
       (hm/coloring-action (rotation-action n) n 2)
       {domain-d :domain, act-d :act}
       (hm/coloring-action (dihedral-vertex-action n) n 2)
       necklaces
       (hm/burnside-count G-c act-c domain-c)
       bracelets
       (hm/burnside-count G-d act-d domain-d)]
      {:n n, :necklaces necklaces, :bracelets bracelets}))
    (range 3 10))]
  (kind/table
   {:column-names ["$n$" "Necklaces ($C_n$)" "Bracelets ($D_n$)"],
    :row-vectors
    (mapv
     (fn [{:keys [n necklaces bracelets]}] [n necklaces bracelets])
     results)})))


(def
 v30_l175
 (let
  [n
   6
   G-d
   (hm/dihedral-group n)
   {domain-d :domain, act-d :act}
   (hm/coloring-action (dihedral-vertex-action n) n 2)]
  (hm/burnside-count G-d act-d domain-d)))


(deftest t31_l180 (is (= v30_l175 13)))


(def
 v33_l191
 (let
  [data
   (vec
    (for
     [n (range 3 21) group-type [:cyclic :dihedral]]
     (let
      [G
       (if
        (= group-type :cyclic)
        (hm/cyclic-group n)
        (hm/dihedral-group n))
       point-act
       (if
        (= group-type :cyclic)
        (rotation-action n)
        (dihedral-vertex-action n))
       ci
       (hm/cycle-index G point-act (range n))]
      {:n n,
       :count (long (hm/polya-count ci 2)),
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
 v35_l224
 (let
  [G
   (hm/symmetric-group 4)
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
     (fn [k] [k (hm/polya-count cube-cycle-index k)])
     (range 1 8))})))


(def
 v36_l246
 (let
  [cube-cycle-index
   {[1 1 1 1 1 1] 1/24,
    [1 1 4] 1/4,
    [1 1 2 2] 1/8,
    [3 3] 1/3,
    [2 2 2] 1/4}]
  [(hm/polya-count cube-cycle-index 1)
   (hm/polya-count cube-cycle-index 2)
   (hm/polya-count cube-cycle-index 3)
   (hm/polya-count cube-cycle-index 6)]))


(deftest t37_l256 (is (= v36_l246 [1 10 57 2226])))
