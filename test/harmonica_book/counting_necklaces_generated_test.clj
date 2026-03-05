(ns
 harmonica-book.counting-necklaces-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l25 (defn rotation-action [n] (fn [g x] (mod (+ x g) n))))


(def
 v4_l28
 (defn
  dihedral-vertex-action
  [n]
  (fn [[t k] x] (case t :r (mod (+ x k) n) :s (mod (- k x) n)))))


(def
 v6_l40
 (defn
  necklace-traces
  "Plotly traces for a single necklace centered at (cx, cy) with radius r.\n   coloring is a vector of color indices, e.g. [0 1 0 1]."
  [coloring cx cy r]
  (let
   [n
    (count coloring)
    bead-colors
    ["#e74c3c" "#3498db" "#2ecc71" "#f39c12" "#9b59b6" "#e67e22"]
    angles
    (mapv
     (fn [i] (- (* 2 Math/PI (/ (double i) n)) (/ Math/PI 2)))
     (range n))
    bxs
    (mapv (fn [a] (+ cx (* r (Math/cos a)))) angles)
    bys
    (mapv (fn [a] (+ cy (* r (Math/sin a)))) angles)]
   [{:type "scatter",
     :mode "lines",
     :x (conj (vec bxs) (first bxs)),
     :y (conj (vec bys) (first bys)),
     :line {:color "#bdc3c7", :width 1},
     :showlegend false,
     :hoverinfo "skip"}
    {:type "scatter",
     :mode "markers",
     :x (vec bxs),
     :y (vec bys),
     :marker
     {:size 16,
      :color
      (mapv
       (fn* [p1__93327#] (get bead-colors p1__93327# "#7f8c8d"))
       coloring),
      :line {:color "#2c3e50", :width 1}},
     :showlegend false,
     :hoverinfo "skip"}])))


(def
 v7_l62
 (defn
  necklaces-row
  "Draw necklaces in a single horizontal row."
  [colorings
   &
   {:keys [title bead-radius spacing width height],
    :or {bead-radius 0.35, spacing 1.2}}]
  (let
   [nc
    (count colorings)
    w
    (or width (max 250 (long (* nc spacing 85))))
    h
    (or height 120)
    traces
    (vec
     (mapcat
      (fn
       [i c]
       (necklace-traces c (* spacing (+ i 0.5)) 0 bead-radius))
      (range)
      colorings))]
   (kind/plotly
    {:data traces,
     :layout
     (cond->
      {:xaxis {:visible false, :scaleanchor "y"},
       :yaxis {:visible false},
       :width w,
       :height h,
       :margin {:t (if title 35 10), :b 10, :l 10, :r 10}}
      title
      (assoc :title title))}))))


(def
 v8_l80
 (defn
  necklaces-grid
  "Draw necklaces in a grid. `rows` is a seq of seqs of coloring vectors.\n   Each inner seq becomes one horizontal row."
  [rows
   &
   {:keys [title bead-radius spacing width height row-labels],
    :or {bead-radius 0.35, spacing 1.2}}]
  (let
   [max-cols
    (apply max (map count rows))
    n-rows
    (count rows)
    w
    (or
     width
     (max
      300
      (long (* (+ max-cols (if row-labels 1.5 0)) spacing 80))))
    h
    (or height (max 150 (long (* n-rows spacing 90))))
    label-offset
    (if row-labels 1.5 0)
    necklace-traces*
    (vec
     (for
      [[ri row]
       (map-indexed vector rows)
       [ci coloring]
       (map-indexed vector row)]
      (necklace-traces
       coloring
       (* spacing (+ ci 0.5 label-offset))
       (* spacing (- (dec n-rows) ri))
       bead-radius)))
    all-traces
    (vec (apply concat necklace-traces*))
    label-traces
    (when
     row-labels
     (mapv
      (fn
       [ri label]
       {:type "scatter",
        :mode "text",
        :x [(* spacing 0.6)],
        :y [(* spacing (- (dec n-rows) ri))],
        :text [label],
        :textfont {:size 11, :color "#2c3e50"},
        :showlegend false,
        :hoverinfo "skip"})
      (range)
      row-labels))]
   (kind/plotly
    {:data (vec (concat all-traces label-traces)),
     :layout
     (cond->
      {:xaxis {:visible false, :scaleanchor "y"},
       :yaxis {:visible false},
       :width w,
       :height h,
       :margin {:t (if title 35 10), :b 10, :l 10, :r 10}}
      title
      (assoc :title title))}))))


(def
 v10_l129
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
     (sort-by (fn* [p1__93328#] (first (sort p1__93328#))) orbs))})))


(def
 v12_l143
 (let
  [n
   4
   G
   (hm/cyclic-group n)
   {:keys [domain act]}
   (hm/coloring-action (rotation-action n) n 2)
   orbs
   (hm/orbits G act domain)
   sorted-orbs
   (sort-by (fn [orb] [(- (count orb)) (first (sort orb))]) orbs)
   rows
   (mapv (fn [orb] (vec (sort orb))) sorted-orbs)
   labels
   (mapv (fn [orb] (str "size " (count orb))) sorted-orbs)]
  (necklaces-grid
   rows
   :title
   "All 16 colorings, grouped by orbit"
   :row-labels
   labels
   :width
   550
   :height
   560)))


(def
 v14_l158
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


(deftest t15_l164 (is (= v14_l158 6)))


(def
 v17_l168
 (let
  [n
   4
   G
   (hm/cyclic-group n)
   {:keys [domain act]}
   (hm/coloring-action (rotation-action n) n 2)
   orbs
   (hm/orbits G act domain)
   reps
   (mapv
    (fn [orb] (first (sort orb)))
    (sort-by (fn* [p1__93329#] (first (sort p1__93329#))) orbs))]
  (necklaces-row reps :title "The 6 distinct 4-bead binary necklaces")))


(def
 v19_l186
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
   {:column-names ["g" "|Fix(g)|"],
    :row-vectors
    (mapv
     (fn [{:keys [element fixed]}] [(str element) fixed])
     fix-counts)})))


(def
 v21_l204
 (let
  [n
   4
   G
   (hm/cyclic-group n)
   {:keys [domain act]}
   (hm/coloring-action (rotation-action n) n 2)
   elts
   (sort (hm/elements G))
   rows
   (mapv (fn [g] (vec (sort (hm/fixed-points act g domain)))) elts)
   labels
   (mapv
    (fn
     [g]
     (let
      [fp (hm/fixed-points act g domain)]
      (str "g=" g " (" (count fp) ")")))
    elts)]
  (necklaces-grid
   rows
   :title
   "Fixed colorings by group element — Burnside's lemma"
   :row-labels
   labels
   :width
   600
   :height
   420)))


(def
 v23_l224
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   {:keys [domain act]}
   (hm/coloring-action (rotation-action n) n 2)]
  (hm/burnside-count G act domain)))


(deftest t24_l229 (is (= v23_l224 14)))


(def
 v26_l238
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
 v28_l250
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   ci
   (hm/cycle-index G (rotation-action n) (range n))]
  (reduce + (vals ci))))


(deftest t29_l255 (is (= v28_l250 1)))


(def
 v31_l260
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   ci
   (hm/cycle-index G (rotation-action n) (range n))]
  (kind/table
   {:column-names ["k (colors)" "Necklaces"],
    :row-vectors
    (mapv (fn [k] [k (hm/polya-count ci k)]) (range 2 8))})))


(def
 v33_l270
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   ci
   (hm/cycle-index G (rotation-action n) (range n))]
  (hm/polya-count ci 2)))


(deftest t34_l275 (is (= v33_l270 14)))


(def
 v35_l277
 (let
  [n
   6
   G
   (hm/cyclic-group n)
   ci
   (hm/cycle-index G (rotation-action n) (range n))]
  (hm/polya-count ci 3)))


(deftest t36_l282 (is (= v35_l277 130)))


(def
 v38_l289
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
   {:column-names ["n" "Necklaces (Cₙ)" "Bracelets (Dₙ)"],
    :row-vectors
    (mapv
     (fn [{:keys [n necklaces bracelets]}] [n necklaces bracelets])
     results)})))


(def
 v40_l307
 (let
  [n
   6
   G-d
   (hm/dihedral-group n)
   {domain-d :domain, act-d :act}
   (hm/coloring-action (dihedral-vertex-action n) n 2)]
  (hm/burnside-count G-d act-d domain-d)))


(deftest t41_l312 (is (= v40_l307 13)))


(def
 v43_l320
 (let
  [n
   6
   G-c
   (hm/cyclic-group n)
   G-d
   (hm/dihedral-group n)
   act-c
   (rotation-action n)
   act-d
   (dihedral-vertex-action n)
   {c-domain :domain, c-act :act}
   (hm/coloring-action act-c n 2)
   {d-domain :domain, d-act :act}
   (hm/coloring-action act-d n 2)
   c-orbs
   (hm/orbits G-c c-act c-domain)
   d-orbs
   (hm/orbits G-d d-act d-domain)
   c-reps
   (sort (mapv (fn [orb] (first (sort orb))) c-orbs))
   d-reps
   (sort (mapv (fn [orb] (first (sort orb))) d-orbs))]
  (kind/fragment
   [(necklaces-row c-reps :title (str "14 necklaces (C₆)") :width 900)
    (necklaces-row
     d-reps
     :title
     (str "13 bracelets (D₆) — the mirror pair merges")
     :width
     900)])))


(def
 v45_l342
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
 v47_l375
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
   {:column-names ["k (colors)" "Distinct cube colorings"],
    :row-vectors
    (mapv
     (fn [k] [k (hm/polya-count cube-cycle-index k)])
     (range 1 8))})))


(def
 v48_l397
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


(deftest t49_l407 (is (= v48_l397 [1 10 57 2226])))
