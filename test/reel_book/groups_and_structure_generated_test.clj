(ns
 reel-book.groups-and-structure-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l18
 (def
  all-groups
  [{:label "Z/1Z", :group (reel/cyclic-group 1)}
   {:label "Z/2Z", :group (reel/cyclic-group 2)}
   {:label "Z/3Z", :group (reel/cyclic-group 3)}
   {:label "Z/7Z", :group (reel/cyclic-group 7)}
   {:label "Z/12Z", :group (reel/cyclic-group 12)}
   {:label "S_1", :group (reel/symmetric-group 1)}
   {:label "S_2", :group (reel/symmetric-group 2)}
   {:label "S_3", :group (reel/symmetric-group 3)}
   {:label "S_4", :group (reel/symmetric-group 4)}
   {:label "S_5", :group (reel/symmetric-group 5)}
   {:label "D_3", :group (reel/dihedral-group 3)}
   {:label "D_4", :group (reel/dihedral-group 4)}
   {:label "D_5", :group (reel/dihedral-group 5)}
   {:label "D_6", :group (reel/dihedral-group 6)}
   {:label "D_8", :group (reel/dihedral-group 8)}
   {:label "D_12", :group (reel/dihedral-group 12)}
   {:label "Z/2Z × Z/2Z",
    :group
    (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 2))}
   {:label "Z/2Z × Z/3Z",
    :group
    (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 3))}
   {:label "Z/3Z × Z/4Z",
    :group
    (reel/product-group (reel/cyclic-group 3) (reel/cyclic-group 4))}
   {:label "D_3 × Z/2Z",
    :group
    (reel/product-group
     (reel/dihedral-group 3)
     (reel/cyclic-group 2))}]))


(def
 v5_l44
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [e (reel/id group)]
      {:group label,
       :pass?
       (every?
        (fn
         [g]
         (and (= (reel/op group e g) g) (= (reel/op group g e) g)))
        (reel/elements group))}))
    all-groups)]
  (every? :pass? results)))


(deftest t6_l55 (is (true? v5_l44)))


(def
 v8_l61
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [e (reel/id group)]
      {:group label,
       :pass?
       (every?
        (fn
         [g]
         (let
          [gi (reel/inv group g)]
          (and (= (reel/op group g gi) e) (= (reel/op group gi g) e))))
        (reel/elements group))}))
    all-groups)]
  (every? :pass? results)))


(deftest t9_l73 (is (true? v8_l61)))


(def
 v11_l81
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [elts
       (vec (reel/elements group))
       n
       (count elts)
       triples
       (if
        (<= n 24)
        (for [a elts b elts c elts] [a b c])
        (let
         [rng (java.util.Random. 42)]
         (repeatedly
          1000
          (fn
           []
           [(elts (.nextInt rng n))
            (elts (.nextInt rng n))
            (elts (.nextInt rng n))]))))]
      {:group label,
       :pass?
       (every?
        (fn
         [[a b c]]
         (=
          (reel/op group (reel/op group a b) c)
          (reel/op group a (reel/op group b c))))
        triples)}))
    all-groups)]
  (every? :pass? results)))


(deftest t12_l100 (is (true? v11_l81)))


(def
 v14_l106
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [elt-set (set (reel/elements group))]
      {:group label,
       :pass?
       (every?
        (fn
         [g]
         (every?
          (fn [h] (contains? elt-set (reel/op group g h)))
          (reel/elements group)))
        (reel/elements group))}))
    all-groups)]
  (every? :pass? results)))


(deftest t15_l118 (is (true? v14_l106)))


(def
 v17_l122
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [elt-set (set (reel/elements group))]
      {:group label,
       :pass?
       (every?
        (fn [g] (contains? elt-set (reel/inv group g)))
        (reel/elements group))}))
    all-groups)]
  (every? :pass? results)))


(deftest t18_l132 (is (true? v17_l122)))


(def
 v20_l138
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     {:group label,
      :pass? (= (reel/order group) (count (reel/elements group)))})
    all-groups)]
  (every? :pass? results)))


(deftest t21_l145 (is (true? v20_l138)))


(def
 v23_l149
 (kind/table
  {:column-names ["Group" "Order"],
   :row-vectors
   (mapv
    (fn [{:keys [label group]}] [label (reel/order group)])
    all-groups)}))


(def
 v25_l159
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [classes
       (reel/conjugacy-classes group)
       all-elts
       (mapcat :elements classes)
       group-set
       (set (reel/elements group))]
      {:group label,
       :pass?
       (and
        (= (count all-elts) (count (set all-elts)))
        (= (set all-elts) group-set))}))
    all-groups)]
  (every? :pass? results)))


(deftest t26_l170 (is (true? v25_l159)))


(def
 v28_l174
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [classes (reel/conjugacy-classes group)]
      {:group label,
       :pass? (= (reel/order group) (reduce + (map :size classes)))}))
    all-groups)]
  (every? :pass? results)))


(deftest t29_l183 (is (true? v28_l174)))


(def
 v31_l187
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [classes (reel/conjugacy-classes group)]
      {:group label,
       :pass?
       (every? (fn [c] (= (:size c) (count (:elements c)))) classes)}))
    all-groups)]
  (every? :pass? results)))


(deftest t32_l197 (is (true? v31_l187)))


(def
 v34_l204
 (let
  [results
   (for
    [{:keys [label group]}
     all-groups
     :when
     (<= (reel/order group) 120)]
    (let
     [classes (reel/conjugacy-classes group)]
     (every?
      (fn
       [cls]
       (let
        [rep (:representative cls)]
        (every?
         (fn
          [h]
          (some
           (fn
            [x]
            (=
             h
             (reel/op group x (reel/op group rep (reel/inv group x)))))
           (reel/elements group)))
         (:elements cls))))
      classes)))]
  (every? true? results)))


(deftest t35_l220 (is (true? v34_l204)))


(def
 v37_l226
 (let
  [results
   (for
    [n (range 2 21)]
    (let
     [G
      (reel/dihedral-group n)
      e
      (reel/id G)
      r
      [:r 1]
      s
      [:s 0]
      r-n
      (reduce (fn [acc _] (reel/op G acc r)) e (range n))
      s-2
      (reel/op G s s)
      srs
      (reel/op G s (reel/op G r s))
      r-inv
      (reel/inv G r)]
     (and (= r-n e) (= s-2 e) (= srs r-inv))))]
  (every? true? results)))


(deftest t38_l239 (is (true? v37_l226)))


(def
 v40_l245
 (let
  [results
   (for
    [n (range 2 25)]
    (let
     [G
      (reel/dihedral-group n)
      actual
      (count (reel/conjugacy-classes G))
      expected
      (if (odd? n) (/ (+ n 3) 2) (+ (/ n 2) 3))]
     (= actual expected)))]
  (every? true? results)))


(deftest t41_l255 (is (true? v40_l245)))


(def
 v43_l259
 (let
  [results
   (for
    [n (range 1 25)]
    (= (reel/order (reel/dihedral-group n)) (* 2 n)))]
  (every? true? results)))


(deftest t44_l264 (is (true? v43_l259)))


(def
 v46_l270
 (let
  [results
   (for
    [[G1 G2]
     [[(reel/cyclic-group 2) (reel/cyclic-group 3)]
      [(reel/cyclic-group 4) (reel/cyclic-group 5)]
      [(reel/dihedral-group 3) (reel/cyclic-group 2)]
      [(reel/dihedral-group 4) (reel/dihedral-group 3)]
      [(reel/symmetric-group 3) (reel/cyclic-group 2)]]]
    (let
     [P (reel/product-group G1 G2)]
     (= (reel/order P) (* (reel/order G1) (reel/order G2)))))]
  (every? true? results)))


(deftest t47_l280 (is (true? v46_l270)))


(def
 v49_l284
 (let
  [results
   (for
    [[G1 G2]
     [[(reel/cyclic-group 3) (reel/cyclic-group 4)]
      [(reel/dihedral-group 3) (reel/cyclic-group 2)]
      [(reel/dihedral-group 4) (reel/dihedral-group 3)]
      [(reel/symmetric-group 3) (reel/cyclic-group 3)]]]
    (let
     [P (reel/product-group G1 G2)]
     (=
      (count (reel/conjugacy-classes P))
      (*
       (count (reel/conjugacy-classes G1))
       (count (reel/conjugacy-classes G2))))))]
  (every? true? results)))


(deftest t50_l295 (is (true? v49_l284)))


(def
 v52_l302
 (let
  [abelian-groups
   [(reel/cyclic-group 5)
    (reel/cyclic-group 12)
    (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 3))
    (reel/product-group (reel/cyclic-group 3) (reel/cyclic-group 4))]
   results
   (for
    [G abelian-groups]
    (every?
     (fn
      [g]
      (every?
       (fn [h] (= (reel/op G g h) (reel/op G h g)))
       (reel/elements G)))
     (reel/elements G)))]
  (every? true? results)))


(deftest t53_l315 (is (true? v52_l302)))


(def
 v55_l319
 (let
  [G (reel/symmetric-group 3) a [1 0 2] b [0 2 1]]
  (not= (reel/op G a b) (reel/op G b a))))


(deftest t56_l324 (is (true? v55_l319)))


(def
 v57_l326
 (let
  [G (reel/dihedral-group 3)]
  (not= (reel/op G [:r 1] [:s 0]) (reel/op G [:s 0] [:r 1]))))


(deftest t58_l329 (is (true? v57_l326)))


(def
 v60_l336
 (defn
  cayley-table-svg
  "Render a Cayley table as an SVG grid with colored cells."
  [G & {:keys [cell-size], :or {cell-size 28}}]
  (let
   [elts
    (vec (reel/elements G))
    n
    (count elts)
    elt-idx
    (into {} (map-indexed (fn [i e] [e i]) elts))
    colors
    (mapv
     (fn
      [i]
      (let
       [hue (* 360.0 (/ i (double n)))]
       (str "hsl(" (int hue) ",70%,75%)")))
     (range n))
    header
    cell-size
    w
    (+ header (* n cell-size) 2)
    h
    (+ header (* n cell-size) 2)]
   (into
    [:svg
     {:width w,
      :height h,
      :xmlns "http://www.w3.org/2000/svg",
      :style "font-family: monospace; font-size: 11px;"}]
    (concat
     (for
      [j (range n)]
      [:text
       {:x (+ header (* j cell-size) (/ cell-size 2)),
        :y (- header 4),
        :text-anchor "middle",
        :font-size 9,
        :fill "#555"}
       (str (elts j))])
     (for
      [i (range n)]
      [:text
       {:x (- header 4),
        :y (+ header (* i cell-size) (/ cell-size 2) 4),
        :text-anchor "end",
        :font-size 9,
        :fill "#555"}
       (str (elts i))])
     (for
      [i
       (range n)
       j
       (range n)
       :let
       [prod (reel/op G (elts i) (elts j)) k (elt-idx prod)]]
      [:rect
       {:x (+ header (* j cell-size)),
        :y (+ header (* i cell-size)),
        :width (dec cell-size),
        :height (dec cell-size),
        :fill (colors k),
        :stroke "#fff",
        :stroke-width 0.5}]))))))


(def v62_l376 (kind/hiccup (cayley-table-svg (reel/cyclic-group 4))))


(def v64_l380 (kind/hiccup (cayley-table-svg (reel/dihedral-group 3))))
