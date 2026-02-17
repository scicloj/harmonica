(ns
 reel-book.groups-and-structure-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l35
 (let
  [G (reel/dihedral-group 4)]
  {:order (reel/order G),
   :identity (reel/id G),
   :elements (vec (reel/elements G))}))


(def v5_l42 (let [G (reel/dihedral-group 5)] (reel/op G [:r 2] [:s 0])))


(def
 v7_l55
 (let
  [V4 (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 2))]
  {:order (reel/order V4), :elements (vec (reel/elements V4))}))


(def
 v9_l64
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
 v11_l90
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


(deftest t12_l101 (is (true? v11_l90)))


(def
 v14_l107
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


(deftest t15_l119 (is (true? v14_l107)))


(def
 v17_l127
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


(deftest t18_l146 (is (true? v17_l127)))


(def
 v20_l152
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


(deftest t21_l164 (is (true? v20_l152)))


(def
 v23_l168
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


(deftest t24_l178 (is (true? v23_l168)))


(def
 v26_l184
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     {:group label,
      :pass? (= (reel/order group) (count (reel/elements group)))})
    all-groups)]
  (every? :pass? results)))


(deftest t27_l191 (is (true? v26_l184)))


(def
 v29_l195
 (kind/table
  {:column-names ["Group" "Order"],
   :row-vectors
   (mapv
    (fn [{:keys [label group]}] [label (reel/order group)])
    all-groups)}))


(def
 v31_l205
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


(deftest t32_l216 (is (true? v31_l205)))


(def
 v34_l220
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


(deftest t35_l229 (is (true? v34_l220)))


(def
 v37_l233
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


(deftest t38_l243 (is (true? v37_l233)))


(def
 v40_l250
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


(deftest t41_l266 (is (true? v40_l250)))


(def
 v43_l272
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


(deftest t44_l285 (is (true? v43_l272)))


(def
 v46_l291
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


(deftest t47_l301 (is (true? v46_l291)))


(def
 v49_l305
 (let
  [results
   (for
    [n (range 1 25)]
    (= (reel/order (reel/dihedral-group n)) (* 2 n)))]
  (every? true? results)))


(deftest t50_l310 (is (true? v49_l305)))


(def
 v52_l316
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


(deftest t53_l326 (is (true? v52_l316)))


(def
 v55_l330
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


(deftest t56_l341 (is (true? v55_l330)))


(def
 v58_l348
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


(deftest t59_l361 (is (true? v58_l348)))


(def
 v61_l365
 (let
  [G (reel/symmetric-group 3) a [1 0 2] b [0 2 1]]
  (not= (reel/op G a b) (reel/op G b a))))


(deftest t62_l370 (is (true? v61_l365)))


(def
 v63_l372
 (let
  [G (reel/dihedral-group 3)]
  (not= (reel/op G [:r 1] [:s 0]) (reel/op G [:s 0] [:r 1]))))


(deftest t64_l375 (is (true? v63_l372)))


(def
 v66_l386
 (kind/hiccup (reel/cayley-table-svg (reel/cyclic-group 4))))
