(ns
 harmonica-book.groups-and-structure-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l41 (def C12 (hm/cyclic-group 12)))


(def v4_l43 (hm/order C12))


(deftest t5_l45 (is (= v4_l43 12)))


(def v7_l49 (hm/id C12))


(deftest t8_l51 (is (= v7_l49 0)))


(def v10_l55 (hm/op C12 7 8))


(deftest t11_l57 (is (= v10_l55 3)))


(def v13_l61 (hm/inv C12 5))


(deftest t14_l63 (is (= v13_l61 7)))


(def v16_l82 (def D4 (hm/dihedral-group 4)))


(def v17_l84 (hm/order D4))


(deftest t18_l86 (is (= v17_l84 8)))


(def v20_l90 (hm/id D4))


(deftest t21_l92 (is (= v20_l90 [:r 0])))


(def v23_l96 (vec (hm/elements D4)))


(def v25_l100 (hm/op D4 [:r 1] [:r 2]))


(deftest t26_l102 (is (= v25_l100 [:r 3])))


(def v28_l106 (hm/op D4 [:r 1] [:s 0]))


(def v30_l110 (hm/op D4 [:s 0] [:s 0]))


(deftest t31_l112 (is (= v30_l110 [:r 0])))


(def v33_l120 (hm/op D4 [:r 1] [:s 0]))


(def v34_l122 (hm/op D4 [:s 0] [:r 1]))


(def
 v35_l124
 (let
  [a (hm/op D4 [:r 1] [:s 0]) b (hm/op D4 [:s 0] [:r 1])]
  (not= a b)))


(deftest t36_l128 (is (true? v35_l124)))


(def
 v38_l140
 (let
  [e
   (hm/id D4)
   r
   [:r 1]
   s
   [:s 0]
   r4
   (reduce (fn [acc _] (hm/op D4 acc r)) e (range 4))
   s2
   (hm/op D4 s s)
   srs
   (hm/op D4 s (hm/op D4 r s))
   r-inv
   (hm/inv D4 r)]
  {"r⁴ = e" (= r4 e), "s² = e" (= s2 e), "srs = r⁻¹" (= srs r-inv)}))


(deftest t39_l151 (is ((fn [m] (every? true? (vals m))) v38_l140)))


(def v41_l161 (def S3 (hm/symmetric-group 3)))


(def v42_l163 (hm/order S3))


(deftest t43_l165 (is (= v42_l163 6)))


(def v45_l170 (hm/op S3 [1 2 0] [0 2 1]))


(def
 v47_l174
 (let [a [1 2 0] b [0 2 1]] (not= (hm/op S3 a b) (hm/op S3 b a))))


(deftest t48_l177 (is (true? v47_l174)))


(def
 v50_l188
 (def V4 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2))))


(def v51_l190 (hm/order V4))


(deftest t52_l192 (is (= v51_l190 4)))


(def v53_l194 (vec (hm/elements V4)))


(def
 v55_l198
 (every? (fn [g] (= (hm/op V4 g g) (hm/id V4))) (hm/elements V4)))


(deftest t56_l201 (is (true? v55_l198)))


(def
 v58_l206
 (hm/order
  (hm/product-group (hm/dihedral-group 3) (hm/cyclic-group 2))))


(deftest t59_l208 (is (= v58_l206 12)))


(def
 v61_l226
 (every?
  (fn
   [g]
   (and (= (hm/op D4 (hm/id D4) g) g) (= (hm/op D4 g (hm/id D4)) g)))
  (hm/elements D4)))


(deftest t62_l231 (is (true? v61_l226)))


(def
 v64_l235
 (every?
  (fn
   [g]
   (let
    [gi (hm/inv D4 g) e (hm/id D4)]
    (and (= (hm/op D4 g gi) e) (= (hm/op D4 gi g) e))))
  (hm/elements D4)))


(deftest t65_l242 (is (true? v64_l235)))


(def
 v67_l246
 (let
  [elts (vec (hm/elements D4))]
  (every?
   (fn
    [[a b c]]
    (= (hm/op D4 (hm/op D4 a b) c) (hm/op D4 a (hm/op D4 b c))))
   (for [a elts b elts c elts] [a b c]))))


(deftest t68_l252 (is (true? v67_l246)))


(def
 v70_l273
 (let
  [classes (hm/conjugacy-classes D4)]
  (kind/table
   {:column-names ["Representative" "Size" "Elements"],
    :row-vectors
    (mapv
     (fn
      [c]
      [(str (:representative c)) (:size c) (str (vec (:elements c)))])
     classes)})))


(def
 v72_l283
 (let
  [classes (hm/conjugacy-classes D4)]
  (reduce + (map :size classes))))


(deftest t73_l286 (is (= v72_l283 8)))


(def v75_l298 (kind/hiccup (hm/cayley-table-svg (hm/cyclic-group 4))))


(def v77_l302 (kind/hiccup (hm/cayley-table-svg (hm/dihedral-group 3))))


(def
 v79_l318
 (kind/table
  {:column-names ["Group" "Order" "# Classes"],
   :row-vectors
   (mapv
    (fn
     [[label G]]
     [label (hm/order G) (count (hm/conjugacy-classes G))])
    [["Z/4Z" (hm/cyclic-group 4)]
     ["Z/12Z" (hm/cyclic-group 12)]
     ["S_3" (hm/symmetric-group 3)]
     ["S_4" (hm/symmetric-group 4)]
     ["S_5" (hm/symmetric-group 5)]
     ["D_4" (hm/dihedral-group 4)]
     ["D_6" (hm/dihedral-group 6)]
     ["D_12" (hm/dihedral-group 12)]
     ["Z/2Z × Z/2Z" V4]
     ["D_3 × Z/2Z"
      (hm/product-group (hm/dihedral-group 3) (hm/cyclic-group 2))]])}))
