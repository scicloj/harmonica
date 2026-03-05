(ns
 harmonica-book.algebraic-identities-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.protocols :as p]
  [scicloj.harmonica.analysis.representations :as rep]
  [scicloj.harmonica.linalg.complex :as cx]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l22
 (def
  test-groups
  "A collection of groups for systematic testing."
  [{:label "Z/2Z", :group (hm/cyclic-group 2), :has-ct? true}
   {:label "Z/5Z", :group (hm/cyclic-group 5), :has-ct? true}
   {:label "Z/7Z", :group (hm/cyclic-group 7), :has-ct? true}
   {:label "Z/12Z", :group (hm/cyclic-group 12), :has-ct? true}
   {:label "S_3", :group (hm/symmetric-group 3), :has-ct? true}
   {:label "S_4", :group (hm/symmetric-group 4), :has-ct? true}
   {:label "S_5", :group (hm/symmetric-group 5), :has-ct? true}
   {:label "D_3", :group (hm/dihedral-group 3), :has-ct? true}
   {:label "D_4", :group (hm/dihedral-group 4), :has-ct? true}
   {:label "D_5", :group (hm/dihedral-group 5), :has-ct? true}
   {:label "D_6", :group (hm/dihedral-group 6), :has-ct? true}
   {:label "D_8", :group (hm/dihedral-group 8), :has-ct? true}
   {:label "Z/2Z × Z/3Z",
    :group (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3)),
    :has-ct? false}
   {:label "Z/2Z × Z/2Z",
    :group (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2)),
    :has-ct? false}
   {:label "Z/3Z × Z/4Z",
    :group (hm/product-group (hm/cyclic-group 3) (hm/cyclic-group 4)),
    :has-ct? false}]))


(def
 v4_l46
 (def
  ct-groups
  "Groups that have character-table implementations."
  (filterv :has-ct? test-groups)))


(def
 v6_l56
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [e
       (hm/id group)
       ok?
       (every?
        (fn [g] (and (= (hm/op group e g) g) (= (hm/op group g e) g)))
        (hm/elements group))]
      {:group label, :pass? ok?}))
    test-groups)]
  (every? :pass? results)))


(deftest t7_l67 (is (true? v6_l56)))


(def
 v9_l71
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [e
       (hm/id group)
       ok?
       (every?
        (fn
         [g]
         (let
          [gi (hm/inv group g)]
          (and (= (hm/op group g gi) e) (= (hm/op group gi g) e))))
        (hm/elements group))]
      {:group label, :pass? ok?}))
    test-groups)]
  (every? :pass? results)))


(deftest t10_l83 (is (true? v9_l71)))


(def
 v12_l89
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [elts
       (vec (hm/elements group))
       triples
       (if
        (<= (count elts) 24)
        (for [a elts b elts c elts] [a b c])
        (let
         [rng (java.util.Random. 42)]
         (repeatedly
          500
          (fn
           []
           [(elts (.nextInt rng (count elts)))
            (elts (.nextInt rng (count elts)))
            (elts (.nextInt rng (count elts)))]))))
       ok?
       (every?
        (fn
         [[a b c]]
         (=
          (hm/op group (hm/op group a b) c)
          (hm/op group a (hm/op group b c))))
        triples)]
      {:group label, :pass? ok?}))
    test-groups)]
  (every? :pass? results)))


(deftest t13_l109 (is (true? v12_l89)))


(def
 v15_l115
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [classes
       (hm/conjugacy-classes group)
       total
       (reduce + (map :size classes))]
      {:group label, :pass? (= total (hm/order group))}))
    test-groups)]
  (every? :pass? results)))


(deftest t16_l123 (is (true? v15_l115)))


(def
 v18_l127
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [classes
       (hm/conjugacy-classes group)
       all-elts
       (mapcat :elements classes)
       group-set
       (set (hm/elements group))]
      {:group label,
       :pass?
       (and
        (= (count all-elts) (count (set all-elts)))
        (= (set all-elts) group-set))}))
    test-groups)]
  (every? :pass? results)))


(deftest t19_l138 (is (true? v18_l127)))


(def
 v21_l151
 (defn
  row-orthogonality-check
  "Check row orthogonality for all pairs of irreps."
  [{:keys [label group]}]
  (let
   [ct
    (hm/character-table group)
    {:keys [table class-sizes]}
    ct
    n-irreps
    (count table)
    order
    (hm/order group)
    tol
    1.0E-8]
   (every?
    identity
    (for
     [i (range n-irreps) j (range n-irreps)]
     (let
      [ip
       (reduce
        +
        (map-indexed
         (fn
          [k sz]
          (let
           [ci (nth (nth table i) k) cj (nth (nth table j) k)]
           (*
            (double sz)
            (+ (* (cx/re ci) (cx/re cj)) (* (cx/im ci) (cx/im cj))))))
         class-sizes))
       expected
       (if (= i j) (double order) 0.0)]
      (< (Math/abs (- ip expected)) tol)))))))


(def v22_l173 (every? row-orthogonality-check ct-groups))
