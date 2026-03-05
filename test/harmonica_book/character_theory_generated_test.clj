(ns
 harmonica-book.character-theory-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [harmonica-book.book-helpers :refer [allclose?]]
  [tech.v3.datatype.functional :as dfn]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l57
 (let
  [ct (hm/character-table (hm/cyclic-group 6))]
  (count (:table ct))))


(deftest t4_l60 (is (= v3_l57 6)))


(def
 v5_l62
 (hm/show-character-table (hm/character-table (hm/cyclic-group 6))))


(def
 v7_l66
 (allclose?
  (el/abs (:table (hm/character-table (hm/cyclic-group 8))))
  1.0))


(deftest t8_l68 (is (true? v7_l66)))


(def
 v10_l77
 (let
  [table
   (:table (hm/character-table (hm/symmetric-group 4)))
   re-vals
   (el/re table)]
  (and
   (allclose? (el/im table) 0.0)
   (allclose? re-vals (dfn/rint re-vals)))))


(deftest t11_l82 (is (true? v10_l77)))


(def
 v13_l92
 (hm/show-character-table (hm/character-table (hm/symmetric-group 3))))


(def
 v14_l94
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv (fn* [p1__67151#] (Math/round (el/re p1__67151#))) row))
    (:table ct))]
  re-table))


(deftest t15_l99 (is (= v14_l94 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def
 v17_l107
 (hm/show-character-table (hm/character-table (hm/symmetric-group 4))))


(def
 v18_l109
 (let
  [ct
   (hm/character-table (hm/symmetric-group 4))
   re-table
   (mapv
    (fn
     [row]
     (mapv (fn* [p1__67152#] (Math/round (el/re p1__67152#))) row))
    (:table ct))]
  re-table))


(deftest
 t19_l114
 (is
  (=
   v18_l109
   [[1 1 1 1 1]
    [3 1 -1 0 -1]
    [2 0 2 -1 0]
    [3 -1 -1 0 1]
    [1 -1 1 1 -1]])))


(def
 v21_l135
 (let
  [ct
   (hm/character-table (hm/symmetric-group 4))
   {:keys [table class-sizes]}
   ct
   order
   (hm/order (:group ct))
   n
   (count table)
   inner
   (fn
    [i j]
    (reduce
     +
     (map-indexed
      (fn
       [k sz]
       (let
        [ci ((table i) k) cj ((table j) k)]
        (*
         (double sz)
         (+ (* (el/re ci) (el/re cj)) (* (el/im ci) (el/im cj))))))
      class-sizes)))
   max-err
   (apply
    max
    (for
     [i (range n) j (range n)]
     (Math/abs (- (inner i j) (if (= i j) (double order) 0.0)))))]
  (< max-err 1.0E-8)))


(deftest t22_l153 (is (true? v21_l135)))


(def
 v24_l164
 (let
  [ct
   (hm/character-table (hm/symmetric-group 4))
   {:keys [table class-sizes]}
   ct
   order
   (hm/order (:group ct))
   n
   (count class-sizes)
   max-err
   (apply
    max
    (for
     [i (range n) j (range n)]
     (let
      [ip
       (reduce
        +
        (map
         (fn
          [row]
          (let
           [ci (row i) cj (row j)]
           (+ (* (el/re ci) (el/re cj)) (* (el/im ci) (el/im cj)))))
         table))
       expected
       (if
        (= i j)
        (/ (double order) (double (nth class-sizes i)))
        0.0)]
      (Math/abs (- ip expected)))))]
  (< max-err 1.0E-8)))


(deftest t25_l181 (is (true? v24_l164)))


(def
 v27_l192
 (hm/show-character-table (hm/character-table (hm/dihedral-group 4))))


(def
 v29_l196
 (let
  [ct-d3
   (hm/character-table (hm/dihedral-group 3))
   dims
   (sort
    (mapv
     (fn* [p1__67153#] (Math/round (el/re (p1__67153# 0))))
     (:table ct-d3)))
   ct-s3
   (hm/character-table (hm/symmetric-group 3))
   dims-s3
   (sort
    (mapv
     (fn* [p1__67154#] (Math/round (el/re (p1__67154# 0))))
     (:table ct-s3)))]
  (= dims dims-s3)))


(deftest t30_l202 (is (true? v29_l196)))


(def
 v32_l211
 (let
  [trivial-row
   (first (:table (hm/character-table (hm/symmetric-group 5))))]
  (allclose? (el/re trivial-row) 1.0)))


(deftest t33_l214 (is (true? v32_l211)))


(def
 v35_l219
 (let
  [ct
   (hm/character-table (hm/symmetric-group 5))
   sign-label
   [1 1 1 1 1]
   labels
   (:irrep-labels ct)
   row-idx
   (.indexOf labels sign-label)
   sign-row
   (nth (:table ct) row-idx)
   classes
   (:classes ct)
   expected
   (mapv (fn [mu] (Math/pow -1 (- 5 (count mu)))) classes)]
  (allclose? (el/re sign-row) expected)))


(deftest t36_l228 (is (true? v35_l219)))


(def
 v38_l232
 (hm/show-character-table (hm/character-table (hm/symmetric-group 5))))
