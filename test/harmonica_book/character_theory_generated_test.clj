(ns
 harmonica-book.character-theory-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [harmonica-book.book-helpers :refer [allclose?]]
  [tech.v3.datatype.functional :as dfn]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l56
 (let
  [ct (hm/character-table (hm/cyclic-group 6))]
  (count (:table ct))))


(deftest t4_l59 (is (= v3_l56 6)))


(def
 v5_l61
 (hm/show-character-table (hm/character-table (hm/cyclic-group 6))))


(def
 v7_l65
 (allclose?
  (cx/cabs (:table (hm/character-table (hm/cyclic-group 8))))
  1.0))


(deftest t8_l67 (is (true? v7_l65)))


(def
 v10_l76
 (let
  [table
   (:table (hm/character-table (hm/symmetric-group 4)))
   re-vals
   (cx/re table)]
  (and
   (allclose? (cx/im table) 0.0)
   (allclose? re-vals (dfn/rint re-vals)))))


(deftest t11_l81 (is (true? v10_l76)))


(def
 v13_l91
 (hm/show-character-table (hm/character-table (hm/symmetric-group 3))))


(def
 v14_l93
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv (fn* [p1__122910#] (Math/round (cx/re p1__122910#))) row))
    (:table ct))]
  re-table))


(deftest t15_l98 (is (= v14_l93 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def
 v17_l106
 (hm/show-character-table (hm/character-table (hm/symmetric-group 4))))


(def
 v18_l108
 (let
  [ct
   (hm/character-table (hm/symmetric-group 4))
   re-table
   (mapv
    (fn
     [row]
     (mapv (fn* [p1__122911#] (Math/round (cx/re p1__122911#))) row))
    (:table ct))]
  re-table))


(deftest
 t19_l113
 (is
  (=
   v18_l108
   [[1 1 1 1 1]
    [3 1 -1 0 -1]
    [2 0 2 -1 0]
    [3 -1 -1 0 1]
    [1 -1 1 1 -1]])))


(def
 v21_l134
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
         (+ (* (cx/re ci) (cx/re cj)) (* (cx/im ci) (cx/im cj))))))
      class-sizes)))
   max-err
   (apply
    max
    (for
     [i (range n) j (range n)]
     (Math/abs (- (inner i j) (if (= i j) (double order) 0.0)))))]
  (< max-err 1.0E-8)))


(deftest t22_l152 (is (true? v21_l134)))


(def
 v24_l163
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
           (+ (* (cx/re ci) (cx/re cj)) (* (cx/im ci) (cx/im cj)))))
         table))
       expected
       (if
        (= i j)
        (/ (double order) (double (nth class-sizes i)))
        0.0)]
      (Math/abs (- ip expected)))))]
  (< max-err 1.0E-8)))


(deftest t25_l180 (is (true? v24_l163)))


(def
 v27_l191
 (hm/show-character-table (hm/character-table (hm/dihedral-group 4))))


(def
 v29_l195
 (let
  [ct-d3
   (hm/character-table (hm/dihedral-group 3))
   dims
   (sort
    (mapv
     (fn* [p1__122912#] (Math/round (cx/re (p1__122912# 0))))
     (:table ct-d3)))
   ct-s3
   (hm/character-table (hm/symmetric-group 3))
   dims-s3
   (sort
    (mapv
     (fn* [p1__122913#] (Math/round (cx/re (p1__122913# 0))))
     (:table ct-s3)))]
  (= dims dims-s3)))


(deftest t30_l201 (is (true? v29_l195)))


(def
 v32_l210
 (let
  [trivial-row
   (first (:table (hm/character-table (hm/symmetric-group 5))))]
  (allclose? (cx/re trivial-row) 1.0)))


(deftest t33_l213 (is (true? v32_l210)))


(def
 v35_l218
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
  (allclose? (cx/re sign-row) expected)))


(deftest t36_l227 (is (true? v35_l218)))


(def
 v38_l231
 (hm/show-character-table (hm/character-table (hm/symmetric-group 5))))
