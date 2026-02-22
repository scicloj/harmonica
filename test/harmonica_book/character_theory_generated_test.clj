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
 v6_l63
 (allclose?
  (cx/cabs (:table (hm/character-table (hm/cyclic-group 8))))
  1.0))


(deftest t7_l65 (is (true? v6_l63)))


(def
 v9_l74
 (let
  [table
   (:table (hm/character-table (hm/symmetric-group 4)))
   re-vals
   (cx/re table)]
  (and
   (allclose? (cx/im table) 0.0)
   (allclose? re-vals (dfn/rint re-vals)))))


(deftest t10_l79 (is (true? v9_l74)))


(def
 v12_l95
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__74322#] (long (Math/round (cx/re p1__74322#))))
      row))
    (:table ct))]
  re-table))


(deftest t13_l100 (is (= v12_l95 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def
 v15_l108
 (let
  [ct
   (hm/character-table (hm/symmetric-group 4))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__74323#] (long (Math/round (cx/re p1__74323#))))
      row))
    (:table ct))]
  re-table))


(deftest
 t16_l113
 (is
  (=
   v15_l108
   [[1 1 1 1 1]
    [3 1 -1 0 -1]
    [2 0 2 -1 0]
    [3 -1 -1 0 1]
    [1 -1 1 1 -1]])))


(def
 v18_l134
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


(deftest t19_l152 (is (true? v18_l134)))


(def
 v21_l163
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


(deftest t22_l180 (is (true? v21_l163)))


(def
 v24_l192
 (let
  [ct-d3
   (hm/character-table (hm/dihedral-group 3))
   dims
   (sort
    (mapv
     (fn* [p1__74324#] (long (Math/round (cx/re (p1__74324# 0)))))
     (:table ct-d3)))
   ct-s3
   (hm/character-table (hm/symmetric-group 3))
   dims-s3
   (sort
    (mapv
     (fn* [p1__74325#] (long (Math/round (cx/re (p1__74325# 0)))))
     (:table ct-s3)))]
  (= dims dims-s3)))


(deftest t25_l198 (is (true? v24_l192)))


(def
 v27_l207
 (let
  [trivial-row
   (first (:table (hm/character-table (hm/symmetric-group 5))))]
  (allclose? (cx/re trivial-row) 1.0)))


(deftest t28_l210 (is (true? v27_l207)))


(def
 v30_l215
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


(deftest t31_l224 (is (true? v30_l215)))


(def
 v33_l228
 (let
  [ct
   (hm/character-table (hm/symmetric-group 5))
   {:keys [table classes irrep-labels]}
   ct]
  (kind/table
   {:column-names (into [""] (mapv str classes)),
    :row-vectors
    (mapv
     (fn
      [label row]
      (into
       [(str label)]
       (mapv
        (fn* [p1__74326#] (long (Math/round (cx/re p1__74326#))))
        row)))
     irrep-labels
     table)})))
