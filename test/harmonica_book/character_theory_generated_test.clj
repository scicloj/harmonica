(ns
 harmonica-book.character-theory-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.complex :as cx]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l33
 (let
  [ct (hm/character-table (hm/cyclic-group 6))]
  (count (:table ct))))


(deftest t4_l36 (is (= v3_l33 6)))


(def
 v6_l40
 (let
  [ct
   (hm/character-table (hm/cyclic-group 8))
   entries
   (for [row (:table ct) v row] v)]
  (every?
   (fn*
    [p1__62510#]
    (< (Math/abs (- (cx/cabs p1__62510#) 1.0)) 1.0E-10))
   entries)))


(deftest t7_l44 (is (true? v6_l40)))


(def
 v9_l51
 (let
  [ct
   (hm/character-table (hm/symmetric-group 4))
   entries
   (for [row (:table ct) v row] v)]
  (every?
   (fn
    [v]
    (and
     (< (Math/abs (cx/im v)) 1.0E-10)
     (< (Math/abs (- (cx/re v) (Math/round (cx/re v)))) 1.0E-10)))
   entries)))


(deftest t10_l58 (is (true? v9_l51)))


(def
 v12_l65
 (let
  [ct (hm/character-table (hm/dihedral-group 5))]
  (count (:table ct))))


(deftest t13_l68 (is (= v12_l65 4)))


(def
 v14_l70
 (let
  [ct (hm/character-table (hm/dihedral-group 6))]
  (count (:table ct))))


(deftest t15_l73 (is (= v14_l70 6)))


(def
 v17_l84
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


(deftest t18_l102 (is (true? v17_l84)))


(def
 v20_l115
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


(deftest t21_l132 (is (true? v20_l115)))


(def
 v23_l146
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__62511#] (long (Math/round (cx/re p1__62511#))))
      row))
    (:table ct))]
  re-table))


(deftest t24_l151 (is (= v23_l146 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def
 v26_l155
 (let
  [ct
   (hm/character-table (hm/symmetric-group 4))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__62512#] (long (Math/round (cx/re p1__62512#))))
      row))
    (:table ct))]
  re-table))


(deftest
 t27_l160
 (is
  (=
   v26_l155
   [[1 1 1 1 1]
    [3 1 -1 0 -1]
    [2 0 2 -1 0]
    [3 -1 -1 0 1]
    [1 -1 1 1 -1]])))


(def
 v29_l172
 (let
  [ct-d3
   (hm/character-table (hm/dihedral-group 3))
   dims
   (sort
    (mapv
     (fn* [p1__62513#] (long (Math/round (cx/re (p1__62513# 0)))))
     (:table ct-d3)))
   ct-s3
   (hm/character-table (hm/symmetric-group 3))
   dims-s3
   (sort
    (mapv
     (fn* [p1__62514#] (long (Math/round (cx/re (p1__62514# 0)))))
     (:table ct-s3)))]
  (= dims dims-s3)))


(deftest t30_l178 (is (true? v29_l172)))


(def
 v32_l184
 (let
  [ct
   (hm/character-table (hm/symmetric-group 5))
   trivial-row
   (first (:table ct))]
  (every?
   (fn*
    [p1__62515#]
    (< (cx/cabs (cx/csub p1__62515# (cx/complex 1.0 0.0))) 1.0E-10))
   trivial-row)))


(deftest t33_l189 (is (true? v32_l184)))


(def
 v35_l194
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
   (:classes ct)]
  (every?
   identity
   (map-indexed
    (fn
     [i mu]
     (let
      [expected
       (Math/pow -1 (- 5 (count mu)))
       actual
       (cx/re (nth sign-row i))]
      (< (Math/abs (- actual expected)) 1.0E-10)))
    classes))))


(deftest t36_l207 (is (true? v35_l194)))


(def
 v38_l215
 (let
  [results
   (for
    [n (range 3 13)]
    (let
     [ct
      (hm/character-table (hm/dihedral-group n))
      dims
      (mapv
       (fn* [p1__62516#] (long (Math/round (cx/re (p1__62516# 0)))))
       (:table ct))
      one-dims
      (count (filter (fn* [p1__62517#] (= 1 p1__62517#)) dims))
      two-dims
      (count (filter (fn* [p1__62518#] (= 2 p1__62518#)) dims))
      expected-1d
      (if (odd? n) 2 4)
      expected-2d
      (if (odd? n) (quot (dec n) 2) (dec (quot n 2)))]
     (and (= one-dims expected-1d) (= two-dims expected-2d))))]
  (every? true? results)))


(deftest t39_l227 (is (true? v38_l215)))


(def
 v41_l233
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
        (fn* [p1__62519#] (long (Math/round (cx/re p1__62519#))))
        row)))
     irrep-labels
     table)})))
