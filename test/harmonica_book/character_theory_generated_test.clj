(ns
 harmonica-book.character-theory-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l54
 (let
  [ct (hm/character-table (hm/cyclic-group 6))]
  (count (:table ct))))


(deftest t4_l57 (is (= v3_l54 6)))


(def
 v6_l61
 (let
  [ct
   (hm/character-table (hm/cyclic-group 8))
   entries
   (for [row (:table ct) v row] v)]
  (every?
   (fn*
    [p1__86885#]
    (< (Math/abs (- (cx/cabs p1__86885#) 1.0)) 1.0E-10))
   entries)))


(deftest t7_l65 (is (true? v6_l61)))


(def
 v9_l74
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


(deftest t10_l81 (is (true? v9_l74)))


(def
 v12_l97
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__86886#] (long (Math/round (cx/re p1__86886#))))
      row))
    (:table ct))]
  re-table))


(deftest t13_l102 (is (= v12_l97 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def
 v15_l110
 (let
  [ct
   (hm/character-table (hm/symmetric-group 4))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__86887#] (long (Math/round (cx/re p1__86887#))))
      row))
    (:table ct))]
  re-table))


(deftest
 t16_l115
 (is
  (=
   v15_l110
   [[1 1 1 1 1]
    [3 1 -1 0 -1]
    [2 0 2 -1 0]
    [3 -1 -1 0 1]
    [1 -1 1 1 -1]])))


(def
 v18_l136
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


(deftest t19_l154 (is (true? v18_l136)))


(def
 v21_l165
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


(deftest t22_l182 (is (true? v21_l165)))


(def
 v24_l194
 (let
  [ct-d3
   (hm/character-table (hm/dihedral-group 3))
   dims
   (sort
    (mapv
     (fn* [p1__86888#] (long (Math/round (cx/re (p1__86888# 0)))))
     (:table ct-d3)))
   ct-s3
   (hm/character-table (hm/symmetric-group 3))
   dims-s3
   (sort
    (mapv
     (fn* [p1__86889#] (long (Math/round (cx/re (p1__86889# 0)))))
     (:table ct-s3)))]
  (= dims dims-s3)))


(deftest t25_l200 (is (true? v24_l194)))


(def
 v27_l209
 (let
  [ct
   (hm/character-table (hm/symmetric-group 5))
   trivial-row
   (first (:table ct))]
  (every?
   (fn*
    [p1__86890#]
    (< (cx/cabs (cx/csub p1__86890# (cx/complex 1.0 0.0))) 1.0E-10))
   trivial-row)))


(deftest t28_l214 (is (true? v27_l209)))


(def
 v30_l219
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


(deftest t31_l232 (is (true? v30_l219)))


(def
 v33_l236
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
        (fn* [p1__86891#] (long (Math/round (cx/re p1__86891#))))
        row)))
     irrep-labels
     table)})))
