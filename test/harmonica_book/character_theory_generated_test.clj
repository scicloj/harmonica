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
 v2_l21
 (defn
  format-cx
  "Format a complex character value for display."
  [v]
  (let
   [re
    (cx/re v)
    im
    (cx/im v)
    near-int?
    (fn [x] (< (Math/abs (- x (Math/round x))) 1.0E-10))
    fmt
    (fn
     [x]
     (if (near-int? x) (long (Math/round x)) (format "%.3f" x)))]
   (cond
    (< (Math/abs im) 1.0E-10)
    (fmt re)
    (< (Math/abs re) 1.0E-10)
    (let
     [i (fmt im)]
     (cond (= i 1) "i" (= i -1) "-i" :else (str i "i")))
    :else
    (let
     [r (fmt re) i (fmt im)]
     (if
      (neg? (if (number? im) im (Double/parseDouble (str im))))
      (str r (fmt im) "i")
      (str r "+" (fmt im) "i")))))))


(def
 v3_l39
 (defn
  show-character-table
  "Display a character table as a kind/table with labeled rows and columns."
  [ct]
  (let
   [{:keys [table classes irrep-labels]} ct]
   (kind/table
    {:column-names (into [""] (mapv str classes)),
     :row-vectors
     (mapv
      (fn [label row] (into [(str label)] (mapv format-cx row)))
      irrep-labels
      table)}))))


(def
 v5_l85
 (let
  [ct (hm/character-table (hm/cyclic-group 6))]
  (count (:table ct))))


(deftest t6_l88 (is (= v5_l85 6)))


(def
 v7_l90
 (show-character-table (hm/character-table (hm/cyclic-group 6))))


(def
 v9_l94
 (allclose?
  (cx/cabs (:table (hm/character-table (hm/cyclic-group 8))))
  1.0))


(deftest t10_l96 (is (true? v9_l94)))


(def
 v12_l105
 (let
  [table
   (:table (hm/character-table (hm/symmetric-group 4)))
   re-vals
   (cx/re table)]
  (and
   (allclose? (cx/im table) 0.0)
   (allclose? re-vals (dfn/rint re-vals)))))


(deftest t13_l110 (is (true? v12_l105)))


(def
 v15_l121
 (show-character-table (hm/character-table (hm/symmetric-group 3))))


(def
 v16_l123
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv (fn* [p1__113974#] (Math/round (cx/re p1__113974#))) row))
    (:table ct))]
  re-table))


(deftest t17_l128 (is (= v16_l123 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def
 v19_l136
 (show-character-table (hm/character-table (hm/symmetric-group 4))))


(def
 v20_l138
 (let
  [ct
   (hm/character-table (hm/symmetric-group 4))
   re-table
   (mapv
    (fn
     [row]
     (mapv (fn* [p1__113975#] (Math/round (cx/re p1__113975#))) row))
    (:table ct))]
  re-table))


(deftest
 t21_l143
 (is
  (=
   v20_l138
   [[1 1 1 1 1]
    [3 1 -1 0 -1]
    [2 0 2 -1 0]
    [3 -1 -1 0 1]
    [1 -1 1 1 -1]])))


(def
 v23_l164
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


(deftest t24_l182 (is (true? v23_l164)))


(def
 v26_l193
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


(deftest t27_l210 (is (true? v26_l193)))


(def
 v29_l221
 (show-character-table (hm/character-table (hm/dihedral-group 4))))


(def
 v31_l225
 (let
  [ct-d3
   (hm/character-table (hm/dihedral-group 3))
   dims
   (sort
    (mapv
     (fn* [p1__113976#] (Math/round (cx/re (p1__113976# 0))))
     (:table ct-d3)))
   ct-s3
   (hm/character-table (hm/symmetric-group 3))
   dims-s3
   (sort
    (mapv
     (fn* [p1__113977#] (Math/round (cx/re (p1__113977# 0))))
     (:table ct-s3)))]
  (= dims dims-s3)))


(deftest t32_l231 (is (true? v31_l225)))


(def
 v34_l240
 (let
  [trivial-row
   (first (:table (hm/character-table (hm/symmetric-group 5))))]
  (allclose? (cx/re trivial-row) 1.0)))


(deftest t35_l243 (is (true? v34_l240)))


(def
 v37_l248
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


(deftest t38_l257 (is (true? v37_l248)))


(def
 v40_l261
 (show-character-table (hm/character-table (hm/symmetric-group 5))))
