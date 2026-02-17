(ns
 reel-book.character-theory-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [fastmath.complex :as c]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l29
 (let
  [ct (reel/character-table (reel/cyclic-group 4))]
  (count (:table ct))))


(deftest t4_l32 (is (= v3_l29 4)))


(def
 v6_l36
 (let
  [ct
   (reel/character-table (reel/cyclic-group 8))
   entries
   (for [row (:table ct) v row] v)]
  (every?
   (fn* [p1__87497#] (< (Math/abs (- (c/abs p1__87497#) 1.0)) 1.0E-10))
   entries)))


(deftest t7_l40 (is (true? v6_l36)))


(def
 v9_l47
 (let
  [ct
   (reel/character-table (reel/symmetric-group 4))
   entries
   (for [row (:table ct) v row] v)]
  (every?
   (fn
    [v]
    (and
     (< (Math/abs (c/im v)) 1.0E-10)
     (< (Math/abs (- (c/re v) (Math/round (c/re v)))) 1.0E-10)))
   entries)))


(deftest t10_l54 (is (true? v9_l47)))


(def
 v12_l62
 (let
  [ct (reel/character-table (reel/dihedral-group 5))]
  (count (:table ct))))


(deftest t13_l65 (is (= v12_l62 4)))


(def
 v14_l67
 (let
  [ct (reel/character-table (reel/dihedral-group 6))]
  (count (:table ct))))


(deftest t15_l70 (is (= v14_l67 6)))


(def
 v17_l80
 (defn
  check-row-orthogonality
  "Check row orthogonality for a character table. Returns max absolute error."
  [ct]
  (let
   [{:keys [table class-sizes]}
    ct
    order
    (reel/order (:group ct))
    n
    (count table)]
   (apply
    max
    (for
     [i (range n) j (range n)]
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
            (+ (* (c/re ci) (c/re cj)) (* (c/im ci) (c/im cj))))))
         class-sizes))
       expected
       (if (= i j) (double order) 0.0)]
      (Math/abs (- ip expected))))))))


(def
 v19_l101
 (let
  [results
   (for
    [n [2 3 5 7 11 13 16 24]]
    (<
     (check-row-orthogonality
      (reel/character-table (reel/cyclic-group n)))
     1.0E-8))]
  (every? true? results)))


(deftest t20_l107 (is (true? v19_l101)))


(def
 v22_l111
 (let
  [results
   (for
    [n [2 3 4 5 6]]
    (<
     (check-row-orthogonality
      (reel/character-table (reel/symmetric-group n)))
     1.0E-8))]
  (every? true? results)))


(deftest t23_l117 (is (true? v22_l111)))


(def
 v25_l121
 (let
  [results
   (for
    [n [3 4 5 6 7 8 9 10 12 15 16 20 24]]
    (<
     (check-row-orthogonality
      (reel/character-table (reel/dihedral-group n)))
     1.0E-8))]
  (every? true? results)))


(deftest t26_l127 (is (true? v25_l121)))


(def
 v28_l135
 (defn
  check-column-orthogonality
  "Check column orthogonality. Returns max absolute error."
  [ct]
  (let
   [{:keys [table class-sizes]}
    ct
    order
    (reel/order (:group ct))
    n
    (count class-sizes)]
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
           [ci (nth row i) cj (nth row j)]
           (+ (* (c/re ci) (c/re cj)) (* (c/im ci) (c/im cj)))))
         table))
       expected
       (if
        (= i j)
        (/ (double order) (double (nth class-sizes i)))
        0.0)]
      (Math/abs (- ip expected))))))))


(def
 v30_l155
 (let
  [results
   (concat
    (for
     [n [3 5 7 12 16]]
     (<
      (check-column-orthogonality
       (reel/character-table (reel/cyclic-group n)))
      1.0E-8))
    (for
     [n [3 4 5 6]]
     (<
      (check-column-orthogonality
       (reel/character-table (reel/symmetric-group n)))
      1.0E-8))
    (for
     [n [3 4 5 6 8 10 12 15]]
     (<
      (check-column-orthogonality
       (reel/character-table (reel/dihedral-group n)))
      1.0E-8)))]
  (every? true? results)))


(deftest t31_l168 (is (true? v30_l155)))


(def
 v33_l177
 (defn
  dim-sq-sum-check
  "Verify dimension-squared sum equals group order."
  [ct]
  (let
   [{:keys [table]}
    ct
    order
    (reel/order (:group ct))
    dims
    (map (fn* [p1__87498#] (c/re (first p1__87498#))) table)
    sum-sq
    (reduce + (map (fn* [p1__87499#] (* p1__87499# p1__87499#)) dims))]
   (< (Math/abs (- sum-sq (double order))) 1.0E-8))))


(def
 v34_l186
 (let
  [results
   (concat
    (for
     [n [2 3 5 7 11 16 24]]
     (dim-sq-sum-check (reel/character-table (reel/cyclic-group n))))
    (for
     [n [2 3 4 5 6 7]]
     (dim-sq-sum-check
      (reel/character-table (reel/symmetric-group n))))
    (for
     [n [3 4 5 6 7 8 10 12 15 20]]
     (dim-sq-sum-check
      (reel/character-table (reel/dihedral-group n)))))]
  (every? true? results)))


(deftest t35_l196 (is (true? v34_l186)))


(def
 v37_l203
 (let
  [results
   (concat
    (for
     [n [2 3 5 7 12 24]]
     (let
      [G (reel/cyclic-group n) ct (reel/character-table G)]
      (= (count (:table ct)) (count (reel/conjugacy-classes G)))))
    (for
     [n [2 3 4 5 6 7]]
     (let
      [G (reel/symmetric-group n) ct (reel/character-table G)]
      (= (count (:table ct)) (count (reel/conjugacy-classes G)))))
    (for
     [n [3 4 5 6 8 10 12]]
     (let
      [G (reel/dihedral-group n) ct (reel/character-table G)]
      (= (count (:table ct)) (count (reel/conjugacy-classes G))))))]
  (every? true? results)))


(deftest t38_l222 (is (true? v37_l203)))


(def
 v40_l229
 (let
  [results
   (concat
    (for
     [n [2 5 12]]
     (let
      [ct (reel/character-table (reel/cyclic-group n))]
      (every?
       (fn*
        [p1__87500#]
        (< (c/abs (c/sub p1__87500# (c/complex 1.0 0.0))) 1.0E-10))
       (first (:table ct)))))
    (for
     [n [3 4 5]]
     (let
      [ct (reel/character-table (reel/symmetric-group n))]
      (every?
       (fn*
        [p1__87501#]
        (< (c/abs (c/sub p1__87501# (c/complex 1.0 0.0))) 1.0E-10))
       (first (:table ct)))))
    (for
     [n [3 5 6 8]]
     (let
      [ct (reel/character-table (reel/dihedral-group n))]
      (every?
       (fn*
        [p1__87502#]
        (< (c/abs (c/sub p1__87502# (c/complex 1.0 0.0))) 1.0E-10))
       (first (:table ct))))))]
  (every? true? results)))


(deftest t41_l245 (is (true? v40_l229)))


(def
 v43_l253
 (let
  [results
   (for
    [n [3 4 5]]
    (let
     [G
      (reel/symmetric-group n)
      ct
      (reel/character-table G)
      {:keys [table class-sizes]}
      ct
      order
      (reel/order G)
      n-irreps
      (count table)]
     (every?
      identity
      (for
       [i (range n-irreps) j (range n-irreps)]
       (let
        [ip
         (reel/character-inner-product
          (nth table i)
          (nth table j)
          class-sizes
          order)
         expected
         (if (= i j) 1.0 0.0)]
        (< (c/abs (c/sub ip (c/complex expected 0.0))) 1.0E-8))))))]
  (every? true? results)))


(deftest t44_l268 (is (true? v43_l253)))


(def
 v46_l282
 (let
  [ct
   (reel/character-table (reel/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__87503#] (long (Math/round (c/re p1__87503#))))
      row))
    (:table ct))]
  re-table))


(deftest t47_l287 (is (= v46_l282 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def
 v49_l291
 (let
  [ct
   (reel/character-table (reel/symmetric-group 4))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__87504#] (long (Math/round (c/re p1__87504#))))
      row))
    (:table ct))]
  re-table))


(deftest
 t50_l296
 (is
  (=
   v49_l291
   [[1 1 1 1 1]
    [3 1 -1 0 -1]
    [2 0 2 -1 0]
    [3 -1 -1 0 1]
    [1 -1 1 1 -1]])))


(def
 v52_l307
 (let
  [ct-d3
   (reel/character-table (reel/dihedral-group 3))
   dims
   (sort
    (mapv
     (fn* [p1__87505#] (long (Math/round (c/re (first p1__87505#)))))
     (:table ct-d3)))
   ct-s3
   (reel/character-table (reel/symmetric-group 3))
   dims-s3
   (sort
    (mapv
     (fn* [p1__87506#] (long (Math/round (c/re (first p1__87506#)))))
     (:table ct-s3)))]
  (= dims dims-s3)))


(deftest t53_l313 (is (true? v52_l307)))


(def
 v55_l322
 (let
  [results
   (for
    [n (range 2 8) mu (reel/partitions n)]
    (let
     [ct
      (reel/character-table (reel/symmetric-group n))
      classes
      (:classes ct)
      idx
      (.indexOf classes mu)
      val
      (c/re (nth (first (:table ct)) idx))]
     (= 1.0 val)))]
  (every? true? results)))


(deftest t56_l332 (is (true? v55_l322)))


(def
 v58_l338
 (let
  [results
   (for
    [n (range 2 8) mu (reel/partitions n)]
    (let
     [ct
      (reel/character-table (reel/symmetric-group n))
      sign-label
      (vec (repeat n 1))
      classes
      (:classes ct)
      labels
      (:irrep-labels ct)
      row-idx
      (.indexOf labels sign-label)
      col-idx
      (.indexOf classes mu)
      val
      (long
       (Math/round (c/re (nth (nth (:table ct) row-idx) col-idx))))
      expected
      (long (Math/pow -1 (- n (count mu))))]
     (= val expected)))]
  (every? true? results)))


(deftest t59_l353 (is (true? v58_l338)))


(def
 v61_l367
 (let
  [results
   (for
    [n (range 3 21)]
    (let
     [ct
      (reel/character-table (reel/dihedral-group n))
      dims
      (mapv
       (fn* [p1__87507#] (long (Math/round (c/re (first p1__87507#)))))
       (:table ct))
      one-dims
      (count (filter (fn* [p1__87508#] (= 1 p1__87508#)) dims))
      two-dims
      (count (filter (fn* [p1__87509#] (= 2 p1__87509#)) dims))
      expected-1d
      (if (odd? n) 2 4)
      expected-2d
      (if (odd? n) (quot (dec n) 2) (dec (quot n 2)))]
     (and (= one-dims expected-1d) (= two-dims expected-2d))))]
  (every? true? results)))


(deftest t62_l379 (is (true? v61_l367)))


(def
 v64_l385
 (let
  [ct
   (reel/character-table (reel/symmetric-group 5))
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
        (fn* [p1__87510#] (long (Math/round (c/re p1__87510#))))
        row)))
     irrep-labels
     table)})))
