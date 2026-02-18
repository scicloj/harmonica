(ns
 harmonica-book.character-theory-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.complex :as cx]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l29
 (let
  [ct (hm/character-table (hm/cyclic-group 4))]
  (count (:table ct))))


(deftest t4_l32 (is (= v3_l29 4)))


(def
 v6_l36
 (let
  [ct
   (hm/character-table (hm/cyclic-group 8))
   entries
   (for [row (:table ct) v row] v)]
  (every?
   (fn*
    [p1__91883#]
    (< (Math/abs (- (cx/cabs p1__91883#) 1.0)) 1.0E-10))
   entries)))


(deftest t7_l40 (is (true? v6_l36)))


(def
 v9_l47
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


(deftest t10_l54 (is (true? v9_l47)))


(def
 v12_l62
 (let
  [ct (hm/character-table (hm/dihedral-group 5))]
  (count (:table ct))))


(deftest t13_l65 (is (= v12_l62 4)))


(def
 v14_l67
 (let
  [ct (hm/character-table (hm/dihedral-group 6))]
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
    (hm/order (:group ct))
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
            (+ (* (cx/re ci) (cx/re cj)) (* (cx/im ci) (cx/im cj))))))
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
     (check-row-orthogonality (hm/character-table (hm/cyclic-group n)))
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
      (hm/character-table (hm/symmetric-group n)))
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
      (hm/character-table (hm/dihedral-group n)))
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
    (hm/order (:group ct))
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
           (+ (* (cx/re ci) (cx/re cj)) (* (cx/im ci) (cx/im cj)))))
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
       (hm/character-table (hm/cyclic-group n)))
      1.0E-8))
    (for
     [n [3 4 5 6]]
     (<
      (check-column-orthogonality
       (hm/character-table (hm/symmetric-group n)))
      1.0E-8))
    (for
     [n [3 4 5 6 8 10 12 15]]
     (<
      (check-column-orthogonality
       (hm/character-table (hm/dihedral-group n)))
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
    (hm/order (:group ct))
    dims
    (map (fn* [p1__91884#] (cx/re (p1__91884# 0))) table)
    sum-sq
    (reduce + (map (fn* [p1__91885#] (* p1__91885# p1__91885#)) dims))]
   (< (Math/abs (- sum-sq (double order))) 1.0E-8))))


(def
 v34_l186
 (let
  [results
   (concat
    (for
     [n [2 3 5 7 11 16 24]]
     (dim-sq-sum-check (hm/character-table (hm/cyclic-group n))))
    (for
     [n [2 3 4 5 6 7]]
     (dim-sq-sum-check (hm/character-table (hm/symmetric-group n))))
    (for
     [n [3 4 5 6 7 8 10 12 15 20]]
     (dim-sq-sum-check (hm/character-table (hm/dihedral-group n)))))]
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
      [G (hm/cyclic-group n) ct (hm/character-table G)]
      (= (count (:table ct)) (count (hm/conjugacy-classes G)))))
    (for
     [n [2 3 4 5 6 7]]
     (let
      [G (hm/symmetric-group n) ct (hm/character-table G)]
      (= (count (:table ct)) (count (hm/conjugacy-classes G)))))
    (for
     [n [3 4 5 6 8 10 12]]
     (let
      [G (hm/dihedral-group n) ct (hm/character-table G)]
      (= (count (:table ct)) (count (hm/conjugacy-classes G))))))]
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
      [ct (hm/character-table (hm/cyclic-group n))]
      (every?
       (fn*
        [p1__91886#]
        (<
         (cx/cabs (cx/csub p1__91886# (cx/complex 1.0 0.0)))
         1.0E-10))
       (first (:table ct)))))
    (for
     [n [3 4 5]]
     (let
      [ct (hm/character-table (hm/symmetric-group n))]
      (every?
       (fn*
        [p1__91887#]
        (<
         (cx/cabs (cx/csub p1__91887# (cx/complex 1.0 0.0)))
         1.0E-10))
       (first (:table ct)))))
    (for
     [n [3 5 6 8]]
     (let
      [ct (hm/character-table (hm/dihedral-group n))]
      (every?
       (fn*
        [p1__91888#]
        (<
         (cx/cabs (cx/csub p1__91888# (cx/complex 1.0 0.0)))
         1.0E-10))
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
      (hm/symmetric-group n)
      ct
      (hm/character-table G)
      {:keys [table class-sizes]}
      ct
      order
      (hm/order G)
      n-irreps
      (count table)]
     (every?
      identity
      (for
       [i (range n-irreps) j (range n-irreps)]
       (let
        [ip
         (hm/character-inner-product
          (nth table i)
          (nth table j)
          class-sizes
          order)
         expected
         (if (= i j) 1.0 0.0)]
        (<
         (cx/cabs (cx/csub ip (cx/complex expected 0.0)))
         1.0E-8))))))]
  (every? true? results)))


(deftest t44_l268 (is (true? v43_l253)))


(def
 v46_l282
 (let
  [ct
   (hm/character-table (hm/symmetric-group 3))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__91889#] (long (Math/round (cx/re p1__91889#))))
      row))
    (:table ct))]
  re-table))


(deftest t47_l287 (is (= v46_l282 [[1 1 1] [2 0 -1] [1 -1 1]])))


(def
 v49_l291
 (let
  [ct
   (hm/character-table (hm/symmetric-group 4))
   re-table
   (mapv
    (fn
     [row]
     (mapv
      (fn* [p1__91890#] (long (Math/round (cx/re p1__91890#))))
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
   (hm/character-table (hm/dihedral-group 3))
   dims
   (sort
    (mapv
     (fn* [p1__91891#] (long (Math/round (cx/re (p1__91891# 0)))))
     (:table ct-d3)))
   ct-s3
   (hm/character-table (hm/symmetric-group 3))
   dims-s3
   (sort
    (mapv
     (fn* [p1__91892#] (long (Math/round (cx/re (p1__91892# 0)))))
     (:table ct-s3)))]
  (= dims dims-s3)))


(deftest t53_l313 (is (true? v52_l307)))


(def
 v55_l322
 (let
  [results
   (for
    [n (range 2 8) mu (hm/partitions n)]
    (let
     [ct
      (hm/character-table (hm/symmetric-group n))
      classes
      (:classes ct)
      idx
      (.indexOf classes mu)
      val
      (cx/re (((:table ct) 0) idx))]
     (= 1.0 val)))]
  (every? true? results)))


(deftest t56_l332 (is (true? v55_l322)))


(def
 v58_l338
 (let
  [results
   (for
    [n (range 2 8) mu (hm/partitions n)]
    (let
     [ct
      (hm/character-table (hm/symmetric-group n))
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
      (long (Math/round (cx/re (((:table ct) row-idx) col-idx))))
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
      (hm/character-table (hm/dihedral-group n))
      dims
      (mapv
       (fn* [p1__91893#] (long (Math/round (cx/re (p1__91893# 0)))))
       (:table ct))
      one-dims
      (count (filter (fn* [p1__91894#] (= 1 p1__91894#)) dims))
      two-dims
      (count (filter (fn* [p1__91895#] (= 2 p1__91895#)) dims))
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
        (fn* [p1__91896#] (long (Math/round (cx/re p1__91896#))))
        row)))
     irrep-labels
     table)})))
