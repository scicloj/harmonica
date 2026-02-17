(ns
 reel-book.group-actions-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l21
 (defn rotation-action [n] (fn [g x] (mod (+ (long x) (long g)) n))))


(def
 v5_l27
 (let
  [G (reel/cyclic-group 5) act (rotation-action 5)]
  (reel/orbit G act 0)))


(deftest t6_l31 (is (= v5_l27 #{0 1 4 3 2})))


(def
 v8_l35
 (let
  [G (reel/cyclic-group 5) act (rotation-action 5)]
  (count (reel/orbits G act (range 5)))))


(deftest t9_l39 (is (= v8_l35 1)))


(def
 v11_l48
 (defn
  dihedral-vertex-action
  [n]
  (fn
   [[t k] x]
   (case
    t
    :r
    (mod (+ (long x) (long k)) n)
    :s
    (mod (- (long k) (long x)) n)))))


(def
 v13_l56
 (let
  [G (reel/dihedral-group 4) act (dihedral-vertex-action 4)]
  (count (reel/orbits G act (range 4)))))


(deftest t14_l60 (is (= v13_l56 1)))


(def
 v16_l70
 (let
  [results
   (for
    [n (range 3 13)]
    (let
     [G
      (reel/dihedral-group n)
      act
      (dihedral-vertex-action n)
      order
      (reel/order G)]
     (every?
      (fn
       [x]
       (=
        order
        (*
         (count (reel/orbit G act x))
         (count (reel/stabilizer G act x)))))
      (range n))))]
  (every? true? results)))


(deftest t17_l81 (is (true? v16_l70)))


(def
 v19_l85
 (let
  [results
   (for
    [n [2 3 5 7 11 13]]
    (let
     [G (reel/cyclic-group n) act (rotation-action n)]
     (every?
      (fn
       [x]
       (=
        n
        (*
         (count (reel/orbit G act x))
         (count (reel/stabilizer G act x)))))
      (range n))))]
  (every? true? results)))


(deftest t20_l95 (is (true? v19_l85)))


(def
 v22_l102
 (let
  [results
   (for
    [n [4 5] k [2 3]]
    (let
     [G
      (reel/symmetric-group n)
      perm-act
      (fn [sigma x] (sigma x))
      {:keys [act domain]}
      (reel/subset-action perm-act (range n) k)]
     (every?
      (fn
       [x]
       (=
        (reel/order G)
        (*
         (count (reel/orbit G act x))
         (count (reel/stabilizer G act x)))))
      (take 5 domain))))]
  (every? true? results)))


(deftest t23_l116 (is (true? v22_l102)))


(def
 v25_l124
 (let
  [G
   (reel/dihedral-group 6)
   act
   (dihedral-vertex-action 6)
   e
   (reel/id G)]
  (count (reel/fixed-points act e (range 6)))))


(deftest t26_l129 (is (= v25_l124 6)))


(def
 v28_l133
 (let
  [results
   (for
    [n (range 3 13)]
    (let
     [act (dihedral-vertex-action n)]
     (= 0 (count (reel/fixed-points act [:r 1] (range n))))))]
  (every? true? results)))


(deftest t29_l139 (is (true? v28_l133)))


(def
 v31_l143
 (let
  [results
   (for
    [n (range 3 13)]
    (let
     [act
      (dihedral-vertex-action n)
      fix-count
      (count (reel/fixed-points act [:s 0] (range n)))]
     (if (odd? n) (= fix-count 1) (<= fix-count 2))))]
  (every? true? results)))


(deftest t32_l152 (is (true? v31_l143)))


(def
 v34_l161
 (defn
  all-colorings
  "Generate all k-colorings of n positions."
  [n k]
  (if
   (zero? n)
   [[]]
   (for [rest (all-colorings (dec n) k) c (range k)] (conj rest c)))))


(def
 v35_l170
 (defn
  coloring-action
  "Action of a cyclic group on colorings."
  [n]
  (fn
   [g coloring]
   (mapv
    (fn* [p1__82070#] (coloring (mod (+ p1__82070# (long g)) n)))
    (range n)))))


(def
 v37_l178
 (let
  [results
   (for
    [n (range 2 9) k [2 3]]
    (let
     [G
      (reel/cyclic-group n)
      domain
      (all-colorings n k)
      act
      (coloring-action n)
      actual-orbits
      (count (reel/orbits G act domain))
      burnside
      (reel/burnside-count G act domain)]
     (= actual-orbits burnside)))]
  (every? true? results)))


(deftest t38_l189 (is (true? v37_l178)))


(def
 v40_l193
 (defn
  dihedral-coloring-action
  "Action of D_n on colorings: rotations and reflections."
  [n]
  (let
   [vertex-act (dihedral-vertex-action n)]
   (fn
    [g coloring]
    (mapv
     (fn* [p1__82071#] (coloring (vertex-act g p1__82071#)))
     (range n))))))


(def
 v41_l200
 (let
  [results
   (for
    [n (range 3 9) k [2 3]]
    (let
     [G
      (reel/dihedral-group n)
      domain
      (all-colorings n k)
      act
      (dihedral-coloring-action n)
      actual-orbits
      (count (reel/orbits G act domain))
      burnside
      (reel/burnside-count G act domain)]
     (= actual-orbits burnside)))]
  (every? true? results)))


(deftest t42_l211 (is (true? v41_l200)))


(def v44_l218 (def known-necklaces [2 3 4 6 8 14 20 36 60]))


(def v45_l219 (def known-bracelets [2 3 4 6 8 13 18 30 46]))


(def
 v46_l221
 (let
  [results
   (for
    [n (range 1 (inc (count known-necklaces)))]
    (let
     [G
      (reel/cyclic-group n)
      domain
      (all-colorings n 2)
      act
      (coloring-action n)]
     (=
      (reel/burnside-count G act domain)
      (nth known-necklaces (dec n)))))]
  (every? true? results)))


(deftest t47_l229 (is (true? v46_l221)))


(def
 v48_l231
 (let
  [results
   (for
    [n (range 1 (inc (count known-bracelets)))]
    (let
     [G
      (reel/dihedral-group n)
      domain
      (all-colorings n 2)
      act
      (dihedral-coloring-action n)]
     (=
      (reel/burnside-count G act domain)
      (nth known-bracelets (dec n)))))]
  (every? true? results)))


(deftest t49_l239 (is (true? v48_l231)))


(def
 v51_l250
 (let
  [G
   (reel/cyclic-group 4)
   act
   (rotation-action 4)
   ci
   (reel/cycle-index G act (range 4))]
  (= 1 (reduce + (vals ci)))))


(deftest t52_l255 (is (true? v51_l250)))


(def
 v54_l259
 (let
  [results
   (concat
    (for
     [n (range 2 10)]
     (let
      [G
       (reel/cyclic-group n)
       act
       (rotation-action n)
       ci
       (reel/cycle-index G act (range n))]
      (= 1 (reduce + (vals ci)))))
    (for
     [n (range 3 10)]
     (let
      [G
       (reel/dihedral-group n)
       act
       (dihedral-vertex-action n)
       ci
       (reel/cycle-index G act (range n))]
      (= 1 (reduce + (vals ci))))))]
  (every? true? results)))


(deftest t55_l273 (is (true? v54_l259)))


(def
 v57_l282
 (let
  [results
   (for
    [n (range 2 9) k [2 3 4]]
    (let
     [G
      (reel/cyclic-group n)
      act
      (rotation-action n)
      ci
      (reel/cycle-index G act (range n))
      polya
      (reel/polya-count ci k)
      domain
      (all-colorings n k)
      act-coloring
      (coloring-action n)
      burnside
      (reel/burnside-count G act-coloring domain)]
     (= polya burnside)))]
  (every? true? results)))


(deftest t58_l295 (is (true? v57_l282)))


(def
 v60_l299
 (let
  [results
   (for
    [n (range 3 8) k [2 3]]
    (let
     [G
      (reel/dihedral-group n)
      act
      (dihedral-vertex-action n)
      ci
      (reel/cycle-index G act (range n))
      polya
      (reel/polya-count ci k)
      domain
      (all-colorings n k)
      act-coloring
      (dihedral-coloring-action n)
      burnside
      (reel/burnside-count G act-coloring domain)]
     (= polya burnside)))]
  (every? true? results)))


(deftest t61_l312 (is (true? v60_l299)))


(def
 v63_l318
 (let
  [results
   (for
    [n (range 1 10)]
    (let
     [G
      (reel/cyclic-group n)
      act
      (rotation-action n)
      ci
      (reel/cycle-index G act (range n))]
     (= (reel/polya-count ci 2) (nth known-necklaces (dec n)))))]
  (every? true? results)))


(deftest t64_l327 (is (true? v63_l318)))


(def
 v66_l334
 (let
  [G
   (reel/cyclic-group 100)
   act
   (rotation-action 100)
   ci
   (reel/cycle-index G act (range 100))]
  (reel/polya-count ci 2)))


(deftest t67_l339 (is (= v66_l334 12676506002282305273966813560N)))


(def
 v69_l347
 (let
  [results
   (for
    [n [4 5 6] k (range 1 n)]
    (let
     [G
      (reel/symmetric-group n)
      perm-act
      (fn [sigma x] (sigma x))
      {:keys [act domain]}
      (reel/subset-action perm-act (range n) k)
      orbit-count
      (count (reel/orbits G act domain))]
     (= orbit-count 1)))]
  (every? true? results)))


(deftest t70_l357 (is (true? v69_l347)))


(def
 v72_l363
 (let
  [results
   (for
    [n [5 6 7] k [2 3]]
    (let
     [G
      (reel/cyclic-group n)
      perm-act
      (fn [g x] (mod (+ (long x) (long g)) n))
      {:keys [act domain]}
      (reel/subset-action perm-act (range n) k)
      subset-orbits
      (count (reel/orbits G act domain))
      weighted-colorings
      (filter (fn [c] (= k (reduce + c))) (all-colorings n 2))
      act-coloring
      (coloring-action n)
      weighted-orbits
      (count (reel/orbits G act-coloring weighted-colorings))]
     (= subset-orbits weighted-orbits)))]
  (every? true? results)))


(deftest t73_l378 (is (true? v72_l363)))
