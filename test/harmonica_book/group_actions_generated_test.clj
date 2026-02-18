(ns
 harmonica-book.group-actions-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l27
 (defn rotation-action [n] (fn [g x] (mod (+ (long x) (long g)) n))))


(def
 v5_l33
 (let
  [G (hm/cyclic-group 5) act (rotation-action 5)]
  (hm/orbit G act 0)))


(deftest t6_l37 (is (= v5_l33 #{0 1 4 3 2})))


(def
 v8_l41
 (let
  [G (hm/cyclic-group 5) act (rotation-action 5)]
  (count (hm/orbits G act (range 5)))))


(deftest t9_l45 (is (= v8_l41 1)))


(def
 v11_l54
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
 v13_l62
 (let
  [G (hm/dihedral-group 4) act (dihedral-vertex-action 4)]
  (count (hm/orbits G act (range 4)))))


(deftest t14_l66 (is (= v13_l62 1)))


(def
 v16_l76
 (let
  [results
   (for
    [n (range 3 13)]
    (let
     [G
      (hm/dihedral-group n)
      act
      (dihedral-vertex-action n)
      order
      (hm/order G)]
     (every?
      (fn
       [x]
       (=
        order
        (*
         (count (hm/orbit G act x))
         (count (hm/stabilizer G act x)))))
      (range n))))]
  (every? true? results)))


(deftest t17_l87 (is (true? v16_l76)))


(def
 v19_l91
 (let
  [results
   (for
    [n [2 3 5 7 11 13]]
    (let
     [G (hm/cyclic-group n) act (rotation-action n)]
     (every?
      (fn
       [x]
       (=
        n
        (*
         (count (hm/orbit G act x))
         (count (hm/stabilizer G act x)))))
      (range n))))]
  (every? true? results)))


(deftest t20_l101 (is (true? v19_l91)))


(def
 v22_l108
 (let
  [results
   (for
    [n [4 5] k [2 3]]
    (let
     [G
      (hm/symmetric-group n)
      perm-act
      (fn [sigma x] (sigma x))
      {:keys [act domain]}
      (hm/subset-action perm-act (range n) k)]
     (every?
      (fn
       [x]
       (=
        (hm/order G)
        (*
         (count (hm/orbit G act x))
         (count (hm/stabilizer G act x)))))
      (take 5 domain))))]
  (every? true? results)))


(deftest t23_l122 (is (true? v22_l108)))


(def
 v25_l130
 (let
  [G (hm/dihedral-group 6) act (dihedral-vertex-action 6) e (hm/id G)]
  (count (hm/fixed-points act e (range 6)))))


(deftest t26_l135 (is (= v25_l130 6)))


(def
 v28_l139
 (let
  [results
   (for
    [n (range 3 13)]
    (let
     [act (dihedral-vertex-action n)]
     (= 0 (count (hm/fixed-points act [:r 1] (range n))))))]
  (every? true? results)))


(deftest t29_l145 (is (true? v28_l139)))


(def
 v31_l149
 (let
  [results
   (for
    [n (range 3 13)]
    (let
     [act
      (dihedral-vertex-action n)
      fix-count
      (count (hm/fixed-points act [:s 0] (range n)))]
     (if (odd? n) (= fix-count 1) (<= fix-count 2))))]
  (every? true? results)))


(deftest t32_l158 (is (true? v31_l149)))


(def
 v34_l173
 (let
  [results
   (for
    [n (range 2 9) k [2 3]]
    (let
     [G
      (hm/cyclic-group n)
      {:keys [domain act]}
      (hm/coloring-action (rotation-action n) n k)
      actual-orbits
      (count (hm/orbits G act domain))
      burnside
      (hm/burnside-count G act domain)]
     (= actual-orbits burnside)))]
  (every? true? results)))


(deftest t35_l183 (is (true? v34_l173)))


(def
 v37_l187
 (let
  [results
   (for
    [n (range 3 9) k [2 3]]
    (let
     [G
      (hm/dihedral-group n)
      {:keys [domain act]}
      (hm/coloring-action (dihedral-vertex-action n) n k)
      actual-orbits
      (count (hm/orbits G act domain))
      burnside
      (hm/burnside-count G act domain)]
     (= actual-orbits burnside)))]
  (every? true? results)))


(deftest t38_l197 (is (true? v37_l187)))


(def v40_l204 (def known-necklaces [2 3 4 6 8 14 20 36 60]))


(def v41_l205 (def known-bracelets [2 3 4 6 8 13 18 30 46]))


(def
 v42_l207
 (let
  [results
   (for
    [n (range 1 (inc (count known-necklaces)))]
    (let
     [G
      (hm/cyclic-group n)
      {:keys [domain act]}
      (hm/coloring-action (rotation-action n) n 2)]
     (=
      (hm/burnside-count G act domain)
      (nth known-necklaces (dec n)))))]
  (every? true? results)))


(deftest t43_l214 (is (true? v42_l207)))


(def
 v44_l216
 (let
  [results
   (for
    [n (range 1 (inc (count known-bracelets)))]
    (let
     [G
      (hm/dihedral-group n)
      {:keys [domain act]}
      (hm/coloring-action (dihedral-vertex-action n) n 2)]
     (=
      (hm/burnside-count G act domain)
      (nth known-bracelets (dec n)))))]
  (every? true? results)))


(deftest t45_l223 (is (true? v44_l216)))


(def
 v47_l234
 (let
  [G
   (hm/cyclic-group 4)
   act
   (rotation-action 4)
   ci
   (hm/cycle-index G act (range 4))]
  (= 1 (reduce + (vals ci)))))


(deftest t48_l239 (is (true? v47_l234)))


(def
 v50_l243
 (let
  [results
   (concat
    (for
     [n (range 2 10)]
     (let
      [G
       (hm/cyclic-group n)
       act
       (rotation-action n)
       ci
       (hm/cycle-index G act (range n))]
      (= 1 (reduce + (vals ci)))))
    (for
     [n (range 3 10)]
     (let
      [G
       (hm/dihedral-group n)
       act
       (dihedral-vertex-action n)
       ci
       (hm/cycle-index G act (range n))]
      (= 1 (reduce + (vals ci))))))]
  (every? true? results)))


(deftest t51_l257 (is (true? v50_l243)))


(def
 v53_l266
 (let
  [results
   (for
    [n (range 2 9) k [2 3 4]]
    (let
     [G
      (hm/cyclic-group n)
      act
      (rotation-action n)
      ci
      (hm/cycle-index G act (range n))
      polya
      (hm/polya-count ci k)
      {:keys [domain], act-col :act}
      (hm/coloring-action act n k)
      burnside
      (hm/burnside-count G act-col domain)]
     (= polya burnside)))]
  (every? true? results)))


(deftest t54_l278 (is (true? v53_l266)))


(def
 v56_l282
 (let
  [results
   (for
    [n (range 3 8) k [2 3]]
    (let
     [G
      (hm/dihedral-group n)
      act
      (dihedral-vertex-action n)
      ci
      (hm/cycle-index G act (range n))
      polya
      (hm/polya-count ci k)
      {:keys [domain], act-col :act}
      (hm/coloring-action act n k)
      burnside
      (hm/burnside-count G act-col domain)]
     (= polya burnside)))]
  (every? true? results)))


(deftest t57_l294 (is (true? v56_l282)))


(def
 v59_l300
 (let
  [results
   (for
    [n (range 1 10)]
    (let
     [G
      (hm/cyclic-group n)
      act
      (rotation-action n)
      ci
      (hm/cycle-index G act (range n))]
     (= (hm/polya-count ci 2) (nth known-necklaces (dec n)))))]
  (every? true? results)))


(deftest t60_l309 (is (true? v59_l300)))


(def
 v62_l316
 (let
  [G
   (hm/cyclic-group 100)
   act
   (rotation-action 100)
   ci
   (hm/cycle-index G act (range 100))]
  (hm/polya-count ci 2)))


(deftest t63_l321 (is (= v62_l316 12676506002282305273966813560N)))


(def
 v65_l329
 (let
  [results
   (for
    [n [4 5 6] k (range 1 n)]
    (let
     [G
      (hm/symmetric-group n)
      perm-act
      (fn [sigma x] (sigma x))
      {:keys [act domain]}
      (hm/subset-action perm-act (range n) k)
      orbit-count
      (count (hm/orbits G act domain))]
     (= orbit-count 1)))]
  (every? true? results)))


(deftest t66_l339 (is (true? v65_l329)))


(def
 v68_l345
 (let
  [results
   (for
    [n [5 6 7] k [2 3]]
    (let
     [G
      (hm/cyclic-group n)
      point-act
      (rotation-action n)
      {:keys [act domain]}
      (hm/subset-action point-act (range n) k)
      subset-orbits
      (count (hm/orbits G act domain))
      {all-cols :domain, act-col :act}
      (hm/coloring-action point-act n 2)
      weighted-colorings
      (filter (fn [c] (= k (reduce + c))) all-cols)
      weighted-orbits
      (count (hm/orbits G act-col weighted-colorings))]
     (= subset-orbits weighted-orbits)))]
  (every? true? results)))


(deftest t69_l359 (is (true? v68_l345)))
