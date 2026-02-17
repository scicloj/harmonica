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
 v34_l167
 (let
  [results
   (for
    [n (range 2 9) k [2 3]]
    (let
     [G
      (reel/cyclic-group n)
      {:keys [domain act]}
      (reel/coloring-action (rotation-action n) n k)
      actual-orbits
      (count (reel/orbits G act domain))
      burnside
      (reel/burnside-count G act domain)]
     (= actual-orbits burnside)))]
  (every? true? results)))


(deftest t35_l177 (is (true? v34_l167)))


(def
 v37_l181
 (let
  [results
   (for
    [n (range 3 9) k [2 3]]
    (let
     [G
      (reel/dihedral-group n)
      {:keys [domain act]}
      (reel/coloring-action (dihedral-vertex-action n) n k)
      actual-orbits
      (count (reel/orbits G act domain))
      burnside
      (reel/burnside-count G act domain)]
     (= actual-orbits burnside)))]
  (every? true? results)))


(deftest t38_l191 (is (true? v37_l181)))


(def v40_l198 (def known-necklaces [2 3 4 6 8 14 20 36 60]))


(def v41_l199 (def known-bracelets [2 3 4 6 8 13 18 30 46]))


(def
 v42_l201
 (let
  [results
   (for
    [n (range 1 (inc (count known-necklaces)))]
    (let
     [G
      (reel/cyclic-group n)
      {:keys [domain act]}
      (reel/coloring-action (rotation-action n) n 2)]
     (=
      (reel/burnside-count G act domain)
      (nth known-necklaces (dec n)))))]
  (every? true? results)))


(deftest t43_l208 (is (true? v42_l201)))


(def
 v44_l210
 (let
  [results
   (for
    [n (range 1 (inc (count known-bracelets)))]
    (let
     [G
      (reel/dihedral-group n)
      {:keys [domain act]}
      (reel/coloring-action (dihedral-vertex-action n) n 2)]
     (=
      (reel/burnside-count G act domain)
      (nth known-bracelets (dec n)))))]
  (every? true? results)))


(deftest t45_l217 (is (true? v44_l210)))


(def
 v47_l228
 (let
  [G
   (reel/cyclic-group 4)
   act
   (rotation-action 4)
   ci
   (reel/cycle-index G act (range 4))]
  (= 1 (reduce + (vals ci)))))


(deftest t48_l233 (is (true? v47_l228)))


(def
 v50_l237
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


(deftest t51_l251 (is (true? v50_l237)))


(def
 v53_l260
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
      {:keys [domain], act-col :act}
      (reel/coloring-action act n k)
      burnside
      (reel/burnside-count G act-col domain)]
     (= polya burnside)))]
  (every? true? results)))


(deftest t54_l272 (is (true? v53_l260)))


(def
 v56_l276
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
      {:keys [domain], act-col :act}
      (reel/coloring-action act n k)
      burnside
      (reel/burnside-count G act-col domain)]
     (= polya burnside)))]
  (every? true? results)))


(deftest t57_l288 (is (true? v56_l276)))


(def
 v59_l294
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


(deftest t60_l303 (is (true? v59_l294)))


(def
 v62_l310
 (let
  [G
   (reel/cyclic-group 100)
   act
   (rotation-action 100)
   ci
   (reel/cycle-index G act (range 100))]
  (reel/polya-count ci 2)))


(deftest t63_l315 (is (= v62_l310 12676506002282305273966813560N)))


(def
 v65_l323
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


(deftest t66_l333 (is (true? v65_l323)))


(def
 v68_l339
 (let
  [results
   (for
    [n [5 6 7] k [2 3]]
    (let
     [G
      (reel/cyclic-group n)
      point-act
      (rotation-action n)
      {:keys [act domain]}
      (reel/subset-action point-act (range n) k)
      subset-orbits
      (count (reel/orbits G act domain))
      {all-cols :domain, act-col :act}
      (reel/coloring-action point-act n 2)
      weighted-colorings
      (filter (fn [c] (= k (reduce + c))) all-cols)
      weighted-orbits
      (count (reel/orbits G act-col weighted-colorings))]
     (= subset-orbits weighted-orbits)))]
  (every? true? results)))


(deftest t69_l353 (is (true? v68_l339)))
