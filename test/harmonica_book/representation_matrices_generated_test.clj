(ns
 harmonica-book.representation-matrices-generated-test
 (:require
  [scicloj.harmonica.core :as hm]
  [scicloj.harmonica.representations :as rep]
  [fastmath.complex :as c]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l25 (def ir-21 (hm/irrep [2 1])))


(def v4_l27 (hm/rep-dimension ir-21))


(deftest t5_l29 (is (= v4_l27 2)))


(def v6_l31 (def ir-31 (hm/irrep [3 1])))


(def v7_l33 (hm/rep-dimension ir-31))


(deftest t8_l35 (is (= v7_l33 3)))


(def
 v10_l39
 (let
  [results
   (for
    [n (range 2 8) lambda (hm/partitions n)]
    (=
     (hm/rep-dimension (hm/irrep lambda))
     (hm/hook-length-dimension lambda)))]
  (every? true? results)))


(deftest t11_l46 (is (true? v10_l39)))


(def
 v13_l53
 (let
  [results
   (for
    [n [3 4 5] lambda (hm/partitions n)]
    (let
     [G
      (hm/symmetric-group n)
      ir
      (hm/irrep lambda)
      elts
      (vec (hm/elements G))
      pairs
      (if
       (<= (count elts) 24)
       (for [a elts b elts] [a b])
       (let
        [rng (java.util.Random. 42)]
        (repeatedly
         300
         (fn
          []
          [(elts (.nextInt rng (count elts)))
           (elts (.nextInt rng (count elts)))]))))]
     (every?
      (fn
       [[s t]]
       (let
        [st
         (hm/op G s t)
         rho-st
         (hm/rep-matrix ir st)
         rho-s-t
         (fm/mulm (hm/rep-matrix ir s) (hm/rep-matrix ir t))
         diff
         (fm/sub rho-st rho-s-t)
         err
         (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
        (< err 1.0E-10)))
      pairs)))]
  (every? true? results)))


(deftest t14_l77 (is (true? v13_l53)))


(def
 v16_l84
 (defn
  identity-matrix
  "Identity matrix as fastmath RealMatrix."
  [d]
  (fm/rows->mat
   (mapv
    (fn [i] (mapv (fn [j] (if (= i j) 1.0 0.0)) (range d)))
    (range d)))))


(def
 v17_l90
 (let
  [results
   (for
    [n [3 4 5] lambda (hm/partitions n)]
    (let
     [G
      (hm/symmetric-group n)
      ir
      (hm/irrep lambda)
      d
      (hm/rep-dimension ir)
      I
      (identity-matrix d)]
     (every?
      (fn
       [sigma]
       (let
        [M
         (hm/rep-matrix ir sigma)
         MtM
         (fm/mulm (fm/transpose M) M)
         diff
         (fm/sub MtM I)
         err
         (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
        (< err 1.0E-10)))
      (hm/elements G))))]
  (every? true? results)))


(deftest t18_l106 (is (true? v17_l90)))


(def
 v20_l115
 (let
  [results
   (for
    [n [3 4 5 6]]
    (let
     [G
      (hm/symmetric-group n)
      ct
      (hm/character-table G)
      classes
      (:classes ct)
      class-idx
      (into {} (map-indexed (fn [i c] [c i]) classes))
      labels
      (:irrep-labels ct)]
     (every?
      identity
      (for
       [lambda (hm/partitions n)]
       (let
        [ir
         (hm/irrep lambda)
         row-idx
         (.indexOf labels lambda)
         row
         (nth (:table ct) row-idx)]
        (every?
         (fn
          [sigma]
          (let
           [ct-idx
            (class-idx (hm/cycle-type sigma))
            chi-val
            (c/re (nth row ct-idx))
            trace-val
            (hm/rep-character ir sigma)]
           (< (Math/abs (- chi-val trace-val)) 1.0E-8)))
         (hm/elements G)))))))]
  (every? true? results)))


(deftest t21_l135 (is (true? v20_l115)))


(def
 v23_l141
 (let
  [results
   (for
    [n [3 4 5 6] lambda (hm/partitions n)]
    (let
     [ir
      (hm/irrep lambda)
      d
      (hm/rep-dimension ir)
      I
      (identity-matrix d)
      rho-e
      (hm/rep-matrix ir (hm/identity-perm n))
      diff
      (fm/sub rho-e I)
      err
      (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
     (< err 1.0E-10)))]
  (every? true? results)))


(deftest t24_l153 (is (true? v23_l141)))


(def
 v26_l159
 (let
  [results
   (for
    [n [3 4 5] lambda (hm/partitions n)]
    (let
     [G (hm/symmetric-group n) ir (hm/irrep lambda)]
     (every?
      (fn
       [sigma]
       (let
        [rho-inv
         (hm/rep-matrix ir (hm/inv G sigma))
         rho-t
         (fm/transpose (hm/rep-matrix ir sigma))
         diff
         (fm/sub rho-inv rho-t)
         err
         (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
        (< err 1.0E-10)))
      (hm/elements G))))]
  (every? true? results)))


(deftest t27_l173 (is (true? v26_l159)))


(def
 v29_l185
 (let
  [results
   (for
    [n [3 4 5 6] lambda (hm/partitions n)]
    (let
     [ir
      (hm/irrep lambda)
      gens
      (hm/rep-generators ir)
      d
      (hm/rep-dimension ir)
      I
      (identity-matrix d)
      mat-err
      (fn
       [A B]
       (let
        [diff (fm/sub A B)]
        (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))))]
     (and
      (every? (fn [gi] (< (mat-err (fm/mulm gi gi) I) 1.0E-10)) gens)
      (every?
       (fn
        [i]
        (let
         [si
          (gens i)
          sj
          (gens (inc i))
          lhs
          (fm/mulm si (fm/mulm sj si))
          rhs
          (fm/mulm sj (fm/mulm si sj))]
         (< (mat-err lhs rhs) 1.0E-10)))
       (range (- (count gens) 1)))
      (every?
       (fn
        [[i j]]
        (let
         [si
          (gens i)
          sj
          (gens j)
          lhs
          (fm/mulm si sj)
          rhs
          (fm/mulm sj si)]
         (< (mat-err lhs rhs) 1.0E-10)))
       (for
        [i
         (range (count gens))
         j
         (range (count gens))
         :when
         (>= (Math/abs (- i j)) 2)]
        [i j])))))]
  (every? true? results)))


(deftest t30_l221 (is (true? v29_l185)))


(def
 v32_l229
 (let
  [results
   (for
    [n [3 4] seed [42 123 7]]
    (let
     [G
      (hm/symmetric-group n)
      parts
      (hm/partitions n)
      irreps
      (mapv hm/irrep parts)
      elts
      (vec (hm/elements G))
      rng
      (java.util.Random. seed)
      f
      (into {} (map (fn [sigma] [sigma (.nextGaussian rng)]) elts))
      lhs
      (rep/plancherel-lhs G f)
      f-hats
      (rep/matrix-fourier-transform-all G f irreps)
      rhs
      (rep/plancherel-rhs G f-hats irreps)]
     (< (Math/abs (- lhs rhs)) 1.0E-8)))]
  (every? true? results)))


(deftest t33_l246 (is (true? v32_l229)))


(def
 v35_l255
 (let
  [results
   (for
    [n [3 4 5] l1 (hm/partitions n) l2 (hm/partitions n)]
    (let
     [ir1
      (hm/irrep l1)
      ir2
      (hm/irrep l2)
      tp
      (hm/tensor-product ir1 ir2)]
     (=
      (hm/rep-dimension tp)
      (* (hm/rep-dimension ir1) (hm/rep-dimension ir2)))))]
  (every? true? results)))


(deftest t36_l266 (is (true? v35_l255)))


(def
 v38_l272
 (let
  [results
   (for
    [n [3 4] l1 (hm/partitions n) l2 (hm/partitions n)]
    (let
     [G
      (hm/symmetric-group n)
      ir1
      (hm/irrep l1)
      ir2
      (hm/irrep l2)
      tp
      (hm/tensor-product ir1 ir2)]
     (every?
      (fn
       [sigma]
       (let
        [c1
         (hm/rep-character ir1 sigma)
         c2
         (hm/rep-character ir2 sigma)
         c-tp
         (fm/trace (hm/rep-matrix tp sigma))]
        (< (Math/abs (- c-tp (* c1 c2))) 1.0E-8)))
      (hm/elements G))))]
  (every? true? results)))


(deftest t39_l288 (is (true? v38_l272)))


(def
 v41_l292
 (let
  [results
   (for
    [n
     [3 4]
     [l1 l2]
     (take
      5
      (for
       [a (hm/partitions n) b (hm/partitions n) :when (not= a b)]
       [a b]))]
    (let
     [G
      (hm/symmetric-group n)
      ir1
      (hm/irrep l1)
      ir2
      (hm/irrep l2)
      tp
      (hm/tensor-product ir1 ir2)
      elts
      (vec (hm/elements G))]
     (every?
      (fn
       [[s t]]
       (let
        [st
         (hm/op G s t)
         rho-st
         (hm/rep-matrix tp st)
         rho-s-t
         (fm/mulm (hm/rep-matrix tp s) (hm/rep-matrix tp t))
         diff
         (fm/sub rho-st rho-s-t)
         err
         (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
        (< err 1.0E-10)))
      (for [a elts b elts] [a b]))))]
  (every? true? results)))


(deftest t42_l314 (is (true? v41_l292)))


(def
 v44_l323
 (let
  [results
   (for
    [n [3 4 5] l1 (hm/partitions n) l2 (hm/partitions n)]
    (let
     [ir1 (hm/irrep l1) ir2 (hm/irrep l2) ds (hm/direct-sum ir1 ir2)]
     (=
      (hm/rep-dimension ds)
      (+ (hm/rep-dimension ir1) (hm/rep-dimension ir2)))))]
  (every? true? results)))


(deftest t45_l334 (is (true? v44_l323)))


(def
 v47_l340
 (let
  [results
   (for
    [n [3 4] l1 (hm/partitions n) l2 (hm/partitions n)]
    (let
     [G
      (hm/symmetric-group n)
      ir1
      (hm/irrep l1)
      ir2
      (hm/irrep l2)
      ds
      (hm/direct-sum ir1 ir2)]
     (every?
      (fn
       [sigma]
       (let
        [c1
         (hm/rep-character ir1 sigma)
         c2
         (hm/rep-character ir2 sigma)
         c-ds
         (fm/trace (hm/rep-matrix ds sigma))]
        (< (Math/abs (- c-ds (+ c1 c2))) 1.0E-8)))
      (hm/elements G))))]
  (every? true? results)))


(deftest t48_l356 (is (true? v47_l340)))


(def
 v50_l370
 (let
  [G
   (hm/symmetric-group 4)
   order
   (hm/order G)
   elts
   (vec (hm/elements G))
   parts
   (hm/partitions 4)
   irreps
   (mapv hm/irrep parts)
   cross-ok?
   (every?
    true?
    (for
     [a
      (range (count irreps))
      b
      (range (count irreps))
      :when
      (< a b)
      :let
      [ira
       (irreps a)
       irb
       (irreps b)
       da
       (hm/rep-dimension ira)
       db
       (hm/rep-dimension irb)]
      i
      (range da)
      j
      (range da)
      k
      (range db)
      l
      (range db)]
     (let
      [avg
       (/
        (reduce
         +
         (map
          (fn
           [sigma]
           (let
            [Ma (hm/rep-matrix ira sigma) Mb (hm/rep-matrix irb sigma)]
            (* (fm/entry Ma i j) (fm/entry Mb k l))))
          elts))
        (double order))]
      (< (Math/abs avg) 1.0E-8))))
   same-ok?
   (every?
    true?
    (for
     [a
      (range (count irreps))
      :let
      [ira (irreps a) da (hm/rep-dimension ira)]
      i
      (range da)
      j
      (range da)
      k
      (range da)
      l
      (range da)]
     (let
      [avg
       (/
        (reduce
         +
         (map
          (fn
           [sigma]
           (let
            [M (hm/rep-matrix ira sigma)]
            (* (fm/entry M i j) (fm/entry M k l))))
          elts))
        (double order))
       expected
       (if (and (= i k) (= j l)) (/ 1.0 (double da)) 0.0)]
      (< (Math/abs (- avg expected)) 1.0E-8))))]
  (and cross-ok? same-ok?)))


(deftest t51_l415 (is (true? v50_l370)))
