(ns
 harmonica-book.representation-matrices-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.analysis.representations :as rep]
  [scicloj.harmonica.linalg.complex :as cx]
  [fastmath.matrix :as fm]
  [tech.v3.tensor :as tensor]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l30 (def ir-31 (hm/irrep [3 1])))


(def v4_l32 ir-31)


(def v5_l34 (hm/rep-dimension ir-31))


(deftest t6_l36 (is (= v5_l34 3)))


(def v8_l40 (hm/hook-length-dimension [3 1]))


(deftest t9_l42 (is (= v8_l40 3)))


(def
 v10_l44
 (defn
  mat->tensor
  "Convert a fastmath matrix to a dtype-next tensor for display."
  [M]
  (tensor/->tensor (fm/mat->array2d M))))


(def
 v12_l54
 (let
  [perms [[0 1 2 3] [1 0 2 3] [1 2 0 3] [1 2 3 0]]]
  (kind/table
   {:column-names ["σ" "Cycle type" "tr(ρ(σ))" "ρ(σ)"],
    :row-vectors
    (mapv
     (fn
      [sigma]
      [(str sigma)
       (str (hm/cycle-type sigma))
       (format "%.0f" (fm/trace (hm/rep-matrix ir-31 sigma)))
       (mat->tensor (hm/rep-matrix ir-31 sigma))])
     perms)})))


(def
 v14_l78
 (let
  [gens (hm/rep-generators ir-31)]
  (kind/table
   {:column-names ["Generator" "Matrix"],
    :row-vectors
    (mapv
     (fn [i g] [(str "$s_" (inc i) "$") (mat->tensor g)])
     (range)
     gens)})))


(def
 v16_l93
 (defn
  mat-err
  "Frobenius norm of the difference of two matrices."
  [A B]
  (let
   [diff (fm/sub A B)]
   (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff)))))))


(def
 v17_l99
 (let
  [G (hm/symmetric-group 4) elts (vec (hm/elements G))]
  (every?
   (fn
    [[s t]]
    (let
     [st
      (hm/op G s t)
      rho-st
      (hm/rep-matrix ir-31 st)
      rho-s-t
      (fm/mulm (hm/rep-matrix ir-31 s) (hm/rep-matrix ir-31 t))]
     (< (mat-err rho-st rho-s-t) 1.0E-10)))
   (for [a elts b elts] [a b]))))


(deftest t18_l109 (is (true? v17_l99)))


(def
 v20_l116
 (defn
  identity-matrix
  [d]
  (fm/rows->mat
   (mapv
    (fn [i] (mapv (fn [j] (if (= i j) 1.0 0.0)) (range d)))
    (range d)))))


(def
 v21_l120
 (let
  [G
   (hm/symmetric-group 4)
   d
   (hm/rep-dimension ir-31)
   I
   (identity-matrix d)]
  (every?
   (fn
    [sigma]
    (let
     [M (hm/rep-matrix ir-31 sigma) MtM (fm/mulm (fm/transpose M) M)]
     (< (mat-err MtM I) 1.0E-10)))
   (hm/elements G))))


(deftest t22_l129 (is (true? v21_l120)))


(def
 v24_l136
 (let
  [d
   (hm/rep-dimension ir-31)
   I
   (identity-matrix d)
   rho-e
   (hm/rep-matrix ir-31 (hm/identity-perm 4))]
  (< (mat-err rho-e I) 1.0E-10)))


(deftest t25_l141 (is (true? v24_l136)))


(def
 v26_l143
 (let
  [G (hm/symmetric-group 4) sigma [2 0 3 1]]
  (<
   (mat-err
    (hm/rep-matrix ir-31 (hm/inv G sigma))
    (fm/transpose (hm/rep-matrix ir-31 sigma)))
   1.0E-10)))


(deftest t27_l149 (is (true? v26_l143)))


(def
 v29_l162
 (let
  [gens
   (hm/rep-generators ir-31)
   d
   (hm/rep-dimension ir-31)
   I
   (identity-matrix d)
   involution-ok?
   (every? (fn [gi] (< (mat-err (fm/mulm gi gi) I) 1.0E-10)) gens)
   braid-ok?
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
   far-comm-ok?
   (every?
    (fn
     [[i j]]
     (let
      [si (gens i) sj (gens j)]
      (< (mat-err (fm/mulm si sj) (fm/mulm sj si)) 1.0E-10)))
    (for
     [i
      (range (count gens))
      j
      (range (count gens))
      :when
      (>= (Math/abs (- i j)) 2)]
     [i j]))]
  {:involution involution-ok?,
   :braid braid-ok?,
   :far-commutativity far-comm-ok?}))


(deftest
 t30_l189
 (is ((fn [result] (every? true? (vals result))) v29_l162)))


(def
 v32_l201
 (let
  [G
   (hm/symmetric-group 3)
   parts
   (hm/partitions 3)
   irreps
   (mapv hm/irrep parts)
   elts
   (vec (hm/elements G))
   rng
   (java.util.Random. 42)
   f
   (into {} (map (fn [sigma] [sigma (.nextGaussian rng)]) elts))
   lhs
   (rep/plancherel-lhs G f)
   f-hats
   (rep/matrix-fourier-transform-all G f irreps)
   rhs
   (rep/plancherel-rhs G f-hats irreps)]
  {:lhs (format "%.6f" lhs),
   :rhs (format "%.6f" rhs),
   :difference (Math/abs (- lhs rhs))}))


(deftest
 t33_l216
 (is ((fn [result] (< (:difference result) 1.0E-10)) v32_l201)))


(def
 v35_l230
 (let
  [ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [2 1])
   tp
   (hm/tensor-product ir1 ir2)]
  {:dim-ir1 (hm/rep-dimension ir1),
   :dim-ir2 (hm/rep-dimension ir2),
   :dim-tensor (hm/rep-dimension tp)}))


(deftest
 t36_l237
 (is
  ((fn
    [{:keys [dim-ir1 dim-ir2 dim-tensor]}]
    (= dim-tensor (* dim-ir1 dim-ir2)))
   v35_l230)))


(def
 v38_l243
 (let
  [G
   (hm/symmetric-group 3)
   ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [2 1])
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
   (hm/elements G))))


(deftest t39_l254 (is (true? v38_l243)))


(def
 v41_l258
 (let
  [ir1
   (hm/irrep [2 1])
   ir2
   (hm/irrep [1 1 1])
   ds
   (hm/direct-sum ir1 ir2)
   G
   (hm/symmetric-group 3)]
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
   (hm/elements G))))


(deftest t42_l269 (is (true? v41_l258)))


(def
 v44_l286
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
      (range (inc a) (count irreps))
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
  {:cross-irrep cross-ok?, :same-irrep same-ok?}))


(deftest
 t45_l328
 (is ((fn [result] (every? true? (vals result))) v44_l286)))
