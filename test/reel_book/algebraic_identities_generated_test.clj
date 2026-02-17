(ns
 reel-book.algebraic-identities-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.reel.protocols :as p]
  [scicloj.reel.representations :as rep]
  [fastmath.complex :as c]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l22
 (def
  test-groups
  "A collection of groups for systematic testing."
  [{:label "Z/2Z", :group (reel/cyclic-group 2), :has-ct? true}
   {:label "Z/5Z", :group (reel/cyclic-group 5), :has-ct? true}
   {:label "Z/7Z", :group (reel/cyclic-group 7), :has-ct? true}
   {:label "Z/12Z", :group (reel/cyclic-group 12), :has-ct? true}
   {:label "S_3", :group (reel/symmetric-group 3), :has-ct? true}
   {:label "S_4", :group (reel/symmetric-group 4), :has-ct? true}
   {:label "S_5", :group (reel/symmetric-group 5), :has-ct? true}
   {:label "D_3", :group (reel/dihedral-group 3), :has-ct? true}
   {:label "D_4", :group (reel/dihedral-group 4), :has-ct? true}
   {:label "D_5", :group (reel/dihedral-group 5), :has-ct? true}
   {:label "D_6", :group (reel/dihedral-group 6), :has-ct? true}
   {:label "D_8", :group (reel/dihedral-group 8), :has-ct? true}
   {:label "Z/2Z × Z/3Z",
    :group
    (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 3)),
    :has-ct? false}
   {:label "Z/2Z × Z/2Z",
    :group
    (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 2)),
    :has-ct? false}
   {:label "Z/3Z × Z/4Z",
    :group
    (reel/product-group (reel/cyclic-group 3) (reel/cyclic-group 4)),
    :has-ct? false}]))


(def
 v4_l46
 (def
  ct-groups
  "Groups that have character-table implementations."
  (filterv :has-ct? test-groups)))


(def
 v6_l56
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [e
       (reel/id group)
       ok?
       (every?
        (fn
         [g]
         (and (= (reel/op group e g) g) (= (reel/op group g e) g)))
        (reel/elements group))]
      {:group label, :pass? ok?}))
    test-groups)]
  (every? :pass? results)))


(deftest t7_l67 (is (true? v6_l56)))


(def
 v9_l71
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [e
       (reel/id group)
       ok?
       (every?
        (fn
         [g]
         (let
          [gi (reel/inv group g)]
          (and (= (reel/op group g gi) e) (= (reel/op group gi g) e))))
        (reel/elements group))]
      {:group label, :pass? ok?}))
    test-groups)]
  (every? :pass? results)))


(deftest t10_l83 (is (true? v9_l71)))


(def
 v12_l89
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [elts
       (vec (reel/elements group))
       triples
       (if
        (<= (count elts) 24)
        (for [a elts b elts c elts] [a b c])
        (let
         [rng (java.util.Random. 42)]
         (repeatedly
          500
          (fn
           []
           [(elts (.nextInt rng (count elts)))
            (elts (.nextInt rng (count elts)))
            (elts (.nextInt rng (count elts)))]))))
       ok?
       (every?
        (fn
         [[a b c]]
         (=
          (reel/op group (reel/op group a b) c)
          (reel/op group a (reel/op group b c))))
        triples)]
      {:group label, :pass? ok?}))
    test-groups)]
  (every? :pass? results)))


(deftest t13_l109 (is (true? v12_l89)))


(def
 v15_l115
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [classes
       (reel/conjugacy-classes group)
       total
       (reduce + (map :size classes))]
      {:group label, :pass? (= total (reel/order group))}))
    test-groups)]
  (every? :pass? results)))


(deftest t16_l123 (is (true? v15_l115)))


(def
 v18_l127
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [classes
       (reel/conjugacy-classes group)
       all-elts
       (mapcat :elements classes)
       group-set
       (set (reel/elements group))]
      {:group label,
       :pass?
       (and
        (= (count all-elts) (count (set all-elts)))
        (= (set all-elts) group-set))}))
    test-groups)]
  (every? :pass? results)))


(deftest t19_l138 (is (true? v18_l127)))


(def
 v21_l151
 (defn
  row-orthogonality-check
  "Check row orthogonality for all pairs of irreps."
  [{:keys [label group]}]
  (let
   [ct
    (reel/character-table group)
    {:keys [table class-sizes]}
    ct
    n-irreps
    (count table)
    order
    (reel/order group)
    tol
    1.0E-8]
   (every?
    identity
    (for
     [i (range n-irreps) j (range n-irreps)]
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
      (< (Math/abs (- ip expected)) tol)))))))


(def v22_l173 (every? row-orthogonality-check ct-groups))


(deftest t23_l175 (is (true? v22_l173)))


(def
 v25_l181
 (defn
  column-orthogonality-check
  "Check column orthogonality for all pairs of conjugacy classes."
  [{:keys [label group]}]
  (let
   [ct
    (reel/character-table group)
    {:keys [table class-sizes]}
    ct
    n-classes
    (count class-sizes)
    order
    (reel/order group)
    tol
    1.0E-8]
   (every?
    identity
    (for
     [i (range n-classes) j (range n-classes)]
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
      (< (Math/abs (- ip expected)) tol)))))))


(def v26_l203 (every? column-orthogonality-check ct-groups))


(deftest t27_l205 (is (true? v26_l203)))


(def
 v29_l209
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [ct
       (reel/character-table group)
       dims
       (mapv (fn [row] (let [d (first row)] (c/re d))) (:table ct))
       dim-sq-sum
       (reduce
        +
        (map (fn* [p1__75697#] (* p1__75697# p1__75697#)) dims))]
      {:group label,
       :pass?
       (<
        (Math/abs (- dim-sq-sum (double (reel/order group))))
        1.0E-8)}))
    ct-groups)]
  (every? :pass? results)))


(deftest t30_l222 (is (true? v29_l209)))


(def
 v32_l226
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [ct
       (reel/character-table group)
       n-irreps
       (count (:table ct))
       n-classes
       (count (reel/conjugacy-classes group))]
      {:group label, :pass? (= n-irreps n-classes)}))
    ct-groups)]
  (every? :pass? results)))


(deftest t33_l235 (is (true? v32_l226)))


(def
 v35_l239
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [ct
       (reel/character-table group)
       trivial-row
       (first (:table ct))
       ok?
       (every?
        (fn
         [chi-val]
         (< (c/abs (c/sub chi-val (c/complex 1.0 0.0))) 1.0E-8))
        trivial-row)]
      {:group label, :pass? ok?}))
    ct-groups)]
  (every? :pass? results)))


(deftest t36_l251 (is (true? v35_l239)))


(def
 v38_l255
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [ct
       (reel/character-table group)
       ok?
       (every?
        (fn
         [row]
         (let
          [d (c/re (first row))]
          (< (Math/abs (- d (Math/round d))) 1.0E-8)))
        (:table ct))]
      {:group label, :pass? ok?}))
    ct-groups)]
  (every? :pass? results)))


(deftest t39_l267 (is (true? v38_l255)))


(def
 v41_l274
 (def
  abelian-groups
  "Abelian groups for Fourier testing."
  [{:label "Z/5Z", :group (reel/cyclic-group 5)}
   {:label "Z/7Z", :group (reel/cyclic-group 7)}
   {:label "Z/12Z", :group (reel/cyclic-group 12)}
   {:label "Z/16Z", :group (reel/cyclic-group 16)}]))


(def
 v43_l283
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [n
       (reel/order group)
       ct
       (reel/character-table group)
       f-vals
       (mapv (fn [i] (c/complex (double (inc i)) 0.0)) (range n))
       f-hat
       (reel/fourier-transform ct f-vals)
       f-back
       (reel/inverse-fourier-transform ct f-hat)
       max-err
       (apply
        max
        (map
         (fn [orig back] (c/abs (c/sub back orig)))
         f-vals
         f-back))]
      {:group label, :pass? (< max-err 1.0E-10)}))
    abelian-groups)]
  (every? :pass? results)))


(deftest t44_l297 (is (true? v43_l283)))


(def
 v46_l301
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [n
       (reel/order group)
       ct
       (reel/character-table group)
       f-vals
       (mapv
        (fn
         [i]
         (c/complex (Math/sin (* 2.0 Math/PI (/ i (double n)))) 0.0))
        (range n))
       f-hat
       (reel/fourier-transform ct f-vals)
       lhs
       (reduce
        +
        (map
         (fn* [p1__75698#] (let [a (c/abs p1__75698#)] (* a a)))
         f-vals))
       rhs
       (*
        (/ 1.0 (double n))
        (reduce
         +
         (map
          (fn* [p1__75699#] (let [a (c/abs p1__75699#)] (* a a)))
          f-hat)))]
      {:group label, :pass? (< (Math/abs (- lhs rhs)) 1.0E-8)}))
    abelian-groups)]
  (every? :pass? results)))


(deftest t47_l315 (is (true? v46_l301)))


(def
 v49_l319
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [n
       (reel/order group)
       ct
       (reel/character-table group)
       f
       (mapv (fn [i] (c/complex (if (< i 3) 1.0 0.0) 0.0)) (range n))
       g
       (mapv
        (fn [i] (c/complex (/ 1.0 (inc (double i))) 0.0))
        (range n))
       conv
       (reel/convolve ct f g)
       f-hat
       (reel/fourier-transform ct f)
       g-hat
       (reel/fourier-transform ct g)
       conv-hat
       (reel/fourier-transform ct conv)
       pointwise
       (mapv c/mult f-hat g-hat)
       max-err
       (apply
        max
        (map
         (fn*
          [p1__75700# p2__75701#]
          (c/abs (c/sub p1__75700# p2__75701#)))
         conv-hat
         pointwise))]
      {:group label, :pass? (< max-err 1.0E-8)}))
    abelian-groups)]
  (every? :pass? results)))


(deftest t50_l335 (is (true? v49_l319)))


(def
 v52_l344
 (let
  [results
   (for
    [n [3 4 5] lambda (reel/partitions n)]
    (let
     [G
      (reel/symmetric-group n)
      ir
      (reel/irrep lambda)
      elts
      (vec (reel/elements G))
      pairs
      (if
       (<= (count elts) 24)
       (for [a elts b elts] [a b])
       (let
        [rng (java.util.Random. 42)]
        (repeatedly
         200
         (fn
          []
          [(elts (.nextInt rng (count elts)))
           (elts (.nextInt rng (count elts)))]))))
      ok?
      (every?
       (fn
        [[s t]]
        (let
         [st
          (reel/op G s t)
          rho-st
          (reel/rep-matrix ir st)
          rho-s-rho-t
          (fm/mulm (reel/rep-matrix ir s) (reel/rep-matrix ir t))
          diff
          (fm/sub rho-st rho-s-rho-t)
          err
          (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
         (< err 1.0E-10)))
       pairs)]
     ok?))]
  (every? identity results)))


(deftest t53_l368 (is (true? v52_l344)))


(def
 v55_l374
 (let
  [results
   (for
    [n [3 4 5] lambda (reel/partitions n)]
    (let
     [G
      (reel/symmetric-group n)
      ir
      (reel/irrep lambda)
      d
      (reel/rep-dimension ir)
      I
      (fm/rows->mat
       (mapv
        (fn [i] (mapv (fn [j] (if (= i j) 1.0 0.0)) (range d)))
        (range d)))]
     (every?
      (fn
       [sigma]
       (let
        [M
         (reel/rep-matrix ir sigma)
         MtM
         (fm/mulm (fm/transpose M) M)
         diff
         (fm/sub MtM I)
         err
         (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
        (< err 1.0E-10)))
      (reel/elements G))))]
  (every? identity results)))


(deftest t56_l393 (is (true? v55_l374)))


(def
 v58_l399
 (let
  [results
   (for
    [n [3 4 5]]
    (let
     [G
      (reel/symmetric-group n)
      ct
      (reel/character-table G)
      parts
      (reel/partitions n)
      classes
      (:classes ct)
      class-idx
      (into {} (map-indexed (fn [i c] [c i]) classes))]
     (every?
      identity
      (for
       [lambda parts]
       (let
        [ir
         (reel/irrep lambda)
         lambda-idx
         (.indexOf (:irrep-labels ct) lambda)
         row
         (nth (:table ct) lambda-idx)]
        (every?
         (fn
          [sigma]
          (let
           [ct-idx
            (class-idx (reel/cycle-type sigma))
            chi-val
            (c/re (nth row ct-idx))
            trace-val
            (reel/rep-character ir sigma)]
           (< (Math/abs (- chi-val trace-val)) 1.0E-8)))
         (reel/elements G)))))))]
  (every? identity results)))


(deftest t59_l419 (is (true? v58_l399)))


(def
 v61_l425
 (let
  [results
   (for
    [n [3 4]]
    (let
     [G
      (reel/symmetric-group n)
      parts
      (reel/partitions n)
      irreps
      (mapv reel/irrep parts)
      elts
      (vec (reel/elements G))
      f
      (into
       {}
       (map-indexed
        (fn [i sigma] [sigma (/ 1.0 (inc (double i)))])
        elts))
      lhs
      (rep/plancherel-lhs G f)
      f-hats
      (rep/matrix-fourier-transform-all G f irreps)
      rhs
      (rep/plancherel-rhs G f-hats irreps)]
     (< (Math/abs (- lhs rhs)) 1.0E-8)))]
  (every? identity results)))


(deftest t62_l441 (is (true? v61_l425)))


(def
 v64_l447
 (let
  [results
   (for
    [n [4 5 6]]
    (let
     [G
      (reel/dihedral-group n)
      act
      (fn
       [[t k] x]
       (case
        t
        :r
        (mod (+ (long x) (long k)) n)
        :s
        (mod (- (long k) (long x)) n)))
      order
      (reel/order G)]
     (every?
      (fn
       [x]
       (let
        [orb (reel/orbit G act x) stab (reel/stabilizer G act x)]
        (= order (* (count orb) (count stab)))))
      (range n))))]
  (every? identity results)))


(deftest t65_l462 (is (true? v64_l447)))


(def
 v67_l466
 (let
  [results
   (for
    [n [3 4 5 6] k [2 3]]
    (let
     [G
      (reel/cyclic-group n)
      domain
      (loop
       [i 0 d [[]]]
       (if
        (= i n)
        d
        (recur (inc i) (for [prev d c (range k)] (conj prev c)))))
      act
      (fn
       [g coloring]
       (mapv
        (fn* [p1__75702#] (coloring (mod (+ p1__75702# (long g)) n)))
        (range n)))
      orbit-count
      (count (reel/orbits G act domain))
      burnside
      (reel/burnside-count G act domain)]
     (= orbit-count burnside)))]
  (every? identity results)))


(deftest t68_l480 (is (true? v67_l466)))


(def
 v70_l484
 (let
  [results
   (for
    [n [3 4 5 6] k [2 3]]
    (let
     [G
      (reel/cyclic-group n)
      act
      (fn [g x] (mod (+ (long x) (long g)) n))
      ci
      (reel/cycle-index G act (range n))
      polya
      (reel/polya-count ci k)
      domain
      (loop
       [i 0 d [[]]]
       (if
        (= i n)
        d
        (recur (inc i) (for [prev d c (range k)] (conj prev c)))))
      act-coloring
      (fn
       [g coloring]
       (mapv
        (fn* [p1__75703#] (coloring (mod (+ p1__75703# (long g)) n)))
        (range n)))
      burnside
      (reel/burnside-count G act-coloring domain)]
     (= polya burnside)))]
  (every? identity results)))


(deftest t71_l501 (is (true? v70_l484)))


(def
 v73_l507
 (let
  [results
   (for
    [n [3 4 5 6 7 8 10 12]]
    (let
     [G
      (reel/dihedral-group n)
      e
      (reel/id G)
      r
      [:r 1]
      s
      [:s 0]
      r-n
      (reduce (fn [acc _] (reel/op G acc r)) e (range n))
      s-2
      (reel/op G s s)
      srs
      (reel/op G s (reel/op G r s))
      r-inv
      (reel/inv G r)]
     (and (= r-n e) (= s-2 e) (= srs r-inv))))]
  (every? identity results)))


(deftest t74_l525 (is (true? v73_l507)))


(def
 v76_l529
 (let
  [results
   (for
    [n [3 4 5 6 7 8 9 10 12 15 16 20]]
    (row-orthogonality-check
     {:label (str "D_" n), :group (reel/dihedral-group n)}))]
  (every? identity results)))


(deftest t77_l535 (is (true? v76_l529)))


(def
 v79_l541
 (let
  [results
   (for
    [n1 [2 3 4 5] n2 [2 3 4 5]]
    (let
     [G1
      (reel/cyclic-group n1)
      G2
      (reel/cyclic-group n2)
      P
      (reel/product-group G1 G2)]
     (= (reel/order P) (* (reel/order G1) (reel/order G2)))))]
  (every? identity results)))


(deftest t80_l550 (is (true? v79_l541)))


(def
 v82_l554
 (let
  [results
   (for
    [n1 [2 3 4 5] n2 [2 3 4]]
    (let
     [G1
      (reel/cyclic-group n1)
      G2
      (reel/dihedral-group n2)
      P
      (reel/product-group G1 G2)]
     (=
      (count (reel/conjugacy-classes P))
      (*
       (count (reel/conjugacy-classes G1))
       (count (reel/conjugacy-classes G2))))))]
  (every? identity results)))


(deftest t83_l565 (is (true? v82_l554)))


(def
 v85_l571
 (let
  [results
   (for
    [n [3 4 5]]
    (let
     [G (reel/symmetric-group n) elts (vec (reel/elements G))]
     (every?
      (fn
       [[s t]]
       (= (reel/sign (reel/op G s t)) (* (reel/sign s) (reel/sign t))))
      (for [a elts b elts] [a b]))))]
  (every? identity results)))


(deftest t86_l581 (is (true? v85_l571)))


(def
 v88_l585
 (let
  [results
   (for
    [n [3 4 5 6]]
    (let
     [G (reel/symmetric-group n) classes (reel/conjugacy-classes G)]
     (every?
      (fn
       [cls]
       (let
        [types (set (map reel/cycle-type (:elements cls)))]
        (= 1 (count types))))
      classes)))]
  (every? identity results)))


(deftest t89_l596 (is (true? v88_l585)))


(def
 v91_l600
 (let
  [results
   (for
    [n [3 4 5 6]]
    (let
     [G
      (reel/symmetric-group n)
      id-perm
      (reel/id G)
      make-swap
      (fn [i] (let [v (vec (range n))] (assoc v i (inc i) (inc i) i)))]
     (every?
      (fn
       [sigma]
       (let
        [swaps
         (reel/adjacent-transposition-decomposition sigma)
         reconstructed
         (reduce (fn [p i] (reel/op G p (make-swap i))) id-perm swaps)]
        (= sigma reconstructed)))
      (reel/elements G))))]
  (every? identity results)))


(deftest t92_l617 (is (true? v91_l600)))


(def
 v94_l623
 (let
  [results
   (for
    [n
     [3 4]
     [l1 l2]
     (let [parts (reel/partitions n)] (for [a parts b parts] [a b]))]
    (let
     [G
      (reel/symmetric-group n)
      ir1
      (reel/irrep l1)
      ir2
      (reel/irrep l2)
      tp
      (reel/tensor-product ir1 ir2)]
     (every?
      (fn
       [sigma]
       (let
        [c1
         (reel/rep-character ir1 sigma)
         c2
         (reel/rep-character ir2 sigma)
         c-tp
         (fm/trace (reel/rep-matrix tp sigma))]
        (< (Math/abs (- c-tp (* c1 c2))) 1.0E-8)))
      (reel/elements G))))]
  (every? identity results)))


(deftest t95_l639 (is (true? v94_l623)))


(def
 v97_l643
 (let
  [results
   (for
    [n
     [3 4]
     [l1 l2]
     (let [parts (reel/partitions n)] (for [a parts b parts] [a b]))]
    (let
     [G
      (reel/symmetric-group n)
      ir1
      (reel/irrep l1)
      ir2
      (reel/irrep l2)
      ds
      (reel/direct-sum ir1 ir2)]
     (every?
      (fn
       [sigma]
       (let
        [c1
         (reel/rep-character ir1 sigma)
         c2
         (reel/rep-character ir2 sigma)
         c-ds
         (fm/trace (reel/rep-matrix ds sigma))]
        (< (Math/abs (- c-ds (+ c1 c2))) 1.0E-8)))
      (reel/elements G))))]
  (every? identity results)))


(deftest t98_l659 (is (true? v97_l643)))


(def
 v100_l663
 (let
  [results
   (for
    [n
     [3 4]
     [l1 l2]
     (let
      [parts (reel/partitions n)]
      (take 4 (for [a parts b parts :when (not= a b)] [a b])))]
    (let
     [G
      (reel/symmetric-group n)
      ir1
      (reel/irrep l1)
      ir2
      (reel/irrep l2)
      tp
      (reel/tensor-product ir1 ir2)
      elts
      (vec (reel/elements G))]
     (every?
      (fn
       [[s t]]
       (let
        [st
         (reel/op G s t)
         rho-st
         (reel/rep-matrix tp st)
         rho-s-t
         (fm/mulm (reel/rep-matrix tp s) (reel/rep-matrix tp t))
         diff
         (fm/sub rho-st rho-s-t)
         err
         (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
        (< err 1.0E-10)))
      (for [a elts b elts] [a b]))))]
  (every? identity results)))


(deftest t101_l683 (is (true? v100_l663)))


(def
 v103_l687
 (let
  [results
   (for
    [n
     [3 4 5]
     [l1 l2]
     (let [parts (reel/partitions n)] (for [a parts b parts] [a b]))]
    (let
     [ir1 (reel/irrep l1) ir2 (reel/irrep l2)]
     (and
      (=
       (reel/rep-dimension (reel/direct-sum ir1 ir2))
       (+ (reel/rep-dimension ir1) (reel/rep-dimension ir2)))
      (=
       (reel/rep-dimension (reel/tensor-product ir1 ir2))
       (* (reel/rep-dimension ir1) (reel/rep-dimension ir2))))))]
  (every? identity results)))


(deftest t104_l699 (is (true? v103_l687)))
