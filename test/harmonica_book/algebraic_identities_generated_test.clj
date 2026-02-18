(ns
 harmonica-book.algebraic-identities-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.protocols :as p]
  [scicloj.harmonica.representations :as rep]
  [scicloj.harmonica.complex :as cx]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l22
 (def
  test-groups
  "A collection of groups for systematic testing."
  [{:label "Z/2Z", :group (hm/cyclic-group 2), :has-ct? true}
   {:label "Z/5Z", :group (hm/cyclic-group 5), :has-ct? true}
   {:label "Z/7Z", :group (hm/cyclic-group 7), :has-ct? true}
   {:label "Z/12Z", :group (hm/cyclic-group 12), :has-ct? true}
   {:label "S_3", :group (hm/symmetric-group 3), :has-ct? true}
   {:label "S_4", :group (hm/symmetric-group 4), :has-ct? true}
   {:label "S_5", :group (hm/symmetric-group 5), :has-ct? true}
   {:label "D_3", :group (hm/dihedral-group 3), :has-ct? true}
   {:label "D_4", :group (hm/dihedral-group 4), :has-ct? true}
   {:label "D_5", :group (hm/dihedral-group 5), :has-ct? true}
   {:label "D_6", :group (hm/dihedral-group 6), :has-ct? true}
   {:label "D_8", :group (hm/dihedral-group 8), :has-ct? true}
   {:label "Z/2Z × Z/3Z",
    :group (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3)),
    :has-ct? false}
   {:label "Z/2Z × Z/2Z",
    :group (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2)),
    :has-ct? false}
   {:label "Z/3Z × Z/4Z",
    :group (hm/product-group (hm/cyclic-group 3) (hm/cyclic-group 4)),
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
       (hm/id group)
       ok?
       (every?
        (fn [g] (and (= (hm/op group e g) g) (= (hm/op group g e) g)))
        (hm/elements group))]
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
       (hm/id group)
       ok?
       (every?
        (fn
         [g]
         (let
          [gi (hm/inv group g)]
          (and (= (hm/op group g gi) e) (= (hm/op group gi g) e))))
        (hm/elements group))]
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
       (vec (hm/elements group))
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
          (hm/op group (hm/op group a b) c)
          (hm/op group a (hm/op group b c))))
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
       (hm/conjugacy-classes group)
       total
       (reduce + (map :size classes))]
      {:group label, :pass? (= total (hm/order group))}))
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
       (hm/conjugacy-classes group)
       all-elts
       (mapcat :elements classes)
       group-set
       (set (hm/elements group))]
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
    (hm/character-table group)
    {:keys [table class-sizes]}
    ct
    n-irreps
    (count table)
    order
    (hm/order group)
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
            (+ (* (cx/re ci) (cx/re cj)) (* (cx/im ci) (cx/im cj))))))
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
    (hm/character-table group)
    {:keys [table class-sizes]}
    ct
    n-classes
    (count class-sizes)
    order
    (hm/order group)
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
           (+ (* (cx/re ci) (cx/re cj)) (* (cx/im ci) (cx/im cj)))))
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
       (hm/character-table group)
       dims
       (mapv (fn [row] (let [d (first row)] (cx/re d))) (:table ct))
       dim-sq-sum
       (reduce
        +
        (map (fn* [p1__93703#] (* p1__93703# p1__93703#)) dims))]
      {:group label,
       :pass?
       (<
        (Math/abs (- dim-sq-sum (double (hm/order group))))
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
       (hm/character-table group)
       n-irreps
       (count (:table ct))
       n-classes
       (count (hm/conjugacy-classes group))]
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
       (hm/character-table group)
       trivial-row
       (first (:table ct))
       ok?
       (every?
        (fn
         [chi-val]
         (< (cx/cabs (cx/csub chi-val (cx/complex 1.0 0.0))) 1.0E-8))
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
       (hm/character-table group)
       ok?
       (every?
        (fn
         [row]
         (let
          [d (cx/re (first row))]
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
  [{:label "Z/5Z", :group (hm/cyclic-group 5)}
   {:label "Z/7Z", :group (hm/cyclic-group 7)}
   {:label "Z/12Z", :group (hm/cyclic-group 12)}
   {:label "Z/16Z", :group (hm/cyclic-group 16)}]))


(def
 v43_l283
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [n
       (hm/order group)
       ct
       (hm/character-table group)
       f-vals
       (cx/complex-tensor-real
        (mapv (fn [i] (double (inc i))) (range n)))
       f-hat
       (hm/fourier-transform ct f-vals)
       f-back
       (hm/inverse-fourier-transform ct f-hat)
       max-err
       (apply max (vec (cx/cabs (cx/csub f-back f-vals))))]
      {:group label, :pass? (< max-err 1.0E-10)}))
    abelian-groups)]
  (every? :pass? results)))


(deftest t44_l295 (is (true? v43_l283)))


(def
 v46_l299
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [n
       (hm/order group)
       ct
       (hm/character-table group)
       f-vals
       (cx/complex-tensor-real
        (mapv
         (fn [i] (Math/sin (* 2.0 Math/PI (/ i (double n)))))
         (range n)))
       f-hat
       (hm/fourier-transform ct f-vals)
       mag-f
       (cx/cabs f-vals)
       mag-fh
       (cx/cabs f-hat)
       lhs
       (apply
        +
        (map (fn* [p1__93704#] (* p1__93704# p1__93704#)) (vec mag-f)))
       rhs
       (*
        (/ 1.0 (double n))
        (apply
         +
         (map
          (fn* [p1__93705#] (* p1__93705# p1__93705#))
          (vec mag-fh))))]
      {:group label, :pass? (< (Math/abs (- lhs rhs)) 1.0E-8)}))
    abelian-groups)]
  (every? :pass? results)))


(deftest t47_l314 (is (true? v46_l299)))


(def
 v49_l318
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [n
       (hm/order group)
       ct
       (hm/character-table group)
       f
       (cx/complex-tensor-real
        (mapv (fn [i] (if (< i 3) 1.0 0.0)) (range n)))
       g
       (cx/complex-tensor-real
        (mapv (fn [i] (/ 1.0 (inc (double i)))) (range n)))
       conv
       (hm/convolve ct f g)
       f-hat
       (hm/fourier-transform ct f)
       g-hat
       (hm/fourier-transform ct g)
       conv-hat
       (hm/fourier-transform ct conv)
       pointwise
       (cx/cmul f-hat g-hat)
       max-err
       (apply max (vec (cx/cabs (cx/csub conv-hat pointwise))))]
      {:group label, :pass? (< max-err 1.0E-8)}))
    abelian-groups)]
  (every? :pass? results)))


(deftest t50_l334 (is (true? v49_l318)))


(def
 v52_l343
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
          (hm/op G s t)
          rho-st
          (hm/rep-matrix ir st)
          rho-s-rho-t
          (fm/mulm (hm/rep-matrix ir s) (hm/rep-matrix ir t))
          diff
          (fm/sub rho-st rho-s-rho-t)
          err
          (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
         (< err 1.0E-10)))
       pairs)]
     ok?))]
  (every? identity results)))


(deftest t53_l367 (is (true? v52_l343)))


(def
 v55_l373
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
      (fm/rows->mat
       (mapv
        (fn [i] (mapv (fn [j] (if (= i j) 1.0 0.0)) (range d)))
        (range d)))]
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
  (every? identity results)))


(deftest t56_l392 (is (true? v55_l373)))


(def
 v58_l398
 (let
  [results
   (for
    [n [3 4 5]]
    (let
     [G
      (hm/symmetric-group n)
      ct
      (hm/character-table G)
      parts
      (hm/partitions n)
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
         (hm/irrep lambda)
         lambda-idx
         (.indexOf (:irrep-labels ct) lambda)
         row
         (nth (:table ct) lambda-idx)]
        (every?
         (fn
          [sigma]
          (let
           [ct-idx
            (class-idx (hm/cycle-type sigma))
            chi-val
            (cx/re (nth row ct-idx))
            trace-val
            (hm/rep-character ir sigma)]
           (< (Math/abs (- chi-val trace-val)) 1.0E-8)))
         (hm/elements G)))))))]
  (every? identity results)))


(deftest t59_l418 (is (true? v58_l398)))


(def
 v61_l424
 (let
  [results
   (for
    [n [3 4]]
    (let
     [G
      (hm/symmetric-group n)
      parts
      (hm/partitions n)
      irreps
      (mapv hm/irrep parts)
      elts
      (vec (hm/elements G))
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


(deftest t62_l440 (is (true? v61_l424)))


(def
 v64_l446
 (let
  [results
   (for
    [n [4 5 6]]
    (let
     [G
      (hm/dihedral-group n)
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
      (hm/order G)]
     (every?
      (fn
       [x]
       (let
        [orb (hm/orbit G act x) stab (hm/stabilizer G act x)]
        (= order (* (count orb) (count stab)))))
      (range n))))]
  (every? identity results)))


(deftest t65_l461 (is (true? v64_l446)))


(def
 v67_l465
 (let
  [results
   (for
    [n [3 4 5 6] k [2 3]]
    (let
     [G
      (hm/cyclic-group n)
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
        (fn* [p1__93706#] (coloring (mod (+ p1__93706# (long g)) n)))
        (range n)))
      orbit-count
      (count (hm/orbits G act domain))
      burnside
      (hm/burnside-count G act domain)]
     (= orbit-count burnside)))]
  (every? identity results)))


(deftest t68_l479 (is (true? v67_l465)))


(def
 v70_l483
 (let
  [results
   (for
    [n [3 4 5 6] k [2 3]]
    (let
     [G
      (hm/cyclic-group n)
      act
      (fn [g x] (mod (+ (long x) (long g)) n))
      ci
      (hm/cycle-index G act (range n))
      polya
      (hm/polya-count ci k)
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
        (fn* [p1__93707#] (coloring (mod (+ p1__93707# (long g)) n)))
        (range n)))
      burnside
      (hm/burnside-count G act-coloring domain)]
     (= polya burnside)))]
  (every? identity results)))


(deftest t71_l500 (is (true? v70_l483)))


(def
 v73_l506
 (let
  [results
   (for
    [n [3 4 5 6 7 8 10 12]]
    (let
     [G
      (hm/dihedral-group n)
      e
      (hm/id G)
      r
      [:r 1]
      s
      [:s 0]
      r-n
      (reduce (fn [acc _] (hm/op G acc r)) e (range n))
      s-2
      (hm/op G s s)
      srs
      (hm/op G s (hm/op G r s))
      r-inv
      (hm/inv G r)]
     (and (= r-n e) (= s-2 e) (= srs r-inv))))]
  (every? identity results)))


(deftest t74_l524 (is (true? v73_l506)))


(def
 v76_l528
 (let
  [results
   (for
    [n [3 4 5 6 7 8 9 10 12 15 16 20]]
    (row-orthogonality-check
     {:label (str "D_" n), :group (hm/dihedral-group n)}))]
  (every? identity results)))


(deftest t77_l534 (is (true? v76_l528)))


(def
 v79_l540
 (let
  [results
   (for
    [n1 [2 3 4 5] n2 [2 3 4 5]]
    (let
     [G1
      (hm/cyclic-group n1)
      G2
      (hm/cyclic-group n2)
      P
      (hm/product-group G1 G2)]
     (= (hm/order P) (* (hm/order G1) (hm/order G2)))))]
  (every? identity results)))


(deftest t80_l549 (is (true? v79_l540)))


(def
 v82_l553
 (let
  [results
   (for
    [n1 [2 3 4 5] n2 [2 3 4]]
    (let
     [G1
      (hm/cyclic-group n1)
      G2
      (hm/dihedral-group n2)
      P
      (hm/product-group G1 G2)]
     (=
      (count (hm/conjugacy-classes P))
      (*
       (count (hm/conjugacy-classes G1))
       (count (hm/conjugacy-classes G2))))))]
  (every? identity results)))


(deftest t83_l564 (is (true? v82_l553)))


(def
 v85_l570
 (let
  [results
   (for
    [n [3 4 5]]
    (let
     [G (hm/symmetric-group n) elts (vec (hm/elements G))]
     (every?
      (fn
       [[s t]]
       (= (hm/sign (hm/op G s t)) (* (hm/sign s) (hm/sign t))))
      (for [a elts b elts] [a b]))))]
  (every? identity results)))


(deftest t86_l580 (is (true? v85_l570)))


(def
 v88_l584
 (let
  [results
   (for
    [n [3 4 5 6]]
    (let
     [G (hm/symmetric-group n) classes (hm/conjugacy-classes G)]
     (every?
      (fn
       [cls]
       (let
        [types (set (map hm/cycle-type (:elements cls)))]
        (= 1 (count types))))
      classes)))]
  (every? identity results)))


(deftest t89_l595 (is (true? v88_l584)))


(def
 v91_l599
 (let
  [results
   (for
    [n [3 4 5 6]]
    (let
     [G
      (hm/symmetric-group n)
      id-perm
      (hm/id G)
      make-swap
      (fn [i] (let [v (vec (range n))] (assoc v i (inc i) (inc i) i)))]
     (every?
      (fn
       [sigma]
       (let
        [swaps
         (hm/adjacent-transposition-decomposition sigma)
         reconstructed
         (reduce (fn [p i] (hm/op G p (make-swap i))) id-perm swaps)]
        (= sigma reconstructed)))
      (hm/elements G))))]
  (every? identity results)))


(deftest t92_l616 (is (true? v91_l599)))


(def
 v94_l622
 (let
  [results
   (for
    [n
     [3 4]
     [l1 l2]
     (let [parts (hm/partitions n)] (for [a parts b parts] [a b]))]
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
  (every? identity results)))


(deftest t95_l638 (is (true? v94_l622)))


(def
 v97_l642
 (let
  [results
   (for
    [n
     [3 4]
     [l1 l2]
     (let [parts (hm/partitions n)] (for [a parts b parts] [a b]))]
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
  (every? identity results)))


(deftest t98_l658 (is (true? v97_l642)))


(def
 v100_l662
 (let
  [results
   (for
    [n
     [3 4]
     [l1 l2]
     (let
      [parts (hm/partitions n)]
      (take 4 (for [a parts b parts :when (not= a b)] [a b])))]
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
  (every? identity results)))


(deftest t101_l682 (is (true? v100_l662)))


(def
 v103_l686
 (let
  [results
   (for
    [n
     [3 4 5]
     [l1 l2]
     (let [parts (hm/partitions n)] (for [a parts b parts] [a b]))]
    (let
     [ir1 (hm/irrep l1) ir2 (hm/irrep l2)]
     (and
      (=
       (hm/rep-dimension (hm/direct-sum ir1 ir2))
       (+ (hm/rep-dimension ir1) (hm/rep-dimension ir2)))
      (=
       (hm/rep-dimension (hm/tensor-product ir1 ir2))
       (* (hm/rep-dimension ir1) (hm/rep-dimension ir2))))))]
  (every? identity results)))


(deftest t104_l698 (is (true? v103_l686)))
