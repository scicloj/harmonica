(ns
 harmonica-book.algebraic-identities-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.protocols :as p]
  [scicloj.harmonica.analysis.representations :as rep]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [fastmath.matrix :as fm]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l23
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
 v4_l47
 (def
  ct-groups
  "Groups that have character-table implementations."
  (filterv :has-ct? test-groups)))


(def
 v6_l57
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


(deftest t7_l68 (is (true? v6_l57)))


(def
 v9_l72
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


(deftest t10_l84 (is (true? v9_l72)))


(def
 v12_l90
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


(deftest t13_l110 (is (true? v12_l90)))


(def
 v15_l116
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


(deftest t16_l124 (is (true? v15_l116)))


(def
 v18_l128
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


(deftest t19_l139 (is (true? v18_l128)))


(def
 v21_l152
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
            (+ (* (el/re ci) (el/re cj)) (* (el/im ci) (el/im cj))))))
         class-sizes))
       expected
       (if (= i j) (double order) 0.0)]
      (< (Math/abs (- ip expected)) tol)))))))


(def v22_l174 (every? row-orthogonality-check ct-groups))


(deftest t23_l176 (is (true? v22_l174)))


(def
 v25_l182
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
           (+ (* (el/re ci) (el/re cj)) (* (el/im ci) (el/im cj)))))
         table))
       expected
       (if
        (= i j)
        (/ (double order) (double (nth class-sizes i)))
        0.0)]
      (< (Math/abs (- ip expected)) tol)))))))


(def v26_l204 (every? column-orthogonality-check ct-groups))


(deftest t27_l206 (is (true? v26_l204)))


(def
 v29_l210
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [ct
       (hm/character-table group)
       dims
       (mapv (fn [row] (let [d (first row)] (el/re d))) (:table ct))
       dim-sq-sum
       (reduce
        +
        (map (fn* [p1__96361#] (* p1__96361# p1__96361#)) dims))]
      {:group label,
       :pass?
       (<
        (Math/abs (- dim-sq-sum (double (hm/order group))))
        1.0E-8)}))
    ct-groups)]
  (every? :pass? results)))


(deftest t30_l223 (is (true? v29_l210)))


(def
 v32_l227
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


(deftest t33_l236 (is (true? v32_l227)))


(def
 v35_l240
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
         (< (el/abs (el/- chi-val (t/complex 1.0 0.0))) 1.0E-8))
        trivial-row)]
      {:group label, :pass? ok?}))
    ct-groups)]
  (every? :pass? results)))


(deftest t36_l252 (is (true? v35_l240)))


(def
 v38_l256
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
          [d (el/re (first row))]
          (< (Math/abs (- d (Math/round d))) 1.0E-8)))
        (:table ct))]
      {:group label, :pass? ok?}))
    ct-groups)]
  (every? :pass? results)))


(deftest t39_l268 (is (true? v38_l256)))


(def
 v41_l275
 (def
  abelian-groups
  "Abelian groups for Fourier testing."
  [{:label "Z/5Z", :group (hm/cyclic-group 5)}
   {:label "Z/7Z", :group (hm/cyclic-group 7)}
   {:label "Z/12Z", :group (hm/cyclic-group 12)}
   {:label "Z/16Z", :group (hm/cyclic-group 16)}]))


(def
 v43_l284
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
       (t/complex-tensor-real
        (mapv (fn [i] (double (inc i))) (range n)))
       f-hat
       (hm/fourier-transform ct f-vals)
       f-back
       (hm/inverse-fourier-transform ct f-hat)
       max-err
       (apply max (vec (el/abs (el/- f-back f-vals))))]
      {:group label, :pass? (< max-err 1.0E-10)}))
    abelian-groups)]
  (every? :pass? results)))


(deftest t44_l296 (is (true? v43_l284)))


(def
 v46_l300
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
       (t/complex-tensor-real
        (mapv
         (fn [i] (Math/sin (* 2.0 Math/PI (/ i (double n)))))
         (range n)))
       f-hat
       (hm/fourier-transform ct f-vals)
       mag-f
       (el/abs f-vals)
       mag-fh
       (el/abs f-hat)
       lhs
       (apply
        +
        (map (fn* [p1__96362#] (* p1__96362# p1__96362#)) (vec mag-f)))
       rhs
       (*
        (/ 1.0 (double n))
        (apply
         +
         (map
          (fn* [p1__96363#] (* p1__96363# p1__96363#))
          (vec mag-fh))))]
      {:group label, :pass? (< (Math/abs (- lhs rhs)) 1.0E-8)}))
    abelian-groups)]
  (every? :pass? results)))


(deftest t47_l315 (is (true? v46_l300)))


(def
 v49_l319
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
       (t/complex-tensor-real
        (mapv (fn [i] (if (< i 3) 1.0 0.0)) (range n)))
       g
       (t/complex-tensor-real
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
       (el/* f-hat g-hat)
       max-err
       (apply max (vec (el/abs (el/- conv-hat pointwise))))]
      {:group label, :pass? (< max-err 1.0E-8)}))
    abelian-groups)]
  (every? :pass? results)))


(deftest t50_l335 (is (true? v49_l319)))


(def
 v52_l344
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


(deftest t53_l368 (is (true? v52_l344)))


(def
 v55_l374
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


(deftest t56_l393 (is (true? v55_l374)))


(def
 v58_l399
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
            (el/re (nth row ct-idx))
            trace-val
            (hm/rep-character ir sigma)]
           (< (Math/abs (- chi-val trace-val)) 1.0E-8)))
         (hm/elements G)))))))]
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


(deftest t62_l441 (is (true? v61_l425)))


(def
 v64_l447
 (let
  [results
   (for
    [n [4 5 6]]
    (let
     [G
      (hm/dihedral-group n)
      act
      (fn [[t k] x] (case t :r (mod (+ x k) n) :s (mod (- k x) n)))
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


(deftest t65_l462 (is (true? v64_l447)))


(def
 v67_l466
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
        (fn* [p1__96364#] (coloring (mod (+ p1__96364# g) n)))
        (range n)))
      orbit-count
      (count (hm/orbits G act domain))
      burnside
      (hm/burnside-count G act domain)]
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
      (hm/cyclic-group n)
      act
      (fn [g x] (mod (+ x g) n))
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
        (fn* [p1__96365#] (coloring (mod (+ p1__96365# g) n)))
        (range n)))
      burnside
      (hm/burnside-count G act-coloring domain)]
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


(deftest t74_l525 (is (true? v73_l507)))


(def
 v76_l529
 (let
  [results
   (for
    [n [3 4 5 6 7 8 9 10 12 15 16 20]]
    (row-orthogonality-check
     {:label (str "D_" n), :group (hm/dihedral-group n)}))]
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
      (hm/cyclic-group n1)
      G2
      (hm/cyclic-group n2)
      P
      (hm/product-group G1 G2)]
     (= (hm/order P) (* (hm/order G1) (hm/order G2)))))]
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


(deftest t83_l565 (is (true? v82_l554)))


(def
 v85_l571
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


(deftest t86_l581 (is (true? v85_l571)))


(def
 v88_l585
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


(deftest t89_l596 (is (true? v88_l585)))


(def
 v91_l600
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


(deftest t92_l617 (is (true? v91_l600)))


(def
 v94_l623
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


(deftest t95_l639 (is (true? v94_l623)))


(def
 v97_l643
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


(deftest t101_l683 (is (true? v100_l663)))


(def
 v103_l687
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


(deftest t104_l699 (is (true? v103_l687)))


(def
 v106_l705
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [elt-set (set (hm/elements group))]
      {:group label,
       :pass?
       (every?
        (fn
         [g]
         (every?
          (fn [h] (contains? elt-set (hm/op group g h)))
          (hm/elements group)))
        (hm/elements group))}))
    test-groups)]
  (every? :pass? results)))


(deftest t107_l717 (is (true? v106_l705)))


(def
 v109_l721
 (let
  [results
   (mapv
    (fn
     [{:keys [label group]}]
     (let
      [elt-set (set (hm/elements group))]
      {:group label,
       :pass?
       (every?
        (fn [g] (contains? elt-set (hm/inv group g)))
        (hm/elements group))}))
    test-groups)]
  (every? :pass? results)))


(deftest t110_l731 (is (true? v109_l721)))


(def
 v112_l735
 (let
  [results
   (for [n (range 1 25)] (= (hm/order (hm/dihedral-group n)) (* 2 n)))]
  (every? true? results)))


(deftest t113_l740 (is (true? v112_l735)))


(def
 v115_l746
 (let
  [results
   (for
    [n (range 1 13)]
    (every?
     (fn [p] (and (every? pos-int? p) (apply >= p) (= n (reduce + p))))
     (hm/partitions n)))]
  (every? true? results)))


(deftest t116_l755 (is (true? v115_l746)))


(def
 v118_l759
 (let
  [results
   (for
    [n (range 1 11) p (hm/partitions n)]
    (= p (hm/partition-conjugate (hm/partition-conjugate p))))]
  (every? true? results)))


(deftest t119_l765 (is (true? v118_l759)))


(def
 v121_l769
 (let
  [results
   (for
    [n (range 1 11) p (hm/partitions n)]
    (= (reduce + p) (reduce + (hm/partition-conjugate p))))]
  (every? true? results)))


(deftest t122_l775 (is (true? v121_l769)))


(def
 v124_l779
 (let
  [results
   (for
    [n (range 1 8) lambda (hm/partitions n)]
    (let
     [hlf
      (hm/hook-length-dimension lambda)
      syt
      (count (hm/standard-young-tableaux lambda))
      rep
      (hm/rep-dimension (hm/irrep lambda))]
     (= hlf syt rep)))]
  (every? true? results)))


(deftest t125_l788 (is (true? v124_l779)))


(def
 v127_l792
 (defn
  class-size-formula
  "Conjugacy class size from the partition formula."
  [n mu]
  (let
   [fact (fn [m] (reduce *' (range 1 (inc m)))) freq (frequencies mu)]
   (/
    (fact n)
    (reduce
     *'
     (map
      (fn [[k ak]] (*' (reduce *' (repeat ak k)) (fact ak)))
      freq))))))


(def
 v128_l803
 (let
  [results
   (for
    [n (range 2 8)]
    (let
     [G (hm/symmetric-group n) classes (hm/conjugacy-classes G)]
     (every?
      (fn
       [cls]
       (let
        [ct (hm/cycle-type (:representative cls))]
        (= (:size cls) (class-size-formula n ct))))
      classes)))]
  (every? true? results)))


(deftest t129_l813 (is (true? v128_l803)))


(def
 v131_l817
 (let
  [G (hm/symmetric-group 5) e (hm/id G)]
  (every?
   (fn
    [sigma]
    (let
     [ct
      (hm/cycle-type sigma)
      expected
      (reduce
       (fn
        [a b]
        (/ (* a b) (biginteger (.gcd (biginteger a) (biginteger b)))))
       (map biginteger ct))
      actual
      (loop
       [k 1 current sigma]
       (if (= current e) k (recur (inc k) (hm/op G current sigma))))]
     (= actual (long expected))))
   (hm/elements G))))


(deftest t132_l830 (is (true? v131_l817)))


(def
 v134_l836
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
      (fm/rows->mat
       (mapv
        (fn [i] (mapv (fn [j] (if (= i j) 1.0 0.0)) (range d)))
        (range d)))
      rho-e
      (hm/rep-matrix ir (hm/identity-perm n))
      diff
      (fm/sub rho-e I)
      err
      (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
     (< err 1.0E-10)))]
  (every? true? results)))


(deftest t135_l851 (is (true? v134_l836)))


(def
 v137_l855
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


(deftest t138_l869 (is (true? v137_l855)))


(def
 v140_l873
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
      k
      (count table)]
     (every?
      identity
      (for
       [i (range k) j (range k)]
       (let
        [ip
         (hm/character-inner-product
          (nth table i)
          (nth table j)
          class-sizes
          order)
         expected
         (if (= i j) 1.0 0.0)]
        (< (el/abs (el/- ip (t/complex expected 0.0))) 1.0E-8))))))]
  (every? true? results)))


(deftest t141_l888 (is (true? v140_l873)))
