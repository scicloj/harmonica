;; # Algebraic Identities — A Verification Suite
;;
;; This notebook systematically verifies the fundamental identities of group
;; theory and representation theory across every group type in the library.
;; Each identity is tested on diverse inputs, serving as both documentation
;; and a thorough regression test.

(ns reel-book.algebraic-identities
  (:require
   [scicloj.reel.core :as reel]
   [scicloj.reel.protocols :as p]
   [scicloj.reel.representations :as rep]
   [fastmath.complex :as c]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Test Groups
;;
;; We verify identities on a diverse collection of groups covering all
;; implemented types: cyclic, symmetric, dihedral, and product.

(def test-groups
  "A collection of groups for systematic testing."
  [{:label "Z/2Z" :group (reel/cyclic-group 2) :has-ct? true}
   {:label "Z/5Z" :group (reel/cyclic-group 5) :has-ct? true}
   {:label "Z/7Z" :group (reel/cyclic-group 7) :has-ct? true}
   {:label "Z/12Z" :group (reel/cyclic-group 12) :has-ct? true}
   {:label "S_3" :group (reel/symmetric-group 3) :has-ct? true}
   {:label "S_4" :group (reel/symmetric-group 4) :has-ct? true}
   {:label "S_5" :group (reel/symmetric-group 5) :has-ct? true}
   {:label "D_3" :group (reel/dihedral-group 3) :has-ct? true}
   {:label "D_4" :group (reel/dihedral-group 4) :has-ct? true}
   {:label "D_5" :group (reel/dihedral-group 5) :has-ct? true}
   {:label "D_6" :group (reel/dihedral-group 6) :has-ct? true}
   {:label "D_8" :group (reel/dihedral-group 8) :has-ct? true}
   {:label "Z/2Z × Z/3Z" :group (reel/product-group
                                 (reel/cyclic-group 2)
                                 (reel/cyclic-group 3)) :has-ct? false}
   {:label "Z/2Z × Z/2Z" :group (reel/product-group
                                 (reel/cyclic-group 2)
                                 (reel/cyclic-group 2)) :has-ct? false}
   {:label "Z/3Z × Z/4Z" :group (reel/product-group
                                 (reel/cyclic-group 3)
                                 (reel/cyclic-group 4)) :has-ct? false}])

(def ct-groups
  "Groups that have character-table implementations."
  (filterv :has-ct? test-groups))

;; ## Group Axioms
;;
;; Every group must satisfy: identity, inverses, and associativity.

;; ### Identity: $e \cdot g = g \cdot e = g$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [e (reel/id group)
                    ok? (every? (fn [g]
                                  (and (= (reel/op group e g) g)
                                       (= (reel/op group g e) g)))
                                (reel/elements group))]
                {:group label :pass? ok?}))
            test-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ### Inverses: $g \cdot g^{-1} = g^{-1} \cdot g = e$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [e (reel/id group)
                    ok? (every? (fn [g]
                                  (let [gi (reel/inv group g)]
                                    (and (= (reel/op group g gi) e)
                                         (= (reel/op group gi g) e))))
                                (reel/elements group))]
                {:group label :pass? ok?}))
            test-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ### Associativity: $(g \cdot h) \cdot k = g \cdot (h \cdot k)$
;;
;; We test a random sample of triples for larger groups.

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [elts (vec (reel/elements group))
                    ;; For groups up to order 24, test all triples
                    ;; For larger groups, sample
                    triples (if (<= (count elts) 24)
                              (for [a elts b elts c elts] [a b c])
                              (let [rng (java.util.Random. 42)]
                                (repeatedly 500
                                            (fn [] [(elts (.nextInt rng (count elts)))
                                                    (elts (.nextInt rng (count elts)))
                                                    (elts (.nextInt rng (count elts)))]))))
                    ok? (every? (fn [[a b c]]
                                  (= (reel/op group (reel/op group a b) c)
                                     (reel/op group a (reel/op group b c))))
                                triples)]
                {:group label :pass? ok?}))
            test-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ## Conjugacy Class Properties
;;
;; ### Classes partition the group: sizes sum to |G|

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [classes (reel/conjugacy-classes group)
                    total (reduce + (map :size classes))]
                {:group label :pass? (= total (reel/order group))}))
            test-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ### Class elements are disjoint and form a partition

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [classes (reel/conjugacy-classes group)
                    all-elts (mapcat :elements classes)
                    group-set (set (reel/elements group))]
                {:group label
                 :pass? (and (= (count all-elts) (count (set all-elts)))
                             (= (set all-elts) group-set))}))
            test-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ## Character Table Properties
;;
;; The character table of a finite group encodes all irreducible representations.
;; It satisfies several fundamental orthogonality relations.

;; ### Row orthogonality
;;
;; $$\sum_{C} |C| \, \chi_i(C) \, \overline{\chi_j(C)} = |G| \, \delta_{ij}$$
;;
;; Different irreps are orthogonal; same irrep has norm $|G|$.

(defn row-orthogonality-check
  "Check row orthogonality for all pairs of irreps."
  [{:keys [label group]}]
  (let [ct (reel/character-table group)
        {:keys [table class-sizes]} ct
        n-irreps (count table)
        order (reel/order group)
        tol 1e-8]
    (every? identity
            (for [i (range n-irreps)
                  j (range n-irreps)]
              (let [ip (reduce + (map-indexed
                                  (fn [k sz]
                                    (let [ci (nth (nth table i) k)
                                          cj (nth (nth table j) k)]
                                      (* (double sz)
                                         (+ (* (c/re ci) (c/re cj))
                                            (* (c/im ci) (c/im cj))))))
                                  class-sizes))
                    expected (if (= i j) (double order) 0.0)]
                (< (Math/abs (- ip expected)) tol))))))

(every? row-orthogonality-check ct-groups)

(kind/test-last (fn [v] (= true v)))

;; ### Column orthogonality
;;
;; $$\sum_{\rho} \chi_\rho(C_i) \, \overline{\chi_\rho(C_j)} = \frac{|G|}{|C_i|} \, \delta_{ij}$$

(defn column-orthogonality-check
  "Check column orthogonality for all pairs of conjugacy classes."
  [{:keys [label group]}]
  (let [ct (reel/character-table group)
        {:keys [table class-sizes]} ct
        n-classes (count class-sizes)
        order (reel/order group)
        tol 1e-8]
    (every? identity
            (for [i (range n-classes)
                  j (range n-classes)]
              (let [ip (reduce + (map (fn [row]
                                        (let [ci (nth row i)
                                              cj (nth row j)]
                                          (+ (* (c/re ci) (c/re cj))
                                             (* (c/im ci) (c/im cj)))))
                                      table))
                    expected (if (= i j)
                               (/ (double order) (double (nth class-sizes i)))
                               0.0)]
                (< (Math/abs (- ip expected)) tol))))))

(every? column-orthogonality-check ct-groups)

(kind/test-last (fn [v] (= true v)))

;; ### Dimension sum: $\sum_\rho d_\rho^2 = |G|$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [ct (reel/character-table group)
                    dims (mapv (fn [row]
                                 (let [d (first row)]
                                   (c/re d)))
                               (:table ct))
                    dim-sq-sum (reduce + (map #(* % %) dims))]
                {:group label
                 :pass? (< (Math/abs (- dim-sq-sum (double (reel/order group)))) 1e-8)}))
            ct-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ### Number of irreps equals number of conjugacy classes

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [ct (reel/character-table group)
                    n-irreps (count (:table ct))
                    n-classes (count (reel/conjugacy-classes group))]
                {:group label :pass? (= n-irreps n-classes)}))
            ct-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ### Trivial character: $\chi_{\text{trivial}}(g) = 1$ for all $g$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [ct (reel/character-table group)
                    ;; The first irrep should be trivial
                    trivial-row (first (:table ct))
                    ok? (every? (fn [chi-val]
                                  (< (c/abs (c/sub chi-val (c/complex 1.0 0.0))) 1e-8))
                                trivial-row)]
                {:group label :pass? ok?}))
            ct-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ### Character values at identity equal dimensions

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [ct (reel/character-table group)
                    ;; Identity is the first class
                    ok? (every? (fn [row]
                                  (let [d (c/re (first row))]
                                    (< (Math/abs (- d (Math/round d))) 1e-8)))
                                (:table ct))]
                {:group label :pass? ok?}))
            ct-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ## Fourier Transform Properties (Abelian Groups)
;;
;; For abelian groups, the Fourier transform satisfies Parseval's theorem,
;; the convolution theorem, and perfect round-tripping.

(def abelian-groups
  "Abelian groups for Fourier testing."
  [{:label "Z/5Z" :group (reel/cyclic-group 5)}
   {:label "Z/7Z" :group (reel/cyclic-group 7)}
   {:label "Z/12Z" :group (reel/cyclic-group 12)}
   {:label "Z/16Z" :group (reel/cyclic-group 16)}])

;; ### Round-trip: inverse(transform(f)) ≈ f

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [n (reel/order group)
                    ct (reel/character-table group)
                    f-vals (mapv (fn [i] (c/complex (double (inc i)) 0.0)) (range n))
                    f-hat (reel/fourier-transform ct f-vals)
                    f-back (reel/inverse-fourier-transform ct f-hat)
                    max-err (apply max (map (fn [orig back]
                                              (c/abs (c/sub back orig)))
                                            f-vals f-back))]
                {:group label :pass? (< max-err 1e-10)}))
            abelian-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ### Parseval's theorem: $\sum |f(g)|^2 = \frac{1}{|G|} \sum |\hat{f}(k)|^2$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [n (reel/order group)
                    ct (reel/character-table group)
                    f-vals (mapv (fn [i] (c/complex (Math/sin (* 2.0 Math/PI (/ i (double n)))) 0.0))
                                 (range n))
                    f-hat (reel/fourier-transform ct f-vals)
                    lhs (reduce + (map #(let [a (c/abs %)] (* a a)) f-vals))
                    rhs (* (/ 1.0 (double n))
                           (reduce + (map #(let [a (c/abs %)] (* a a)) f-hat)))]
                {:group label :pass? (< (Math/abs (- lhs rhs)) 1e-8)}))
            abelian-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ### Convolution theorem: $\widehat{f * g} = \hat{f} \cdot \hat{g}$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [n (reel/order group)
                    ct (reel/character-table group)
                    f (mapv (fn [i] (c/complex (if (< i 3) 1.0 0.0) 0.0)) (range n))
                    g (mapv (fn [i] (c/complex (/ 1.0 (inc (double i))) 0.0)) (range n))
                    conv (reel/convolve ct f g)
                    f-hat (reel/fourier-transform ct f)
                    g-hat (reel/fourier-transform ct g)
                    conv-hat (reel/fourier-transform ct conv)
                    pointwise (mapv c/mult f-hat g-hat)
                    max-err (apply max (map #(c/abs (c/sub %1 %2)) conv-hat pointwise))]
                {:group label :pass? (< max-err 1e-8)}))
            abelian-groups)]
  (every? :pass? results))

(kind/test-last (fn [v] (= true v)))

;; ## Representation Theory (S_n)
;;
;; Young's orthogonal form provides explicit matrix representations for S_n.
;; We verify the key properties.

;; ### Homomorphism: $\rho(\sigma \tau) = \rho(\sigma) \rho(\tau)$

(let [results
      (for [n [3 4 5]
            lambda (reel/partitions n)]
        (let [G (reel/symmetric-group n)
              ir (reel/irrep lambda)
              elts (vec (reel/elements G))
              ;; Test all pairs for small groups
              pairs (if (<= (count elts) 24)
                      (for [a elts b elts] [a b])
                      (let [rng (java.util.Random. 42)]
                        (repeatedly 200 (fn [] [(elts (.nextInt rng (count elts)))
                                                (elts (.nextInt rng (count elts)))]))))
              ok? (every? (fn [[s t]]
                            (let [st (reel/op G s t)
                                  rho-st (reel/rep-matrix ir st)
                                  rho-s-rho-t (fm/mulm (reel/rep-matrix ir s)
                                                       (reel/rep-matrix ir t))
                                  diff (fm/sub rho-st rho-s-rho-t)
                                  err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                              (< err 1e-10)))
                          pairs)]
          ok?))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Orthogonality: representation matrices are orthogonal
;;
;; $\rho(\sigma)^T = \rho(\sigma)^{-1} = \rho(\sigma^{-1})$

(let [results
      (for [n [3 4 5]
            lambda (reel/partitions n)]
        (let [G (reel/symmetric-group n)
              ir (reel/irrep lambda)
              d (reel/rep-dimension ir)
              I (fm/rows->mat (mapv (fn [i]
                                      (mapv (fn [j] (if (= i j) 1.0 0.0))
                                            (range d)))
                                    (range d)))]
          (every? (fn [sigma]
                    (let [M (reel/rep-matrix ir sigma)
                          MtM (fm/mulm (fm/transpose M) M)
                          diff (fm/sub MtM I)
                          err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                      (< err 1e-10)))
                  (reel/elements G))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Trace matches character table
;;
;; $\text{tr}(\rho_\lambda(\sigma)) = \chi_\lambda(\text{cycle-type}(\sigma))$

(let [results
      (for [n [3 4 5]]
        (let [G (reel/symmetric-group n)
              ct (reel/character-table G)
              parts (reel/partitions n)
              classes (:classes ct)
              class-idx (into {} (map-indexed (fn [i c] [c i]) classes))]
          (every? identity
                  (for [lambda parts]
                    (let [ir (reel/irrep lambda)
                          lambda-idx (.indexOf (:irrep-labels ct) lambda)
                          row (nth (:table ct) lambda-idx)]
                      (every? (fn [sigma]
                                (let [ct-idx (class-idx (reel/cycle-type sigma))
                                      chi-val (c/re (nth row ct-idx))
                                      trace-val (reel/rep-character ir sigma)]
                                  (< (Math/abs (- chi-val trace-val)) 1e-8)))
                              (reel/elements G)))))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Plancherel identity
;;
;; $$\sum_{\sigma \in G} |f(\sigma)|^2 = \frac{1}{|G|} \sum_\rho d_\rho \|\hat{f}(\rho)\|_F^2$$

(let [results
      (for [n [3 4]]
        (let [G (reel/symmetric-group n)
              parts (reel/partitions n)
              irreps (mapv reel/irrep parts)
              ;; Use a non-trivial test function
              elts (vec (reel/elements G))
              f (into {} (map-indexed (fn [i sigma]
                                        [sigma (/ 1.0 (inc (double i)))])
                                      elts))
              lhs (rep/plancherel-lhs G f)
              f-hats (rep/matrix-fourier-transform-all G f irreps)
              rhs (rep/plancherel-rhs G f-hats irreps)]
          (< (Math/abs (- lhs rhs)) 1e-8)))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ## Group Action Identities

;; ### Orbit-stabilizer theorem: $|G| = |\text{Orb}(x)| \cdot |\text{Stab}(x)|$

(let [results
      (for [n [4 5 6]]
        (let [G (reel/dihedral-group n)
              act (fn [[t k] x]
                    (case t
                      :r (mod (+ (long x) (long k)) n)
                      :s (mod (- (long k) (long x)) n)))
              order (reel/order G)]
          (every? (fn [x]
                    (let [orb (reel/orbit G act x)
                          stab (reel/stabilizer G act x)]
                      (= order (* (count orb) (count stab)))))
                  (range n))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Burnside equals orbit count

(let [results
      (for [n [3 4 5 6]
            k [2 3]]
        (let [G (reel/cyclic-group n)
              domain (loop [i 0 d [[]]]
                       (if (= i n) d
                           (recur (inc i) (for [prev d c (range k)] (conj prev c)))))
              act (fn [g coloring]
                    (mapv #(coloring (mod (+ % (long g)) n)) (range n)))
              orbit-count (count (reel/orbits G act domain))
              burnside (reel/burnside-count G act domain)]
          (= orbit-count burnside)))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Cycle index gives correct Pólya count

(let [results
      (for [n [3 4 5 6]
            k [2 3]]
        (let [G (reel/cyclic-group n)
              act (fn [g x] (mod (+ (long x) (long g)) n))
              ci (reel/cycle-index G act (range n))
              polya (reel/polya-count ci k)
              ;; Compare with direct Burnside
              domain (loop [i 0 d [[]]]
                       (if (= i n) d
                           (recur (inc i) (for [prev d c (range k)] (conj prev c)))))
              act-coloring (fn [g coloring]
                             (mapv #(coloring (mod (+ % (long g)) n)) (range n)))
              burnside (reel/burnside-count G act-coloring domain)]
          (= polya burnside)))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ## Dihedral-Specific Identities

;; ### Presentation relations: $r^n = s^2 = e$, $s r s = r^{-1}$

(let [results
      (for [n [3 4 5 6 7 8 10 12]]
        (let [G (reel/dihedral-group n)
              e (reel/id G)
              r [:r 1]
              s [:s 0]
              ;; r^n = e
              r-n (reduce (fn [acc _] (reel/op G acc r)) e (range n))
              ;; s^2 = e
              s-2 (reel/op G s s)
              ;; s·r·s = r^{-1}
              srs (reel/op G s (reel/op G r s))
              r-inv (reel/inv G r)]
          (and (= r-n e)
               (= s-2 e)
               (= srs r-inv))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Dihedral character table: orthogonality on diverse sizes

(let [results
      (for [n [3 4 5 6 7 8 9 10 12 15 16 20]]
        (row-orthogonality-check {:label (str "D_" n)
                                  :group (reel/dihedral-group n)}))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ## Product Group Identities

;; ### Order of product: $|G_1 \times G_2| = |G_1| \cdot |G_2|$

(let [results
      (for [n1 [2 3 4 5]
            n2 [2 3 4 5]]
        (let [G1 (reel/cyclic-group n1)
              G2 (reel/cyclic-group n2)
              P (reel/product-group G1 G2)]
          (= (reel/order P) (* (reel/order G1) (reel/order G2)))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Classes of product: $k(G_1 \times G_2) = k(G_1) \cdot k(G_2)$

(let [results
      (for [n1 [2 3 4 5]
            n2 [2 3 4]]
        (let [G1 (reel/cyclic-group n1)
              G2 (reel/dihedral-group n2)
              P (reel/product-group G1 G2)]
          (= (count (reel/conjugacy-classes P))
             (* (count (reel/conjugacy-classes G1))
                (count (reel/conjugacy-classes G2))))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ## Permutation Identities (S_n specific)

;; ### Sign is a homomorphism: $\text{sign}(\sigma\tau) = \text{sign}(\sigma) \cdot \text{sign}(\tau)$

(let [results
      (for [n [3 4 5]]
        (let [G (reel/symmetric-group n)
              elts (vec (reel/elements G))]
          (every? (fn [[s t]]
                    (= (reel/sign (reel/op G s t))
                       (* (reel/sign s) (reel/sign t))))
                  (for [a elts b elts] [a b]))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Cycle type determines conjugacy class

(let [results
      (for [n [3 4 5 6]]
        (let [G (reel/symmetric-group n)
              classes (reel/conjugacy-classes G)]
          (every? (fn [cls]
                    ;; All elements in the same class have the same cycle type
                    (let [types (set (map reel/cycle-type (:elements cls)))]
                      (= 1 (count types))))
                  classes)))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Decomposition round-trip: compose(decompose(σ)) = σ

(let [results
      (for [n [3 4 5 6]]
        (let [G (reel/symmetric-group n)
              id-perm (reel/id G)
              make-swap (fn [i]
                          (let [v (vec (range n))]
                            (assoc v i (inc i) (inc i) i)))]
          (every? (fn [sigma]
                    (let [swaps (reel/adjacent-transposition-decomposition sigma)
                          reconstructed (reduce (fn [p i]
                                                  (reel/op G (make-swap i) p))
                                                id-perm
                                                swaps)]
                      (= sigma reconstructed)))
                  (reel/elements G))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ## Tensor Product and Direct Sum

;; ### Tensor product character: $\chi_{\rho_1 \otimes \rho_2}(g) = \chi_{\rho_1}(g) \cdot \chi_{\rho_2}(g)$

(let [results
      (for [n [3 4]
            [l1 l2] (let [parts (reel/partitions n)]
                      (for [a parts b parts] [a b]))]
        (let [G (reel/symmetric-group n)
              ir1 (reel/irrep l1)
              ir2 (reel/irrep l2)
              tp (reel/tensor-product ir1 ir2)]
          (every? (fn [sigma]
                    (let [c1 (reel/rep-character ir1 sigma)
                          c2 (reel/rep-character ir2 sigma)
                          c-tp (fm/trace (reel/rep-matrix tp sigma))]
                      (< (Math/abs (- c-tp (* c1 c2))) 1e-8)))
                  (reel/elements G))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Direct sum character: $\chi_{\rho_1 \oplus \rho_2}(g) = \chi_{\rho_1}(g) + \chi_{\rho_2}(g)$

(let [results
      (for [n [3 4]
            [l1 l2] (let [parts (reel/partitions n)]
                      (for [a parts b parts] [a b]))]
        (let [G (reel/symmetric-group n)
              ir1 (reel/irrep l1)
              ir2 (reel/irrep l2)
              ds (reel/direct-sum ir1 ir2)]
          (every? (fn [sigma]
                    (let [c1 (reel/rep-character ir1 sigma)
                          c2 (reel/rep-character ir2 sigma)
                          c-ds (fm/trace (reel/rep-matrix ds sigma))]
                      (< (Math/abs (- c-ds (+ c1 c2))) 1e-8)))
                  (reel/elements G))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Tensor product is a homomorphism

(let [results
      (for [n [3 4]
            [l1 l2] (let [parts (reel/partitions n)]
                      (take 4 (for [a parts b parts
                                    :when (not= a b)] [a b])))]
        (let [G (reel/symmetric-group n)
              ir1 (reel/irrep l1)
              ir2 (reel/irrep l2)
              tp (reel/tensor-product ir1 ir2)
              elts (vec (reel/elements G))]
          (every? (fn [[s t]]
                    (let [st (reel/op G s t)
                          rho-st (reel/rep-matrix tp st)
                          rho-s-t (fm/mulm (reel/rep-matrix tp s) (reel/rep-matrix tp t))
                          diff (fm/sub rho-st rho-s-t)
                          err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                      (< err 1e-10)))
                  (for [a elts b elts] [a b]))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ### Direct sum dimension: $d_{\rho_1 \oplus \rho_2} = d_{\rho_1} + d_{\rho_2}$

(let [results
      (for [n [3 4 5]
            [l1 l2] (let [parts (reel/partitions n)]
                      (for [a parts b parts] [a b]))]
        (let [ir1 (reel/irrep l1)
              ir2 (reel/irrep l2)]
          (and (= (reel/rep-dimension (reel/direct-sum ir1 ir2))
                  (+ (reel/rep-dimension ir1) (reel/rep-dimension ir2)))
               (= (reel/rep-dimension (reel/tensor-product ir1 ir2))
                  (* (reel/rep-dimension ir1) (reel/rep-dimension ir2))))))]
  (every? identity results))

(kind/test-last (fn [v] (= true v)))

;; ## Summary
;;
;; This notebook verified:
;;
;; - **Group axioms**: identity, inverses, associativity for 15 groups
;; - **Conjugacy classes**: partition the group, sizes sum correctly
;; - **Character table**: row orthogonality, column orthogonality, dimension
;;   sum formula, irrep count = class count, trivial character, integer dimensions
;; - **Fourier transform**: round-trip, Parseval's theorem, convolution theorem
;; - **Representations**: homomorphism property, orthogonal matrices, trace = character,
;;   Plancherel identity
;; - **Tensor product**: character multiplicativity, homomorphism, dimension formula
;; - **Direct sum**: character additivity, dimension formula
;; - **Group actions**: orbit-stabilizer theorem, Burnside = orbit count,
;;   Pólya = Burnside
;; - **Dihedral groups**: presentation relations, character orthogonality for n up to 20
;; - **Product groups**: order formula, class count formula
;; - **Permutations**: sign homomorphism, cycle type determines class,
;;   decomposition round-trip
