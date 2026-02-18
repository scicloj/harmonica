;; # Algebraic Identities — A Verification Suite
;;
;; This notebook systematically verifies the fundamental identities of group
;; theory and representation theory across every group type in the library.
;; Each identity is tested on diverse inputs, serving as both documentation
;; and a thorough regression test.

(ns harmonica-book.algebraic-identities
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.protocols :as p]
   [scicloj.harmonica.representations :as rep]
   [scicloj.harmonica.complex :as cx]
   [fastmath.matrix :as fm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Test Groups
;;
;; We verify identities on a diverse collection of groups covering all
;; implemented types: cyclic, symmetric, dihedral, and product.

(def test-groups
  "A collection of groups for systematic testing."
  [{:label "Z/2Z" :group (hm/cyclic-group 2) :has-ct? true}
   {:label "Z/5Z" :group (hm/cyclic-group 5) :has-ct? true}
   {:label "Z/7Z" :group (hm/cyclic-group 7) :has-ct? true}
   {:label "Z/12Z" :group (hm/cyclic-group 12) :has-ct? true}
   {:label "S_3" :group (hm/symmetric-group 3) :has-ct? true}
   {:label "S_4" :group (hm/symmetric-group 4) :has-ct? true}
   {:label "S_5" :group (hm/symmetric-group 5) :has-ct? true}
   {:label "D_3" :group (hm/dihedral-group 3) :has-ct? true}
   {:label "D_4" :group (hm/dihedral-group 4) :has-ct? true}
   {:label "D_5" :group (hm/dihedral-group 5) :has-ct? true}
   {:label "D_6" :group (hm/dihedral-group 6) :has-ct? true}
   {:label "D_8" :group (hm/dihedral-group 8) :has-ct? true}
   {:label "Z/2Z × Z/3Z" :group (hm/product-group
                                 (hm/cyclic-group 2)
                                 (hm/cyclic-group 3)) :has-ct? false}
   {:label "Z/2Z × Z/2Z" :group (hm/product-group
                                 (hm/cyclic-group 2)
                                 (hm/cyclic-group 2)) :has-ct? false}
   {:label "Z/3Z × Z/4Z" :group (hm/product-group
                                 (hm/cyclic-group 3)
                                 (hm/cyclic-group 4)) :has-ct? false}])

(def ct-groups
  "Groups that have character-table implementations."
  (filterv :has-ct? test-groups))

;; ## Group Axioms
;;
;; Every group must satisfy: identity, inverses, and associativity.

;; ### Identity: $e \cdot g = g \cdot e = g$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [e (hm/id group)
                    ok? (every? (fn [g]
                                  (and (= (hm/op group e g) g)
                                       (= (hm/op group g e) g)))
                                (hm/elements group))]
                {:group label :pass? ok?}))
            test-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Inverses: $g \cdot g^{-1} = g^{-1} \cdot g = e$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [e (hm/id group)
                    ok? (every? (fn [g]
                                  (let [gi (hm/inv group g)]
                                    (and (= (hm/op group g gi) e)
                                         (= (hm/op group gi g) e))))
                                (hm/elements group))]
                {:group label :pass? ok?}))
            test-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Associativity: $(g \cdot h) \cdot k = g \cdot (h \cdot k)$
;;
;; We test a random sample of triples for larger groups.

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [elts (vec (hm/elements group))
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
                                  (= (hm/op group (hm/op group a b) c)
                                     (hm/op group a (hm/op group b c))))
                                triples)]
                {:group label :pass? ok?}))
            test-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Conjugacy Class Properties
;;
;; ### Classes partition the group: sizes sum to |G|

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [classes (hm/conjugacy-classes group)
                    total (reduce + (map :size classes))]
                {:group label :pass? (= total (hm/order group))}))
            test-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Class elements are disjoint and form a partition

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [classes (hm/conjugacy-classes group)
                    all-elts (mapcat :elements classes)
                    group-set (set (hm/elements group))]
                {:group label
                 :pass? (and (= (count all-elts) (count (set all-elts)))
                             (= (set all-elts) group-set))}))
            test-groups)]
  (every? :pass? results))

(kind/test-last [true?])

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
  (let [ct (hm/character-table group)
        {:keys [table class-sizes]} ct
        n-irreps (count table)
        order (hm/order group)
        tol 1e-8]
    (every? identity
            (for [i (range n-irreps)
                  j (range n-irreps)]
              (let [ip (reduce + (map-indexed
                                  (fn [k sz]
                                    (let [ci (nth (nth table i) k)
                                          cj (nth (nth table j) k)]
                                      (* (double sz)
                                         (+ (* (cx/re ci) (cx/re cj))
                                            (* (cx/im ci) (cx/im cj))))))
                                  class-sizes))
                    expected (if (= i j) (double order) 0.0)]
                (< (Math/abs (- ip expected)) tol))))))

(every? row-orthogonality-check ct-groups)

(kind/test-last [true?])

;; ### Column orthogonality
;;
;; $$\sum_{\rho} \chi_\rho(C_i) \, \overline{\chi_\rho(C_j)} = \frac{|G|}{|C_i|} \, \delta_{ij}$$

(defn column-orthogonality-check
  "Check column orthogonality for all pairs of conjugacy classes."
  [{:keys [label group]}]
  (let [ct (hm/character-table group)
        {:keys [table class-sizes]} ct
        n-classes (count class-sizes)
        order (hm/order group)
        tol 1e-8]
    (every? identity
            (for [i (range n-classes)
                  j (range n-classes)]
              (let [ip (reduce + (map (fn [row]
                                        (let [ci (nth row i)
                                              cj (nth row j)]
                                          (+ (* (cx/re ci) (cx/re cj))
                                             (* (cx/im ci) (cx/im cj)))))
                                      table))
                    expected (if (= i j)
                               (/ (double order) (double (nth class-sizes i)))
                               0.0)]
                (< (Math/abs (- ip expected)) tol))))))

(every? column-orthogonality-check ct-groups)

(kind/test-last [true?])

;; ### Dimension sum: $\sum_\rho d_\rho^2 = |G|$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [ct (hm/character-table group)
                    dims (mapv (fn [row]
                                 (let [d (first row)]
                                   (cx/re d)))
                               (:table ct))
                    dim-sq-sum (reduce + (map #(* % %) dims))]
                {:group label
                 :pass? (< (Math/abs (- dim-sq-sum (double (hm/order group)))) 1e-8)}))
            ct-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Number of irreps equals number of conjugacy classes

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [ct (hm/character-table group)
                    n-irreps (count (:table ct))
                    n-classes (count (hm/conjugacy-classes group))]
                {:group label :pass? (= n-irreps n-classes)}))
            ct-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Trivial character: $\chi_{\text{trivial}}(g) = 1$ for all $g$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [ct (hm/character-table group)
                    ;; The first irrep should be trivial
                    trivial-row (first (:table ct))
                    ok? (every? (fn [chi-val]
                                  (< (cx/cabs (cx/csub chi-val (cx/complex 1.0 0.0))) 1e-8))
                                trivial-row)]
                {:group label :pass? ok?}))
            ct-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Character values at identity equal dimensions

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [ct (hm/character-table group)
                    ;; Identity is the first class
                    ok? (every? (fn [row]
                                  (let [d (cx/re (first row))]
                                    (< (Math/abs (- d (Math/round d))) 1e-8)))
                                (:table ct))]
                {:group label :pass? ok?}))
            ct-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Fourier Transform Properties (Abelian Groups)
;;
;; For abelian groups, the Fourier transform satisfies Parseval's theorem,
;; the convolution theorem, and perfect round-tripping.

(def abelian-groups
  "Abelian groups for Fourier testing."
  [{:label "Z/5Z" :group (hm/cyclic-group 5)}
   {:label "Z/7Z" :group (hm/cyclic-group 7)}
   {:label "Z/12Z" :group (hm/cyclic-group 12)}
   {:label "Z/16Z" :group (hm/cyclic-group 16)}])

;; ### Round-trip: inverse(transform(f)) ≈ f

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [n (hm/order group)
                    ct (hm/character-table group)
                    f-vals (cx/complex-tensor-real (mapv (fn [i] (double (inc i))) (range n)))
                    f-hat (hm/fourier-transform ct f-vals)
                    f-back (hm/inverse-fourier-transform ct f-hat)
                    max-err (apply max (vec (cx/cabs (cx/csub f-back f-vals))))]
                {:group label :pass? (< max-err 1e-10)}))
            abelian-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Parseval's theorem: $\sum |f(g)|^2 = \frac{1}{|G|} \sum |\hat{f}(k)|^2$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [n (hm/order group)
                    ct (hm/character-table group)
                    f-vals (cx/complex-tensor-real (mapv (fn [i] (Math/sin (* 2.0 Math/PI (/ i (double n))))) (range n)))
                    f-hat (hm/fourier-transform ct f-vals)
                    mag-f (cx/cabs f-vals)
                    mag-fh (cx/cabs f-hat)
                    lhs (apply + (map #(* % %) (vec mag-f)))
                    rhs (* (/ 1.0 (double n))
                           (apply + (map #(* % %) (vec mag-fh))))]
                {:group label :pass? (< (Math/abs (- lhs rhs)) 1e-8)}))
            abelian-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Convolution theorem: $\widehat{f * g} = \hat{f} \cdot \hat{g}$

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [n (hm/order group)
                    ct (hm/character-table group)
                    f (cx/complex-tensor-real (mapv (fn [i] (if (< i 3) 1.0 0.0)) (range n)))
                    g (cx/complex-tensor-real (mapv (fn [i] (/ 1.0 (inc (double i)))) (range n)))
                    conv (hm/convolve ct f g)
                    f-hat (hm/fourier-transform ct f)
                    g-hat (hm/fourier-transform ct g)
                    conv-hat (hm/fourier-transform ct conv)
                    pointwise (cx/cmul f-hat g-hat)
                    max-err (apply max (vec (cx/cabs (cx/csub conv-hat pointwise))))]
                {:group label :pass? (< max-err 1e-8)}))
            abelian-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Representation Theory (S_n)
;;
;; Young's orthogonal form provides explicit matrix representations for S_n.
;; We verify the key properties.

;; ### Homomorphism: $\rho(\sigma \tau) = \rho(\sigma) \rho(\tau)$

(let [results
      (for [n [3 4 5]
            lambda (hm/partitions n)]
        (let [G (hm/symmetric-group n)
              ir (hm/irrep lambda)
              elts (vec (hm/elements G))
              ;; Test all pairs for small groups
              pairs (if (<= (count elts) 24)
                      (for [a elts b elts] [a b])
                      (let [rng (java.util.Random. 42)]
                        (repeatedly 200 (fn [] [(elts (.nextInt rng (count elts)))
                                                (elts (.nextInt rng (count elts)))]))))
              ok? (every? (fn [[s t]]
                            (let [st (hm/op G s t)
                                  rho-st (hm/rep-matrix ir st)
                                  rho-s-rho-t (fm/mulm (hm/rep-matrix ir s)
                                                       (hm/rep-matrix ir t))
                                  diff (fm/sub rho-st rho-s-rho-t)
                                  err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                              (< err 1e-10)))
                          pairs)]
          ok?))]
  (every? identity results))

(kind/test-last [true?])

;; ### Orthogonality: representation matrices are orthogonal
;;
;; $\rho(\sigma)^T = \rho(\sigma)^{-1} = \rho(\sigma^{-1})$

(let [results
      (for [n [3 4 5]
            lambda (hm/partitions n)]
        (let [G (hm/symmetric-group n)
              ir (hm/irrep lambda)
              d (hm/rep-dimension ir)
              I (fm/rows->mat (mapv (fn [i]
                                      (mapv (fn [j] (if (= i j) 1.0 0.0))
                                            (range d)))
                                    (range d)))]
          (every? (fn [sigma]
                    (let [M (hm/rep-matrix ir sigma)
                          MtM (fm/mulm (fm/transpose M) M)
                          diff (fm/sub MtM I)
                          err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                      (< err 1e-10)))
                  (hm/elements G))))]
  (every? identity results))

(kind/test-last [true?])

;; ### Trace matches character table
;;
;; $\text{tr}(\rho_\lambda(\sigma)) = \chi_\lambda(\text{cycle-type}(\sigma))$

(let [results
      (for [n [3 4 5]]
        (let [G (hm/symmetric-group n)
              ct (hm/character-table G)
              parts (hm/partitions n)
              classes (:classes ct)
              class-idx (into {} (map-indexed (fn [i c] [c i]) classes))]
          (every? identity
                  (for [lambda parts]
                    (let [ir (hm/irrep lambda)
                          lambda-idx (.indexOf (:irrep-labels ct) lambda)
                          row (nth (:table ct) lambda-idx)]
                      (every? (fn [sigma]
                                (let [ct-idx (class-idx (hm/cycle-type sigma))
                                      chi-val (cx/re (nth row ct-idx))
                                      trace-val (hm/rep-character ir sigma)]
                                  (< (Math/abs (- chi-val trace-val)) 1e-8)))
                              (hm/elements G)))))))]
  (every? identity results))

(kind/test-last [true?])

;; ### Plancherel identity
;;
;; $$\sum_{\sigma \in G} |f(\sigma)|^2 = \frac{1}{|G|} \sum_\rho d_\rho \|\hat{f}(\rho)\|_F^2$$

(let [results
      (for [n [3 4]]
        (let [G (hm/symmetric-group n)
              parts (hm/partitions n)
              irreps (mapv hm/irrep parts)
              ;; Use a non-trivial test function
              elts (vec (hm/elements G))
              f (into {} (map-indexed (fn [i sigma]
                                        [sigma (/ 1.0 (inc (double i)))])
                                      elts))
              lhs (rep/plancherel-lhs G f)
              f-hats (rep/matrix-fourier-transform-all G f irreps)
              rhs (rep/plancherel-rhs G f-hats irreps)]
          (< (Math/abs (- lhs rhs)) 1e-8)))]
  (every? identity results))

(kind/test-last [true?])

;; ## Group Action Identities

;; ### Orbit-stabilizer theorem: $|G| = |\text{Orb}(x)| \cdot |\text{Stab}(x)|$

(let [results
      (for [n [4 5 6]]
        (let [G (hm/dihedral-group n)
              act (fn [[t k] x]
                    (case t
                      :r (mod (+ (long x) (long k)) n)
                      :s (mod (- (long k) (long x)) n)))
              order (hm/order G)]
          (every? (fn [x]
                    (let [orb (hm/orbit G act x)
                          stab (hm/stabilizer G act x)]
                      (= order (* (count orb) (count stab)))))
                  (range n))))]
  (every? identity results))

(kind/test-last [true?])

;; ### Burnside equals orbit count

(let [results
      (for [n [3 4 5 6]
            k [2 3]]
        (let [G (hm/cyclic-group n)
              domain (loop [i 0 d [[]]]
                       (if (= i n) d
                           (recur (inc i) (for [prev d c (range k)] (conj prev c)))))
              act (fn [g coloring]
                    (mapv #(coloring (mod (+ % (long g)) n)) (range n)))
              orbit-count (count (hm/orbits G act domain))
              burnside (hm/burnside-count G act domain)]
          (= orbit-count burnside)))]
  (every? identity results))

(kind/test-last [true?])

;; ### Cycle index gives correct Pólya count

(let [results
      (for [n [3 4 5 6]
            k [2 3]]
        (let [G (hm/cyclic-group n)
              act (fn [g x] (mod (+ (long x) (long g)) n))
              ci (hm/cycle-index G act (range n))
              polya (hm/polya-count ci k)
              ;; Compare with direct Burnside
              domain (loop [i 0 d [[]]]
                       (if (= i n) d
                           (recur (inc i) (for [prev d c (range k)] (conj prev c)))))
              act-coloring (fn [g coloring]
                             (mapv #(coloring (mod (+ % (long g)) n)) (range n)))
              burnside (hm/burnside-count G act-coloring domain)]
          (= polya burnside)))]
  (every? identity results))

(kind/test-last [true?])

;; ## Dihedral-Specific Identities

;; ### Presentation relations: $r^n = s^2 = e$, $s r s = r^{-1}$

(let [results
      (for [n [3 4 5 6 7 8 10 12]]
        (let [G (hm/dihedral-group n)
              e (hm/id G)
              r [:r 1]
              s [:s 0]
              ;; r^n = e
              r-n (reduce (fn [acc _] (hm/op G acc r)) e (range n))
              ;; s^2 = e
              s-2 (hm/op G s s)
              ;; s·r·s = r^{-1}
              srs (hm/op G s (hm/op G r s))
              r-inv (hm/inv G r)]
          (and (= r-n e)
               (= s-2 e)
               (= srs r-inv))))]
  (every? identity results))

(kind/test-last [true?])

;; ### Dihedral character table: orthogonality on diverse sizes

(let [results
      (for [n [3 4 5 6 7 8 9 10 12 15 16 20]]
        (row-orthogonality-check {:label (str "D_" n)
                                  :group (hm/dihedral-group n)}))]
  (every? identity results))

(kind/test-last [true?])

;; ## Product Group Identities

;; ### Order of product: $|G_1 \times G_2| = |G_1| \cdot |G_2|$

(let [results
      (for [n1 [2 3 4 5]
            n2 [2 3 4 5]]
        (let [G1 (hm/cyclic-group n1)
              G2 (hm/cyclic-group n2)
              P (hm/product-group G1 G2)]
          (= (hm/order P) (* (hm/order G1) (hm/order G2)))))]
  (every? identity results))

(kind/test-last [true?])

;; ### Classes of product: $k(G_1 \times G_2) = k(G_1) \cdot k(G_2)$

(let [results
      (for [n1 [2 3 4 5]
            n2 [2 3 4]]
        (let [G1 (hm/cyclic-group n1)
              G2 (hm/dihedral-group n2)
              P (hm/product-group G1 G2)]
          (= (count (hm/conjugacy-classes P))
             (* (count (hm/conjugacy-classes G1))
                (count (hm/conjugacy-classes G2))))))]
  (every? identity results))

(kind/test-last [true?])

;; ## Permutation Identities (S_n specific)

;; ### Sign is a homomorphism: $\text{sign}(\sigma\tau) = \text{sign}(\sigma) \cdot \text{sign}(\tau)$

(let [results
      (for [n [3 4 5]]
        (let [G (hm/symmetric-group n)
              elts (vec (hm/elements G))]
          (every? (fn [[s t]]
                    (= (hm/sign (hm/op G s t))
                       (* (hm/sign s) (hm/sign t))))
                  (for [a elts b elts] [a b]))))]
  (every? identity results))

(kind/test-last [true?])

;; ### Cycle type determines conjugacy class

(let [results
      (for [n [3 4 5 6]]
        (let [G (hm/symmetric-group n)
              classes (hm/conjugacy-classes G)]
          (every? (fn [cls]
                    ;; All elements in the same class have the same cycle type
                    (let [types (set (map hm/cycle-type (:elements cls)))]
                      (= 1 (count types))))
                  classes)))]
  (every? identity results))

(kind/test-last [true?])

;; ### Decomposition round-trip: compose(decompose(σ)) = σ

(let [results
      (for [n [3 4 5 6]]
        (let [G (hm/symmetric-group n)
              id-perm (hm/id G)
              make-swap (fn [i]
                          (let [v (vec (range n))]
                            (assoc v i (inc i) (inc i) i)))]
          (every? (fn [sigma]
                    (let [swaps (hm/adjacent-transposition-decomposition sigma)
                          reconstructed (reduce (fn [p i]
                                                  (hm/op G p (make-swap i)))
                                                id-perm
                                                swaps)]
                      (= sigma reconstructed)))
                  (hm/elements G))))]
  (every? identity results))

(kind/test-last [true?])

;; ## Tensor Product and Direct Sum

;; ### Tensor product character: $\chi_{\rho_1 \otimes \rho_2}(g) = \chi_{\rho_1}(g) \cdot \chi_{\rho_2}(g)$

(let [results
      (for [n [3 4]
            [l1 l2] (let [parts (hm/partitions n)]
                      (for [a parts b parts] [a b]))]
        (let [G (hm/symmetric-group n)
              ir1 (hm/irrep l1)
              ir2 (hm/irrep l2)
              tp (hm/tensor-product ir1 ir2)]
          (every? (fn [sigma]
                    (let [c1 (hm/rep-character ir1 sigma)
                          c2 (hm/rep-character ir2 sigma)
                          c-tp (fm/trace (hm/rep-matrix tp sigma))]
                      (< (Math/abs (- c-tp (* c1 c2))) 1e-8)))
                  (hm/elements G))))]
  (every? identity results))

(kind/test-last [true?])

;; ### Direct sum character: $\chi_{\rho_1 \oplus \rho_2}(g) = \chi_{\rho_1}(g) + \chi_{\rho_2}(g)$

(let [results
      (for [n [3 4]
            [l1 l2] (let [parts (hm/partitions n)]
                      (for [a parts b parts] [a b]))]
        (let [G (hm/symmetric-group n)
              ir1 (hm/irrep l1)
              ir2 (hm/irrep l2)
              ds (hm/direct-sum ir1 ir2)]
          (every? (fn [sigma]
                    (let [c1 (hm/rep-character ir1 sigma)
                          c2 (hm/rep-character ir2 sigma)
                          c-ds (fm/trace (hm/rep-matrix ds sigma))]
                      (< (Math/abs (- c-ds (+ c1 c2))) 1e-8)))
                  (hm/elements G))))]
  (every? identity results))

(kind/test-last [true?])

;; ### Tensor product is a homomorphism

(let [results
      (for [n [3 4]
            [l1 l2] (let [parts (hm/partitions n)]
                      (take 4 (for [a parts b parts
                                    :when (not= a b)] [a b])))]
        (let [G (hm/symmetric-group n)
              ir1 (hm/irrep l1)
              ir2 (hm/irrep l2)
              tp (hm/tensor-product ir1 ir2)
              elts (vec (hm/elements G))]
          (every? (fn [[s t]]
                    (let [st (hm/op G s t)
                          rho-st (hm/rep-matrix tp st)
                          rho-s-t (fm/mulm (hm/rep-matrix tp s) (hm/rep-matrix tp t))
                          diff (fm/sub rho-st rho-s-t)
                          err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                      (< err 1e-10)))
                  (for [a elts b elts] [a b]))))]
  (every? identity results))

(kind/test-last [true?])

;; ### Direct sum dimension: $d_{\rho_1 \oplus \rho_2} = d_{\rho_1} + d_{\rho_2}$

(let [results
      (for [n [3 4 5]
            [l1 l2] (let [parts (hm/partitions n)]
                      (for [a parts b parts] [a b]))]
        (let [ir1 (hm/irrep l1)
              ir2 (hm/irrep l2)]
          (and (= (hm/rep-dimension (hm/direct-sum ir1 ir2))
                  (+ (hm/rep-dimension ir1) (hm/rep-dimension ir2)))
               (= (hm/rep-dimension (hm/tensor-product ir1 ir2))
                  (* (hm/rep-dimension ir1) (hm/rep-dimension ir2))))))]
  (every? identity results))

(kind/test-last [true?])

;; ## Additional Group Axioms

;; ### Closure: products stay in the group

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [elt-set (set (hm/elements group))]
                {:group label
                 :pass? (every? (fn [g]
                                  (every? (fn [h]
                                            (contains? elt-set (hm/op group g h)))
                                          (hm/elements group)))
                                (hm/elements group))}))
            test-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Inverse is in the group

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [elt-set (set (hm/elements group))]
                {:group label
                 :pass? (every? (fn [g]
                                  (contains? elt-set (hm/inv group g)))
                                (hm/elements group))}))
            test-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Dihedral order: $|D_n| = 2n$

(let [results
      (for [n (range 1 25)]
        (= (hm/order (hm/dihedral-group n)) (* 2 n)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Partition Properties

;; ### All parts are positive, descending, and sum to $n$

(let [results
      (for [n (range 1 13)]
        (every? (fn [p]
                  (and (every? pos-int? p)
                       (apply >= p)
                       (= n (reduce + p))))
                (hm/partitions n)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Conjugate is an involution: $(\lambda')' = \lambda$

(let [results
      (for [n (range 1 11)
            p (hm/partitions n)]
        (= p (hm/partition-conjugate (hm/partition-conjugate p))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Conjugate preserves sum: $|\lambda'| = |\lambda|$

(let [results
      (for [n (range 1 11)
            p (hm/partitions n)]
        (= (reduce + p) (reduce + (hm/partition-conjugate p))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Hook-length formula matches SYT enumeration and rep dimension

(let [results
      (for [n (range 1 8)
            lambda (hm/partitions n)]
        (let [hlf (hm/hook-length-dimension lambda)
              syt (count (hm/standard-young-tableaux lambda))
              rep (hm/rep-dimension (hm/irrep lambda))]
          (= hlf syt rep)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Conjugacy class size formula: $|C_\mu| = n! / \prod_k k^{a_k} a_k!$

(defn class-size-formula
  "Conjugacy class size from the partition formula."
  [n mu]
  (let [fact (fn [m] (reduce *' (range 1 (inc m))))
        freq (frequencies mu)]
    (/ (fact n)
       (reduce *' (map (fn [[k ak]]
                         (*' (reduce *' (repeat ak k))
                             (fact ak)))
                       freq)))))

(let [results
      (for [n (range 2 8)]
        (let [G (hm/symmetric-group n)
              classes (hm/conjugacy-classes G)]
          (every? (fn [cls]
                    (let [ct (hm/cycle-type (:representative cls))]
                      (= (:size cls) (class-size-formula n ct))))
                  classes)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Permutation order equals LCM of cycle lengths

(let [G (hm/symmetric-group 5)
      e (hm/id G)]
  (every? (fn [sigma]
            (let [ct (hm/cycle-type sigma)
                  expected (reduce (fn [a b]
                                     (/ (* a b) (biginteger (.gcd (biginteger a) (biginteger b)))))
                                   (map biginteger ct))
                  actual (loop [k 1 current sigma]
                           (if (= current e) k
                               (recur (inc k) (hm/op G current sigma))))]
              (= actual (long expected))))
          (hm/elements G)))

(kind/test-last [true?])

;; ## Additional Representation Properties

;; ### Identity maps to identity matrix: $\rho(e) = I$

(let [results
      (for [n [3 4 5 6]
            lambda (hm/partitions n)]
        (let [ir (hm/irrep lambda)
              d (hm/rep-dimension ir)
              I (fm/rows->mat (mapv (fn [i]
                                      (mapv (fn [j] (if (= i j) 1.0 0.0))
                                            (range d)))
                                    (range d)))
              rho-e (hm/rep-matrix ir (hm/identity-perm n))
              diff (fm/sub rho-e I)
              err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
          (< err 1e-10)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Inverse maps to transpose: $\rho(\sigma^{-1}) = \rho(\sigma)^T$

(let [results
      (for [n [3 4 5]
            lambda (hm/partitions n)]
        (let [G (hm/symmetric-group n)
              ir (hm/irrep lambda)]
          (every? (fn [sigma]
                    (let [rho-inv (hm/rep-matrix ir (hm/inv G sigma))
                          rho-t (fm/transpose (hm/rep-matrix ir sigma))
                          diff (fm/sub rho-inv rho-t)
                          err (Math/sqrt (fm/trace (fm/mulm diff (fm/transpose diff))))]
                      (< err 1e-10)))
                  (hm/elements G))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Character inner product: $\langle \chi_i, \chi_j \rangle = \delta_{ij}$

(let [results
      (for [n [3 4 5]]
        (let [G (hm/symmetric-group n)
              ct (hm/character-table G)
              {:keys [table class-sizes]} ct
              order (hm/order G)
              k (count table)]
          (every? identity
                  (for [i (range k) j (range k)]
                    (let [ip (hm/character-inner-product
                              (nth table i) (nth table j) class-sizes order)
                          expected (if (= i j) 1.0 0.0)]
                      (< (cx/cabs (cx/csub ip (cx/complex expected 0.0))) 1e-8))))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Summary
;;
;; This notebook verified:
;;
;; - **Group axioms**: identity, inverses, associativity, closure, inverse membership
;;   for 15 groups
;; - **Conjugacy classes**: partition the group, sizes sum correctly
;; - **Character table**: row orthogonality, column orthogonality, dimension
;;   sum formula, irrep count = class count, trivial character, integer dimensions,
;;   character inner product = Kronecker delta
;; - **Fourier transform**: round-trip, Parseval's theorem, convolution theorem
;; - **Representations**: homomorphism, orthogonal matrices, trace = character,
;;   Plancherel identity, identity maps to $I$, inverse maps to transpose
;; - **Tensor product**: character multiplicativity, homomorphism, dimension formula
;; - **Direct sum**: character additivity, dimension formula
;; - **Group actions**: orbit-stabilizer theorem, Burnside = orbit count,
;;   Pólya = Burnside
;; - **Dihedral groups**: presentation relations, character orthogonality, order = $2n$
;; - **Product groups**: order formula, class count formula
;; - **Permutations**: sign homomorphism, cycle type determines class,
;;   decomposition round-trip, order = LCM of cycle lengths
;; - **Partitions**: validity, conjugate involution, conjugate preserves sum,
;;   hook-length formula, class size formula
