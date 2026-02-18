;; # Groups and Structure
;;
;; A **[group](https://en.wikipedia.org/wiki/Group_(mathematics))** $(G, \cdot)$ is a set $G$ with an operation satisfying
;; closure, associativity, identity, and inverses. This notebook
;; systematically verifies these axioms and explores the structural
;; properties of every group type in the library.

(ns harmonica-book.groups-and-structure
  (:require
   [scicloj.harmonica.core :as hm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## The group types in the library
;;
;; The library provides four families of groups. The first two —
;; **cyclic groups** and **symmetric groups** — were introduced in
;; [The DFT as Group Fourier Transform](dft_as_group_fourier.html) and
;; [Symmetric Groups](symmetric_groups.html). Here we introduce the
;; remaining two.
;;
;; ### Dihedral groups
;;
;; The **[dihedral group](https://en.wikipedia.org/wiki/Dihedral_group)** $D_n$ is the symmetry group of a regular $n$-gon:
;; $n$ rotations and $n$ reflections, for a total of $2n$ elements.
;;
;; Elements are represented as tagged pairs:
;;
;; - $[:r\; k]$ — rotation by $2\pi k/n$ (for $k = 0, \ldots, n{-}1$)
;; - $[:s\; k]$ — reflection (for $k = 0, \ldots, n{-}1$)
;;
;; The group is defined by the **[presentation](https://en.wikipedia.org/wiki/Presentation_of_a_group)** $r^n = s^2 = e$ and
;; $s r s = r^{-1}$: rotating then reflecting then rotating again
;; is the same as reflecting in a different axis.

(let [G (hm/dihedral-group 4)]
  {:order (hm/order G)
   :identity (hm/id G)
   :elements (vec (hm/elements G))})

;; A rotation composed with a reflection gives a reflection:

(let [G (hm/dihedral-group 5)]
  (hm/op G [:r 2] [:s 0]))

;; ### Product groups
;;
;; The **[direct product](https://en.wikipedia.org/wiki/Direct_product_of_groups)** $G_1 \times G_2$ has elements that are pairs
;; $[g, h]$ with $g \in G_1$, $h \in G_2$. All operations act
;; componentwise: $(g_1, h_1) \cdot (g_2, h_2) = (g_1 g_2,\; h_1 h_2)$.
;;
;; The simplest non-trivial example is the **[Klein four-group](https://en.wikipedia.org/wiki/Klein_four-group)**
;; $V_4 = \mathbb{Z}/2\mathbb{Z} \times \mathbb{Z}/2\mathbb{Z}$, which
;; has 4 elements and is the smallest non-cyclic group:

(let [V4 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2))]
  {:order (hm/order V4)
   :elements (vec (hm/elements V4))})

;; ## All groups under test
;;
;; We test a diverse collection: small and large cyclic, symmetric,
;; dihedral, and product groups.

(def all-groups
  [{:label "Z/1Z" :group (hm/cyclic-group 1)}
   {:label "Z/2Z" :group (hm/cyclic-group 2)}
   {:label "Z/3Z" :group (hm/cyclic-group 3)}
   {:label "Z/7Z" :group (hm/cyclic-group 7)}
   {:label "Z/12Z" :group (hm/cyclic-group 12)}
   {:label "S_1" :group (hm/symmetric-group 1)}
   {:label "S_2" :group (hm/symmetric-group 2)}
   {:label "S_3" :group (hm/symmetric-group 3)}
   {:label "S_4" :group (hm/symmetric-group 4)}
   {:label "S_5" :group (hm/symmetric-group 5)}
   {:label "D_3" :group (hm/dihedral-group 3)}
   {:label "D_4" :group (hm/dihedral-group 4)}
   {:label "D_5" :group (hm/dihedral-group 5)}
   {:label "D_6" :group (hm/dihedral-group 6)}
   {:label "D_8" :group (hm/dihedral-group 8)}
   {:label "D_12" :group (hm/dihedral-group 12)}
   {:label "Z/2Z × Z/2Z" :group (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2))}
   {:label "Z/2Z × Z/3Z" :group (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3))}
   {:label "Z/3Z × Z/4Z" :group (hm/product-group (hm/cyclic-group 3) (hm/cyclic-group 4))}
   {:label "D_3 × Z/2Z" :group (hm/product-group (hm/dihedral-group 3) (hm/cyclic-group 2))}])

;; ## Identity axiom
;;
;; $e \cdot g = g \cdot e = g$ for all $g \in G$.

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [e (hm/id group)]
                {:group label
                 :pass? (every? (fn [g]
                                  (and (= (hm/op group e g) g)
                                       (= (hm/op group g e) g)))
                                (hm/elements group))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Inverse axiom
;;
;; $g \cdot g^{-1} = g^{-1} \cdot g = e$ for all $g \in G$.

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [e (hm/id group)]
                {:group label
                 :pass? (every? (fn [g]
                                  (let [gi (hm/inv group g)]
                                    (and (= (hm/op group g gi) e)
                                         (= (hm/op group gi g) e))))
                                (hm/elements group))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Associativity
;;
;; $(g \cdot h) \cdot k = g \cdot (h \cdot k)$
;;
;; We test exhaustively for small groups and sample for larger ones.

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [elts (vec (hm/elements group))
                    n (count elts)
                    triples (if (<= n 24)
                              (for [a elts b elts c elts] [a b c])
                              (let [rng (java.util.Random. 42)]
                                (repeatedly 1000
                                            (fn [] [(elts (.nextInt rng n))
                                                    (elts (.nextInt rng n))
                                                    (elts (.nextInt rng n))]))))]
                {:group label
                 :pass? (every? (fn [[a b c]]
                                  (= (hm/op group (hm/op group a b) c)
                                     (hm/op group a (hm/op group b c))))
                                triples)}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Closure
;;
;; The product of any two elements is in the group.

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [elt-set (set (hm/elements group))]
                {:group label
                 :pass? (every? (fn [g]
                                  (every? (fn [h]
                                            (contains? elt-set (hm/op group g h)))
                                          (hm/elements group)))
                                (hm/elements group))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Inverse is in the group

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [elt-set (set (hm/elements group))]
                {:group label
                 :pass? (every? (fn [g]
                                  (contains? elt-set (hm/inv group g)))
                                (hm/elements group))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Order
;;
;; The `order` function returns the number of elements.

(let [results
      (mapv (fn [{:keys [label group]}]
              {:group label
               :pass? (= (hm/order group) (count (hm/elements group)))})
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; Group sizes:

(kind/table
 {:column-names ["Group" "Order"]
  :row-vectors (mapv (fn [{:keys [label group]}]
                       [label (hm/order group)])
                     all-groups)})

;; ## Conjugacy classes
;;
;; ### Classes partition the group

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [classes (hm/conjugacy-classes group)
                    all-elts (mapcat :elements classes)
                    group-set (set (hm/elements group))]
                {:group label
                 :pass? (and (= (count all-elts) (count (set all-elts)))
                             (= (set all-elts) group-set))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Sizes sum to order

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [classes (hm/conjugacy-classes group)]
                {:group label
                 :pass? (= (hm/order group)
                           (reduce + (map :size classes)))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Reported size matches element count

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [classes (hm/conjugacy-classes group)]
                {:group label
                 :pass? (every? (fn [c]
                                  (= (:size c) (count (:elements c))))
                                classes)}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Conjugacy: all elements in a class are actually conjugate
;;
;; For each class $C$, for every $g, h \in C$, there exists some
;; $x \in G$ such that $x g x^{-1} = h$.

(let [results
      (for [{:keys [label group]} all-groups
            :when (<= (hm/order group) 120)]
        (let [classes (hm/conjugacy-classes group)]
          (every? (fn [cls]
                    (let [rep (:representative cls)]
                      (every? (fn [h]
                                (some (fn [x]
                                        (= h (hm/op group x
                                                      (hm/op group rep
                                                               (hm/inv group x)))))
                                      (hm/elements group)))
                              (:elements cls))))
                  classes)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Dihedral group structure
;;
;; ### Presentation: $r^n = s^2 = e$, $s r s = r^{-1}$

(let [results
      (for [n (range 2 21)]
        (let [G (hm/dihedral-group n)
              e (hm/id G)
              r [:r 1]
              s [:s 0]
              r-n (reduce (fn [acc _] (hm/op G acc r)) e (range n))
              s-2 (hm/op G s s)
              srs (hm/op G s (hm/op G r s))
              r-inv (hm/inv G r)]
          (and (= r-n e) (= s-2 e) (= srs r-inv))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Conjugacy class count
;;
;; $D_n$ has $(n+3)/2$ classes for odd $n$, and $n/2 + 3$ for even $n$.

(let [results
      (for [n (range 2 25)]
        (let [G (hm/dihedral-group n)
              actual (count (hm/conjugacy-classes G))
              expected (if (odd? n)
                         (/ (+ n 3) 2)
                         (+ (/ n 2) 3))]
          (= actual expected)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Order is $2n$

(let [results
      (for [n (range 1 25)]
        (= (hm/order (hm/dihedral-group n)) (* 2 n)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Product group structure
;;
;; ### Order: $|G_1 \times G_2| = |G_1| \cdot |G_2|$

(let [results
      (for [[G1 G2] [[(hm/cyclic-group 2) (hm/cyclic-group 3)]
                     [(hm/cyclic-group 4) (hm/cyclic-group 5)]
                     [(hm/dihedral-group 3) (hm/cyclic-group 2)]
                     [(hm/dihedral-group 4) (hm/dihedral-group 3)]
                     [(hm/symmetric-group 3) (hm/cyclic-group 2)]]]
        (let [P (hm/product-group G1 G2)]
          (= (hm/order P) (* (hm/order G1) (hm/order G2)))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Class count: $k(G_1 \times G_2) = k(G_1) \cdot k(G_2)$

(let [results
      (for [[G1 G2] [[(hm/cyclic-group 3) (hm/cyclic-group 4)]
                     [(hm/dihedral-group 3) (hm/cyclic-group 2)]
                     [(hm/dihedral-group 4) (hm/dihedral-group 3)]
                     [(hm/symmetric-group 3) (hm/cyclic-group 3)]]]
        (let [P (hm/product-group G1 G2)]
          (= (count (hm/conjugacy-classes P))
             (* (count (hm/conjugacy-classes G1))
                (count (hm/conjugacy-classes G2))))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Abelian groups: commutativity
;;
;; Cyclic groups and products of cyclic groups are abelian:
;; $g \cdot h = h \cdot g$ for all $g, h$.

(let [abelian-groups [(hm/cyclic-group 5)
                      (hm/cyclic-group 12)
                      (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 3))
                      (hm/product-group (hm/cyclic-group 3) (hm/cyclic-group 4))]
      results
      (for [G abelian-groups]
        (every? (fn [g]
                  (every? (fn [h]
                            (= (hm/op G g h) (hm/op G h g)))
                          (hm/elements G)))
                (hm/elements G)))]
  (every? true? results))

(kind/test-last [true?])

;; Non-abelian groups have non-commuting elements:

(let [G (hm/symmetric-group 3)
      a [1 0 2]
      b [0 2 1]]
  (not= (hm/op G a b) (hm/op G b a)))

(kind/test-last [true?])

(let [G (hm/dihedral-group 3)]
  (not= (hm/op G [:r 1] [:s 0]) (hm/op G [:s 0] [:r 1])))

(kind/test-last [true?])

;; ## Cayley table visualization
;;
;; For small groups, the Cayley table shows the complete multiplication
;; structure.

;; The library provides `hm/cayley-table-svg`:

;; Cayley table for $\mathbb{Z}/4\mathbb{Z}$:

(kind/hiccup (hm/cayley-table-svg (hm/cyclic-group 4)))

;; Cayley table for $D_3$ (6 elements):

(kind/hiccup (hm/cayley-table-svg (hm/dihedral-group 3)))

;; ## Summary of verified identities
;;
;; This notebook verified across 20 groups (cyclic, symmetric, dihedral, product):
;;
;; - **Identity axiom**: $eg = ge = g$ for all elements
;; - **Inverse axiom**: $gg^{-1} = g^{-1}g = e$ for all elements
;; - **Associativity**: $(gh)k = g(hk)$, exhaustive or sampled
;; - **Closure**: products and inverses stay in the group
;; - **Order**: `order` = `(count (elements G))`
;; - **Conjugacy classes**: partition, size sum, actual conjugacy verified
;; - **Dihedral**: presentation relations for $n = 2, \ldots, 20$, class counts,
;;   order $= 2n$
;; - **Product**: order and class count formulas
;; - **Commutativity**: abelian groups commute, non-abelian don't

