;; # Groups and Structure
;;
;; A **group** $(G, \cdot)$ is a set $G$ with an operation satisfying
;; closure, associativity, identity, and inverses. This notebook
;; systematically verifies these axioms and explores the structural
;; properties of every group type in the library.

(ns reel-book.groups-and-structure
  (:require
   [scicloj.reel.core :as reel]
   [scicloj.kindly.v4.kind :as kind]))

;; ## All groups under test
;;
;; We test a diverse collection: small and large cyclic, symmetric,
;; dihedral, and product groups.

(def all-groups
  [{:label "Z/1Z" :group (reel/cyclic-group 1)}
   {:label "Z/2Z" :group (reel/cyclic-group 2)}
   {:label "Z/3Z" :group (reel/cyclic-group 3)}
   {:label "Z/7Z" :group (reel/cyclic-group 7)}
   {:label "Z/12Z" :group (reel/cyclic-group 12)}
   {:label "S_1" :group (reel/symmetric-group 1)}
   {:label "S_2" :group (reel/symmetric-group 2)}
   {:label "S_3" :group (reel/symmetric-group 3)}
   {:label "S_4" :group (reel/symmetric-group 4)}
   {:label "S_5" :group (reel/symmetric-group 5)}
   {:label "D_3" :group (reel/dihedral-group 3)}
   {:label "D_4" :group (reel/dihedral-group 4)}
   {:label "D_5" :group (reel/dihedral-group 5)}
   {:label "D_6" :group (reel/dihedral-group 6)}
   {:label "D_8" :group (reel/dihedral-group 8)}
   {:label "D_12" :group (reel/dihedral-group 12)}
   {:label "Z/2Z × Z/2Z" :group (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 2))}
   {:label "Z/2Z × Z/3Z" :group (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 3))}
   {:label "Z/3Z × Z/4Z" :group (reel/product-group (reel/cyclic-group 3) (reel/cyclic-group 4))}
   {:label "D_3 × Z/2Z" :group (reel/product-group (reel/dihedral-group 3) (reel/cyclic-group 2))}])

;; ## Identity axiom
;;
;; $e \cdot g = g \cdot e = g$ for all $g \in G$.

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [e (reel/id group)]
                {:group label
                 :pass? (every? (fn [g]
                                  (and (= (reel/op group e g) g)
                                       (= (reel/op group g e) g)))
                                (reel/elements group))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Inverse axiom
;;
;; $g \cdot g^{-1} = g^{-1} \cdot g = e$ for all $g \in G$.

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [e (reel/id group)]
                {:group label
                 :pass? (every? (fn [g]
                                  (let [gi (reel/inv group g)]
                                    (and (= (reel/op group g gi) e)
                                         (= (reel/op group gi g) e))))
                                (reel/elements group))}))
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
              (let [elts (vec (reel/elements group))
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
                                  (= (reel/op group (reel/op group a b) c)
                                     (reel/op group a (reel/op group b c))))
                                triples)}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Closure
;;
;; The product of any two elements is in the group.

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [elt-set (set (reel/elements group))]
                {:group label
                 :pass? (every? (fn [g]
                                  (every? (fn [h]
                                            (contains? elt-set (reel/op group g h)))
                                          (reel/elements group)))
                                (reel/elements group))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Inverse is in the group

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [elt-set (set (reel/elements group))]
                {:group label
                 :pass? (every? (fn [g]
                                  (contains? elt-set (reel/inv group g)))
                                (reel/elements group))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ## Order
;;
;; The `order` function returns the number of elements.

(let [results
      (mapv (fn [{:keys [label group]}]
              {:group label
               :pass? (= (reel/order group) (count (reel/elements group)))})
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; Group sizes:

(kind/table
 {:column-names ["Group" "Order"]
  :row-vectors (mapv (fn [{:keys [label group]}]
                       [label (reel/order group)])
                     all-groups)})

;; ## Conjugacy classes
;;
;; ### Classes partition the group

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [classes (reel/conjugacy-classes group)
                    all-elts (mapcat :elements classes)
                    group-set (set (reel/elements group))]
                {:group label
                 :pass? (and (= (count all-elts) (count (set all-elts)))
                             (= (set all-elts) group-set))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Sizes sum to order

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [classes (reel/conjugacy-classes group)]
                {:group label
                 :pass? (= (reel/order group)
                            (reduce + (map :size classes)))}))
            all-groups)]
  (every? :pass? results))

(kind/test-last [true?])

;; ### Reported size matches element count

(let [results
      (mapv (fn [{:keys [label group]}]
              (let [classes (reel/conjugacy-classes group)]
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
            :when (<= (reel/order group) 120)]
        (let [classes (reel/conjugacy-classes group)]
          (every? (fn [cls]
                    (let [rep (:representative cls)]
                      (every? (fn [h]
                                (some (fn [x]
                                        (= h (reel/op group x
                                                        (reel/op group rep
                                                                 (reel/inv group x)))))
                                      (reel/elements group)))
                              (:elements cls))))
                  classes)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Dihedral group structure
;;
;; ### Presentation: $r^n = s^2 = e$, $s r s = r^{-1}$

(let [results
      (for [n (range 2 21)]
        (let [G (reel/dihedral-group n)
              e (reel/id G)
              r [:r 1]
              s [:s 0]
              r-n (reduce (fn [acc _] (reel/op G acc r)) e (range n))
              s-2 (reel/op G s s)
              srs (reel/op G s (reel/op G r s))
              r-inv (reel/inv G r)]
          (and (= r-n e) (= s-2 e) (= srs r-inv))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Conjugacy class count
;;
;; $D_n$ has $(n+3)/2$ classes for odd $n$, and $n/2 + 3$ for even $n$.

(let [results
      (for [n (range 2 25)]
        (let [G (reel/dihedral-group n)
              actual (count (reel/conjugacy-classes G))
              expected (if (odd? n)
                         (/ (+ n 3) 2)
                         (+ (/ n 2) 3))]
          (= actual expected)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Order is $2n$

(let [results
      (for [n (range 1 25)]
        (= (reel/order (reel/dihedral-group n)) (* 2 n)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Product group structure
;;
;; ### Order: $|G_1 \times G_2| = |G_1| \cdot |G_2|$

(let [results
      (for [[G1 G2] [[(reel/cyclic-group 2) (reel/cyclic-group 3)]
                      [(reel/cyclic-group 4) (reel/cyclic-group 5)]
                      [(reel/dihedral-group 3) (reel/cyclic-group 2)]
                      [(reel/dihedral-group 4) (reel/dihedral-group 3)]
                      [(reel/symmetric-group 3) (reel/cyclic-group 2)]]]
        (let [P (reel/product-group G1 G2)]
          (= (reel/order P) (* (reel/order G1) (reel/order G2)))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Class count: $k(G_1 \times G_2) = k(G_1) \cdot k(G_2)$

(let [results
      (for [[G1 G2] [[(reel/cyclic-group 3) (reel/cyclic-group 4)]
                      [(reel/dihedral-group 3) (reel/cyclic-group 2)]
                      [(reel/dihedral-group 4) (reel/dihedral-group 3)]
                      [(reel/symmetric-group 3) (reel/cyclic-group 3)]]]
        (let [P (reel/product-group G1 G2)]
          (= (count (reel/conjugacy-classes P))
             (* (count (reel/conjugacy-classes G1))
                (count (reel/conjugacy-classes G2))))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Abelian groups: commutativity
;;
;; Cyclic groups and products of cyclic groups are abelian:
;; $g \cdot h = h \cdot g$ for all $g, h$.

(let [abelian-groups [(reel/cyclic-group 5)
                      (reel/cyclic-group 12)
                      (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 3))
                      (reel/product-group (reel/cyclic-group 3) (reel/cyclic-group 4))]
      results
      (for [G abelian-groups]
        (every? (fn [g]
                  (every? (fn [h]
                            (= (reel/op G g h) (reel/op G h g)))
                          (reel/elements G)))
                (reel/elements G)))]
  (every? true? results))

(kind/test-last [true?])

;; Non-abelian groups have non-commuting elements:

(let [G (reel/symmetric-group 3)
      a [1 0 2]
      b [0 2 1]]
  (not= (reel/op G a b) (reel/op G b a)))

(kind/test-last [true?])

(let [G (reel/dihedral-group 3)]
  (not= (reel/op G [:r 1] [:s 0]) (reel/op G [:s 0] [:r 1])))

(kind/test-last [true?])

;; ## Cayley table visualization
;;
;; For small groups, the Cayley table shows the complete multiplication
;; structure.

(defn cayley-table-svg
  "Render a Cayley table as an SVG grid with colored cells."
  [G & {:keys [cell-size] :or {cell-size 28}}]
  (let [elts (vec (reel/elements G))
        n (count elts)
        elt-idx (into {} (map-indexed (fn [i e] [e i]) elts))
        ;; Generate colors for each element
        colors (mapv (fn [i]
                       (let [hue (* 360.0 (/ i (double n)))]
                         (str "hsl(" (int hue) ",70%,75%)")))
                     (range n))
        header cell-size
        w (+ header (* n cell-size) 2)
        h (+ header (* n cell-size) 2)]
    (into [:svg {:width w :height h :xmlns "http://www.w3.org/2000/svg"
                 :style "font-family: monospace; font-size: 11px;"}]
          (concat
           ;; Column headers
           (for [j (range n)]
             [:text {:x (+ header (* j cell-size) (/ cell-size 2))
                     :y (- header 4)
                     :text-anchor "middle" :font-size 9 :fill "#555"}
              (str (elts j))])
           ;; Row headers
           (for [i (range n)]
             [:text {:x (- header 4)
                     :y (+ header (* i cell-size) (/ cell-size 2) 4)
                     :text-anchor "end" :font-size 9 :fill "#555"}
              (str (elts i))])
           ;; Table cells
           (for [i (range n) j (range n)
                 :let [prod (reel/op G (elts i) (elts j))
                       k (elt-idx prod)]]
             [:rect {:x (+ header (* j cell-size))
                     :y (+ header (* i cell-size))
                     :width (dec cell-size) :height (dec cell-size)
                     :fill (colors k) :stroke "#fff" :stroke-width 0.5}])))))

;; Cayley table for $\mathbb{Z}/4\mathbb{Z}$:

(kind/hiccup (cayley-table-svg (reel/cyclic-group 4)))

;; Cayley table for $D_3$ (6 elements):

(kind/hiccup (cayley-table-svg (reel/dihedral-group 3)))

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

