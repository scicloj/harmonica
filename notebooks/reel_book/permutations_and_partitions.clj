;; # Permutations and Partitions
;;
;; This notebook is a deep dive into the algebraic properties of
;; **permutations** and **partitions** — the two foundational data types
;; in the representation theory of symmetric groups. We verify identities
;; systematically and visualize partitions as Young diagrams.
;;
;; For a narrative introduction to permutations, cycles, and the symmetric
;; group, see [Symmetric Groups](symmetric_groups.html).

(ns reel-book.permutations-and-partitions
  (:require
   [scicloj.reel.core :as reel]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Permutation Identities
;;
;; The following verifications cover the fundamental algebraic properties
;; of permutations in $S_n$. Each identity is tested exhaustively on
;; small symmetric groups.

;; ### Sign is a homomorphism
;;
;; $\operatorname{sign}(\sigma \tau) =
;; \operatorname{sign}(\sigma) \cdot \operatorname{sign}(\tau)$.
;; We verify for all pairs in $S_5$.

(let [G (reel/symmetric-group 5)
      elts (vec (reel/elements G))]
  (every? (fn [[s t]]
            (= (reel/sign (reel/op G s t))
               (* (reel/sign s) (reel/sign t))))
          (for [a elts b elts] [a b])))

(kind/test-last [true?])

;; ### Inverse round-trip
;;
;; $\sigma \circ \sigma^{-1} = \sigma^{-1} \circ \sigma = e$ for all
;; permutations in $S_n$.

(let [results
      (for [n (range 1 7)]
        (let [G (reel/symmetric-group n)
              e (reel/id G)]
          (every? (fn [sigma]
                    (let [si (reel/inv G sigma)]
                      (and (= (reel/op G sigma si) e)
                           (= (reel/op G si sigma) e))))
                  (reel/elements G))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Adjacent transposition decomposition
;;
;; Every permutation can be written as a product of adjacent transpositions
;; $s_i = (i, i{+}1)$. We verify the decomposition reconstructs the
;; original for all of $S_6$.

(let [results
      (for [n (range 1 7)]
        (let [G (reel/symmetric-group n)
              id-perm (reel/id G)
              make-swap (fn [i]
                          (let [v (vec (range n))]
                            (assoc v i (inc i) (inc i) i)))]
          (every? (fn [sigma]
                    (let [swaps (reel/adjacent-transposition-decomposition sigma)
                          reconstructed (reduce (fn [p i] (reel/op G p (make-swap i)))
                                                id-perm swaps)]
                      (= sigma reconstructed)))
                  (reel/elements G))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Cycle type determines conjugacy class
;;
;; Two permutations are conjugate in $S_n$ if and only if they have the
;; same cycle type. We verify this for $S_3$ through $S_6$.

(let [results
      (for [n (range 3 7)]
        (let [G (reel/symmetric-group n)
              classes (reel/conjugacy-classes G)]
          (every? (fn [cls]
                    (= 1 (count (set (map reel/cycle-type (:elements cls))))))
                  classes)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Composition is associative
;;
;; $(\sigma \circ \tau) \circ \rho = \sigma \circ (\tau \circ \rho)$,
;; verified exhaustively for $S_4$.

(let [G (reel/symmetric-group 4)
      elts (vec (reel/elements G))]
  (every? (fn [[a b c]]
            (= (reel/op G (reel/op G a b) c)
               (reel/op G a (reel/op G b c))))
          (for [a elts b elts c elts] [a b c])))

(kind/test-last [true?])

;; ### Order of a permutation
;;
;; The **order** of $\sigma$ is the smallest positive $k$ such that
;; $\sigma^k = e$. It equals the LCM of the cycle lengths.

(defn perm-order
  "Compute the order of a permutation by repeated composition."
  [G sigma]
  (let [e (reel/id G)]
    (loop [k 1
           current sigma]
      (if (= current e)
        k
        (recur (inc k) (reel/op G current sigma))))))

;; Verify: order = lcm of cycle lengths, for all of $S_5$.

(let [G (reel/symmetric-group 5)]
  (every? (fn [sigma]
            (let [ct (reel/cycle-type sigma)
                  expected (reduce (fn [a b] (/ (* a b) (biginteger (.gcd (biginteger a) (biginteger b)))))
                                  (map biginteger ct))]
              (= (perm-order G sigma) (long expected))))
          (reel/elements G)))

(kind/test-last [true?])

;; ## Partitions
;;
;; A **partition** of $n$ is a way to write $n$ as a sum of positive integers
;; in descending order. Partitions are central to the representation theory
;; of $S_n$: they index both conjugacy classes and irreducible representations.

;; ### Enumeration

(reel/partitions 1)

(kind/test-last [= [[1]]])

(reel/partitions 4)

(kind/test-last [= [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]])

(reel/partitions 5)

(kind/test-last [= [[5] [4 1] [3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]]])

;; The number of partitions $p(n)$ grows sub-exponentially.

(kind/table
 {:column-names ["$n$" "$p(n)$"]
  :row-vectors (mapv (fn [n] [n (count (reel/partitions n))]) (range 1 16))})

;; All parts are positive and sum to $n$.

(let [results
      (for [n (range 1 13)]
        (every? (fn [p]
                  (and (every? pos-int? p)
                       (apply >= p)
                       (= n (reduce + p))))
                (reel/partitions n)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Young diagrams as SVG
;;
;; A partition is visualized as a **Young diagram**: a left-justified array
;; of boxes where row $i$ has $\lambda_i$ boxes.

(defn young-diagram-svg
  "Render a partition as an SVG Young diagram."
  [lambda & {:keys [cell-size fill stroke]
             :or {cell-size 28 fill "#4a90d9" stroke "#2c3e50"}}]
  (let [rows (count lambda)
        max-cols (if (seq lambda) (first lambda) 0)
        w (+ (* max-cols cell-size) 2)
        h (+ (* rows cell-size) 2)]
    (into [:svg {:width w :height h
                 :xmlns "http://www.w3.org/2000/svg"}]
          (for [r (range rows)
                c (range (nth lambda r))]
            [:rect {:x (+ 1 (* c cell-size))
                    :y (+ 1 (* r cell-size))
                    :width (dec cell-size)
                    :height (dec cell-size)
                    :fill fill
                    :stroke stroke
                    :stroke-width 1.5
                    :rx 2}]))))

;; The partitions of 5 and their Young diagrams:

(kind/hiccup
 (into [:div {:style "display: flex; flex-wrap: wrap; gap: 24px; align-items: flex-end;"}]
       (for [p (reel/partitions 5)]
         [:div {:style "text-align: center;"}
          (young-diagram-svg p)
          [:div {:style "margin-top: 4px; font-family: monospace; font-size: 13px;"}
           (str p)]])))

;; ### Conjugate (transpose) partition
;;
;; The **conjugate** $\lambda'$ of a partition $\lambda$ is obtained by
;; reflecting the Young diagram along the main diagonal — swapping rows
;; and columns.

(reel/partition-conjugate [4 2 1])

(kind/test-last [= [3 2 1 1]])

(reel/partition-conjugate [3 3])

(kind/test-last [= [2 2 2]])

(reel/partition-conjugate [5])

(kind/test-last [= [1 1 1 1 1]])

;; A partition and its conjugate, side by side:

(kind/hiccup
 (let [p [4 2 1]
       pc (reel/partition-conjugate p)]
   [:div {:style "display: flex; gap: 40px; align-items: flex-end;"}
    [:div {:style "text-align: center;"}
     (young-diagram-svg p)
     [:div {:style "margin-top: 4px; font-family: monospace;"} (str "λ = " p)]]
    [:div {:style "text-align: center; font-size: 24px; align-self: center;"} "↔"]
    [:div {:style "text-align: center;"}
     (young-diagram-svg pc :fill "#e67e22")
     [:div {:style "margin-top: 4px; font-family: monospace;"} (str "λ' = " pc)]]]))

;; Conjugate is an involution: $(\lambda')' = \lambda$.

(let [results
      (for [n (range 1 11)
            p (reel/partitions n)]
        (= p (reel/partition-conjugate (reel/partition-conjugate p))))]
  (every? true? results))

(kind/test-last [true?])

;; Conjugate preserves the sum: $|\lambda'| = |\lambda|$.

(let [results
      (for [n (range 1 11)
            p (reel/partitions n)]
        (= (reduce + p) (reduce + (reel/partition-conjugate p))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Hook lengths and the hook-length formula
;;
;; For each cell $(i, j)$ in a Young diagram, the **hook length** $h(i,j)$
;; counts the cells directly to the right and directly below, plus the cell
;; itself.
;;
;; The **hook-length formula** gives the number of standard Young tableaux
;; (SYT) of shape $\lambda$:
;;
;; $$f^\lambda = \frac{n!}{\prod_{(i,j) \in \lambda} h(i,j)}$$
;;
;; This equals the dimension of the irreducible representation of $S_n$
;; indexed by $\lambda$.

(defn hook-lengths
  "Compute all hook lengths for a partition."
  [lambda]
  (let [conj (reel/partition-conjugate lambda)]
    (for [i (range (count lambda))
          j (range (nth lambda i))]
      (+ (- (nth lambda i) j)
         (- (nth conj j) i)
         -1))))

;; Visualize hook lengths on a Young diagram:

(defn young-hooks-svg
  "Render a Young diagram with hook lengths displayed in each cell."
  [lambda & {:keys [cell-size] :or {cell-size 36}}]
  (let [conj (reel/partition-conjugate lambda)
        rows (count lambda)
        max-cols (first lambda)
        w (+ (* max-cols cell-size) 2)
        h (+ (* rows cell-size) 2)]
    (into [:svg {:width w :height h
                 :xmlns "http://www.w3.org/2000/svg"}]
          (for [r (range rows)
                c (range (nth lambda r))
                :let [hook (+ (- (nth lambda r) c)
                              (- (nth conj c) r)
                              -1)]]
            [:g
             [:rect {:x (+ 1 (* c cell-size))
                     :y (+ 1 (* r cell-size))
                     :width (dec cell-size)
                     :height (dec cell-size)
                     :fill "#ecf0f1"
                     :stroke "#2c3e50"
                     :stroke-width 1.5
                     :rx 2}]
             [:text {:x (+ 1 (* c cell-size) (/ cell-size 2))
                     :y (+ 1 (* r cell-size) (/ cell-size 2) 5)
                     :text-anchor "middle"
                     :font-family "monospace"
                     :font-size 14
                     :fill "#2c3e50"}
              (str hook)]]))))

;; Hook lengths for $[4, 2, 1]$:

(kind/hiccup (young-hooks-svg [4 2 1]))

;; Hook lengths for $[3, 2, 2]$:

(kind/hiccup (young-hooks-svg [3 2 2]))

;; ### Hook-length formula verification
;;
;; The product of hook lengths divides $n!$ to give the number of SYT,
;; which equals the count from explicit enumeration.

(defn factorial [n] (reduce *' (range 1 (inc n))))

(let [results
      (for [n (range 1 8)
            lambda (reel/partitions n)]
        (let [hooks (hook-lengths lambda)
              hook-product (reduce *' hooks)
              formula-dim (/ (factorial n) hook-product)
              enum-dim (count (reel/standard-young-tableaux lambda))
              lib-dim (reel/hook-length-dimension lambda)]
          (and (= formula-dim enum-dim)
               (= formula-dim lib-dim))))]
  (every? true? results))

(kind/test-last [true?])

;; Dimension table for partitions of 5:

(kind/table
 {:column-names ["Partition λ" "Hook product" "f^λ = 5!/hooks" "# SYT (enumerated)"]
  :row-vectors
  (mapv (fn [lambda]
          (let [hooks (hook-lengths lambda)
                hp (reduce *' hooks)
                dim (/ (factorial 5) hp)
                enum (count (reel/standard-young-tableaux lambda))]
            [(str lambda) (str hp) (str dim) (str enum)]))
        (reel/partitions 5))})

;; ### Dimension-squared sum
;;
;; The sum of squared dimensions of all irreps equals $|S_n| = n!$:
;;
;; $$\sum_{\lambda \vdash n} (f^\lambda)^2 = n!$$

(let [results
      (for [n (range 1 9)]
        (let [dims (map reel/hook-length-dimension (reel/partitions n))
              sum-sq (reduce + (map #(* % %) dims))]
          (= sum-sq (factorial n))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Standard Young Tableaux
;;
;; A **standard Young tableau** (SYT) of shape $\lambda$ is a filling of the
;; Young diagram with $1, 2, \ldots, n$ such that entries increase along each
;; row and down each column. SYTs form the basis for Young's orthogonal
;; representation.

(reel/standard-young-tableaux [2 1])

(kind/test-last [= [[[1 2] [3]] [[1 3] [2]]]])

(count (reel/standard-young-tableaux [3 2]))

(kind/test-last [= 5])

;; Visualize all SYTs of shape $[3, 2]$:

(defn syt-svg
  "Render a standard Young tableau as SVG with numbers in cells."
  [syt & {:keys [cell-size] :or {cell-size 32}}]
  (let [rows (count syt)
        max-cols (apply max (map count syt))
        w (+ (* max-cols cell-size) 2)
        h (+ (* rows cell-size) 2)]
    (into [:svg {:width w :height h
                 :xmlns "http://www.w3.org/2000/svg"}]
          (for [r (range rows)
                c (range (count (nth syt r)))
                :let [val (nth (nth syt r) c)]]
            [:g
             [:rect {:x (+ 1 (* c cell-size))
                     :y (+ 1 (* r cell-size))
                     :width (dec cell-size)
                     :height (dec cell-size)
                     :fill "#d5e8d4"
                     :stroke "#2c3e50"
                     :stroke-width 1.5
                     :rx 2}]
             [:text {:x (+ 1 (* c cell-size) (/ cell-size 2))
                     :y (+ 1 (* r cell-size) (/ cell-size 2) 5)
                     :text-anchor "middle"
                     :font-family "monospace"
                     :font-size 14
                     :font-weight "bold"
                     :fill "#2c3e50"}
              (str val)]]))))

(kind/hiccup
 (into [:div {:style "display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start;"}]
       (for [t (reel/standard-young-tableaux [3 2])]
         [:div {:style "text-align: center;"}
          (syt-svg t)])))

;; ### Conjugacy class sizes
;;
;; The size of the conjugacy class in $S_n$ with cycle type $\mu$ is
;;
;; $$|C_\mu| = \frac{n!}{\prod_k k^{a_k} \cdot a_k!}$$
;;
;; where $a_k$ is the number of parts equal to $k$.

(defn class-size-formula
  "Compute conjugacy class size from the partition formula."
  [n mu]
  (let [freq (frequencies mu)]
    (/ (factorial n)
       (reduce *' (map (fn [[k ak]]
                         (*' (reduce *' (repeat ak k))
                             (factorial ak)))
                       freq)))))

;; Verify: the formula matches the actual class sizes from enumeration,
;; and all sizes sum to $n!$.

(let [results
      (for [n (range 2 8)]
        (let [G (reel/symmetric-group n)
              classes (reel/conjugacy-classes G)]
          (and
           ;; Sizes match formula
           (every? (fn [cls]
                     (let [ct (reel/cycle-type (:representative cls))]
                       (= (:size cls) (class-size-formula n ct))))
                   classes)
           ;; Sizes sum to n!
           (= (reduce + (map :size classes)) (factorial n)))))]
  (every? true? results))

(kind/test-last [true?])

;; Table of conjugacy classes for $S_5$:

(let [G (reel/symmetric-group 5)
      classes (reel/conjugacy-classes G)]
  (kind/table
   {:column-names ["Cycle type" "Class size" "Formula"]
    :row-vectors (mapv (fn [cls]
                         (let [ct (reel/cycle-type (:representative cls))]
                           [(str ct)
                            (:size cls)
                            (class-size-formula 5 ct)]))
                       classes)}))

;; ## Permutation cycle diagram
;;
;; A cycle diagram draws elements around a circle with arrows showing
;; where each element maps.

(defn cycle-diagram-svg
  "Render a permutation as a cycle diagram SVG."
  [sigma & {:keys [radius] :or {radius 80}}]
  (let [n (count sigma)
        cx (+ radius 30)
        cy (+ radius 30)
        w (* 2 (+ radius 30))
        h (* 2 (+ radius 30))
        angle (fn [i] (- (* 2 Math/PI (/ i (double n))) (/ Math/PI 2)))
        px (fn [i] (+ cx (* radius (Math/cos (angle i)))))
        py (fn [i] (+ cy (* radius (Math/sin (angle i)))))
        node-r 14]
    (into
     [:svg {:width w :height h :xmlns "http://www.w3.org/2000/svg"}
      [:defs
       [:marker {:id "arrowhead" :markerWidth 8 :markerHeight 6
                 :refX 7 :refY 3 :orient "auto"}
        [:polygon {:points "0 0, 8 3, 0 6" :fill "#e74c3c"}]]]]
     (concat
      ;; Arrows for non-fixed points
      (for [i (range n)
            :when (not= i (sigma i))
            :let [j (sigma i)
                  x1 (px i) y1 (py i)
                  x2 (px j) y2 (py j)
                  dx (- x2 x1) dy (- y2 y1)
                  dist (Math/sqrt (+ (* dx dx) (* dy dy)))
                  ;; Shorten arrow to avoid overlapping nodes
                  ux (/ dx dist) uy (/ dy dist)
                  sx (+ x1 (* ux (+ node-r 2)))
                  sy (+ y1 (* uy (+ node-r 2)))
                  ex (- x2 (* ux (+ node-r 4)))
                  ey (- y2 (* uy (+ node-r 4)))]]
        [:line {:x1 sx :y1 sy :x2 ex :y2 ey
                :stroke "#e74c3c" :stroke-width 1.5
                :marker-end "url(#arrowhead)"}])
      ;; Nodes
      (for [i (range n)
            :let [fixed? (= i (sigma i))]]
        [:g
         [:circle {:cx (px i) :cy (py i) :r node-r
                   :fill (if fixed? "#bdc3c7" "#3498db")
                   :stroke "#2c3e50" :stroke-width 1.5}]
         [:text {:x (px i) :y (+ (py i) 5)
                 :text-anchor "middle"
                 :font-family "monospace" :font-size 13
                 :font-weight "bold" :fill "white"}
          (str i)]])))))

;; The 4-cycle $(0\;1\;2\;3)$:

(kind/hiccup (cycle-diagram-svg [1 2 3 0]))

;; Two disjoint 2-cycles $(0\;1)(2\;3)$:

(kind/hiccup (cycle-diagram-svg [1 0 3 2]))

;; A permutation with a fixed point: $(0\;2\;4)(1\;3)$, 5 is fixed.

(kind/hiccup (cycle-diagram-svg [2 3 4 1 0 5]))

;; ## Summary of verified identities
;;
;; This notebook verified:
;;
;; - Sign is a group homomorphism ($S_5$, all $120^2$ pairs)
;; - Inverse round-trip for $S_1$ through $S_6$ (all elements)
;; - Adjacent transposition decomposition reconstructs original ($S_1$ through $S_6$)
;; - Cycle type determines conjugacy class ($S_3$ through $S_6$)
;; - Associativity exhaustively verified for $S_4$ (all $24^3$ triples)
;; - Permutation order equals LCM of cycle lengths ($S_5$, all elements)
;; - Partitions are valid (positive, descending, correct sum) for $n = 1, \ldots, 12$
;; - Conjugate is an involution for $n = 1, \ldots, 10$
;; - Conjugate preserves the sum for $n = 1, \ldots, 10$
;; - Hook-length formula matches SYT enumeration for $n = 1, \ldots, 7$
;; - Dimension-squared sum equals $n!$ for $n = 1, \ldots, 8$
;; - Conjugacy class size formula matches enumeration for $S_2$ through $S_7$

