;; # Counting Necklaces — Burnside's Lemma Visualized
;;
;; How many distinct [necklaces](https://en.wikipedia.org/wiki/Necklace_(combinatorics)) can you make with $n$ beads and $k$ colors,
;; if two necklaces that differ only by rotation are considered the same?
;; What if flipping the necklace over also counts as "the same"?
;;
;; The answers come from **[Burnside's lemma](https://en.wikipedia.org/wiki/Burnside%27s_lemma)** and the **[cycle index polynomial](https://en.wikipedia.org/wiki/Cycle_index)**
;; — tools from group theory that turn symmetry into a counting formula.
;;
;; The key idea: a group **acts** on colorings by permuting bead positions.
;; Two colorings in the same **orbit** — reachable from each other by group
;; elements — are the same necklace. Burnside's lemma counts orbits
;; without enumerating them.

(ns harmonica-book.counting-necklaces
  (:require
   [scicloj.harmonica :as hm]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [scicloj.kindly.v4.kind :as kind]))

;; We'll use `rotation-action` and `dihedral-vertex-action` as point actions
;; on bead positions, then lift them to colorings with `hm/coloring-action`.

(defn rotation-action [n]
  (fn [g x] (mod (+ x g) n)))

(defn dihedral-vertex-action [n]
  (fn [[t k] x]
    (case t
      :r (mod (+ x k) n)
      :s (mod (- k x) n))))

;; ## The Setup
;;
;; A "necklace" with $n$ beads and $k$ colors is an equivalence class of
;; colorings under rotation. Two colorings are the same necklace if one
;; can be rotated into the other.
;;
;; The cyclic group $C_n$ acts on colorings by rotating all bead positions.
;; Two colorings in the same **orbit** under this action are the same necklace.

;; ## Brute Force for Small Cases
;;
;; Let's start with $n = 4$ beads and $k = 2$ colors.
;; There are $2^4 = 16$ colorings total.

(let [n 4
      G (hm/cyclic-group n)
      {:keys [domain act]} (hm/coloring-action (rotation-action n) n 2)
      orbs (hm/orbits G act domain)]
  (kind/table
   {:column-names ["Orbit #" "Size" "Representative"]
    :row-vectors (mapv (fn [i orb]
                         [(inc i) (count orb) (str (first (sort orb)))])
                       (range) (sort-by #(first (sort %)) orbs))}))

;; There are 6 distinct necklaces — down from 16 colorings.

(let [n 4
      G (hm/cyclic-group n)
      {:keys [domain act]} (hm/coloring-action (rotation-action n) n 2)
      orbs (hm/orbits G act domain)]
  (count orbs))

(kind/test-last [= 6])

;; ## Burnside's Lemma
;;
;; Enumerating all orbits is expensive. Burnside's lemma gives us a formula:
;;
;; $$|\text{orbits}| = \frac{1}{|G|} \sum_{g \in G} |\text{Fix}(g)|$$
;;
;; where $\text{Fix}(g) = \{x : g \cdot x = x\}$ is the set of colorings
;; fixed by group element $g$.

(let [n 6
      G (hm/cyclic-group n)
      {:keys [domain act]} (hm/coloring-action (rotation-action n) n 2)
      fix-counts (mapv (fn [g]
                         {:element g
                          :fixed (count (hm/fixed-points act g domain))})
                       (hm/elements G))]
  (kind/table
   {:column-names ["$g$" "$|\\text{Fix}(g)|$"]
    :row-vectors (mapv (fn [{:keys [element fixed]}]
                         [(str element) fixed])
                       fix-counts)}))

;; The Burnside count:

(let [n 6
      G (hm/cyclic-group n)
      {:keys [domain act]} (hm/coloring-action (rotation-action n) n 2)]
  (hm/burnside-count G act domain))

(kind/test-last [= 14])

;; ## The Cycle Index Polynomial
;;
;; The cycle index encodes the cycle structure of each group element's
;; action on the domain. For the cyclic group $C_n$ acting on $n$ positions
;; by rotation, rotation by $k$ positions has cycle type determined by
;; $\gcd(k, n)$.

(let [n 6
      G (hm/cyclic-group n)
      ci (hm/cycle-index G (rotation-action n) (range n))]
  (kind/table
   {:column-names ["Cycle type" "Coefficient"]
    :row-vectors (mapv (fn [[ct coeff]]
                         [(str ct) (str coeff)])
                       (sort-by (comp count first) ci))}))

;;  The cycle index coefficients sum to 1 (it's a probability distribution
;; over cycle types):

(let [n 6
      G (hm/cyclic-group n)
      ci (hm/cycle-index G (rotation-action n) (range n))]
  (reduce + (vals ci)))

(kind/test-last [= 1])

;; Substituting $p_i = k$ in the cycle index gives the number of
;; necklaces with $k$ colors — this is **Pólya enumeration**:

(let [n 6
      G (hm/cyclic-group n)
      ci (hm/cycle-index G (rotation-action n) (range n))]
  (kind/table
   {:column-names ["$k$ (colors)" "Necklaces"]
    :row-vectors (mapv (fn [k] [k (hm/polya-count ci k)])
                       (range 2 8))}))

;;  Pólya gives the same answer as Burnside — verify for a few cases:

(let [n 6
      G (hm/cyclic-group n)
      ci (hm/cycle-index G (rotation-action n) (range n))]
  (hm/polya-count ci 2))

(kind/test-last [= 14])

(let [n 6
      G (hm/cyclic-group n)
      ci (hm/cycle-index G (rotation-action n) (range n))]
  (hm/polya-count ci 3))

(kind/test-last [= 130])

;; ## Bracelets: Adding Reflections
;;
;; A **bracelet** is a necklace that can also be flipped over — the
;; symmetry group is the dihedral group $D_n$ instead of $C_n$.

(let [results
      (mapv (fn [n]
              (let [G-c (hm/cyclic-group n)
                    G-d (hm/dihedral-group n)
                    {domain-c :domain act-c :act} (hm/coloring-action (rotation-action n) n 2)
                    {domain-d :domain act-d :act} (hm/coloring-action (dihedral-vertex-action n) n 2)
                    necklaces (hm/burnside-count G-c act-c domain-c)
                    bracelets (hm/burnside-count G-d act-d domain-d)]
                {:n n :necklaces necklaces :bracelets bracelets}))
            (range 3 10))]
  (kind/table
   {:column-names ["$n$" "Necklaces ($C_n$)" "Bracelets ($D_n$)"]
    :row-vectors (mapv (fn [{:keys [n necklaces bracelets]}]
                         [n necklaces bracelets])
                       results)}))

;;  Verify known values for binary bracelets (OEIS A000029):

(let [n 6
      G-d (hm/dihedral-group n)
      {domain-d :domain act-d :act} (hm/coloring-action (dihedral-vertex-action n) n 2)]
  (hm/burnside-count G-d act-d domain-d))

(kind/test-last [= 13])

;; The difference between necklaces and bracelets grows: some necklaces
;; that look different from above are the same when you can flip them.

;; ## Scaling Up with the Cycle Index
;;
;; For large $n$, enumerating all $k^n$ colorings is infeasible. But the
;; cycle index works purely from the group's action on positions — no need
;; to enumerate colorings.

(let [data (vec
            (for [n (range 3 21)
                  group-type [:cyclic :dihedral]]
              (let [G (if (= group-type :cyclic)
                        (hm/cyclic-group n)
                        (hm/dihedral-group n))
                    point-act (if (= group-type :cyclic)
                                (rotation-action n)
                                (dihedral-vertex-action n))
                    ci (hm/cycle-index G point-act (range n))]
                {:n n
                 :count (long (hm/polya-count ci 2))
                 :type (if (= group-type :cyclic) "Necklaces" "Bracelets")})))]
  (-> (tc/dataset data)
      (plotly/base {:=x :n :=y :count :=color :type})
      (plotly/layer-line)
      (plotly/layer-point {:=mark-size 6})
      (plotly/update-data
       (fn [d] (assoc d
                      :=layout {:title "Binary necklaces and bracelets"
                                :xaxis {:title "n (number of beads)"}
                                :yaxis {:title "Count" :type "log"}})))
      plotly/plot))

;; ## Cube Colorings
;;
;; How many ways can you color the 6 faces of a cube with $k$ colors,
;; up to rotation? The rotation group of the cube is isomorphic to $S_4$
;; (order 24), acting on the 6 faces.
;;
;; The 24 rotations of the cube, expressed as permutations of the faces
;; {0=top, 1=bottom, 2=front, 3=back, 4=left, 5=right}:

(let [;; Cube face permutations under the 24 rotations
      ;; Generated from the standard rotation group of the cube
      G (hm/symmetric-group 4)
      ;; Map S_4 elements to permutations of 6 faces via the
      ;; isomorphism: S_4 acts on the 4 body diagonals, inducing
      ;; permutations of the 6 faces.
      ;; The 3 pairs of opposite faces are: {0,1}, {2,3}, {4,5}
      ;; Each S_4 element permutes these pairs.
      ;; Simpler: use the cycle index of the rotation group directly.
      ;; The cycle index of the cube rotation group acting on 6 faces is known:
      ;; Z = (1/24)(a₁⁶ + 6·a₁²a₄ + 3·a₁²a₂² + 8·a₃² + 6·a₂³)
      cube-cycle-index {[1 1 1 1 1 1] 1/24 ;; identity (1)
                        [1 1 4] 6/24 ;; face rotations ±90° (6)
                        [1 1 2 2] 3/24 ;; face rotations 180° (3)
                        [3 3] 8/24 ;; vertex rotations ±120° (8)
                        [2 2 2] 6/24}] ;; edge rotations 180° (6)
  (kind/table
   {:column-names ["$k$ (colors)" "Distinct cube colorings"]
    :row-vectors (mapv (fn [k]
                         [k (hm/polya-count cube-cycle-index k)])
                       (range 1 8))}))

(let [cube-cycle-index {[1 1 1 1 1 1] 1/24
                        [1 1 4] 6/24
                        [1 1 2 2] 3/24
                        [3 3] 8/24
                        [2 2 2] 6/24}]
  [(hm/polya-count cube-cycle-index 1)
   (hm/polya-count cube-cycle-index 2)
   (hm/polya-count cube-cycle-index 3)
   (hm/polya-count cube-cycle-index 6)])

(kind/test-last [= [1 10 57 2226]])

;; With 2 colors, there are 10 distinct cubes. With 6 colors (one per face),
;; there are 1800 — meaning only $6!/1800 = 720/1800$ of all arrangements
;; are truly distinct.

;; ## Summary
;;
;; This notebook demonstrated:
;;
;; - **Group actions**: a group acting on a set partitions it into orbits
;; - **Burnside's lemma**: counting orbits by averaging fixed-point counts
;; - **The cycle index**: encoding symmetry structure as a polynomial
;; - **Pólya enumeration**: substituting into the cycle index to count colorings
;; - **Necklaces vs bracelets**: $C_n$ (rotations only) vs $D_n$ (rotations + flips)
;; - **Cube colorings**: using a known cycle index for a non-trivial symmetry group

;; For the algebraic foundations behind group actions, see
;; [Group Actions](group_actions.html).

;; For another orbit-counting story — classifying musical chords via
;; dihedral group actions — see [Chord Geometry](chord_geometry.html).

