;; # Group Actions
;;
;; A **group action** is a way for a group to act on a set — each group
;; element becomes a transformation of the set, and group multiplication
;; corresponds to composition of transformations.
;;
;; This notebook covers the orbit-stabilizer theorem, Burnside's lemma,
;; the cycle index, and Pólya enumeration, with thorough algebraic
;; verification.

(ns harmonica-book.group-actions
  (:require
   [scicloj.harmonica.core :as hm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Actions of cyclic groups
;;
;; The cyclic group $C_n$ acts on $\{0, \ldots, n{-}1\}$ by rotation:
;; $g \cdot x = (x + g) \bmod n$.

(defn rotation-action [n]
  (fn [g x] (mod (+ (long x) (long g)) n)))

;; The orbit of any point under $C_n$ is the entire set (the action
;; is transitive).

(let [G (hm/cyclic-group 5)
      act (rotation-action 5)]
  (hm/orbit G act 0))

(kind/test-last [= #{0 1 2 3 4}])

;; There is exactly one orbit.

(let [G (hm/cyclic-group 5)
      act (rotation-action 5)]
  (count (hm/orbits G act (range 5))))

(kind/test-last [= 1])

;; ## Actions of dihedral groups
;;
;; The dihedral group $D_n$ acts on vertices of a regular $n$-gon:
;;
;; - Rotation $[:r\; k]$ sends vertex $x$ to $(x + k) \bmod n$
;; - Reflection $[:s\; k]$ sends vertex $x$ to $(k - x) \bmod n$

(defn dihedral-vertex-action [n]
  (fn [[t k] x]
    (case t
      :r (mod (+ (long x) (long k)) n)
      :s (mod (- (long k) (long x)) n))))

;; $D_4$ acts transitively on 4 vertices.

(let [G (hm/dihedral-group 4)
      act (dihedral-vertex-action 4)]
  (count (hm/orbits G act (range 4))))

(kind/test-last [= 1])

;; ## Orbit-stabilizer theorem
;;
;; For any group $G$ acting on a set $X$:
;;
;; $$|G| = |\text{Orb}(x)| \cdot |\text{Stab}(x)|$$
;;
;; We verify this for dihedral groups acting on vertices.

(let [results
      (for [n (range 3 13)]
        (let [G (hm/dihedral-group n)
              act (dihedral-vertex-action n)
              order (hm/order G)]
          (every? (fn [x]
                    (= order (* (count (hm/orbit G act x))
                                (count (hm/stabilizer G act x)))))
                  (range n))))]
  (every? true? results))

(kind/test-last [true?])

;; Verify for cyclic groups acting on themselves (regular representation).

(let [results
      (for [n [2 3 5 7 11 13]]
        (let [G (hm/cyclic-group n)
              act (rotation-action n)]
          (every? (fn [x]
                    (= n (* (count (hm/orbit G act x))
                            (count (hm/stabilizer G act x)))))
                  (range n))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Orbit-stabilizer for subset actions
;;
;; $S_n$ acts on $k$-element subsets of $\{0, \ldots, n{-}1\}$. The
;; orbit-stabilizer theorem still holds.

(let [results
      (for [n [4 5]
            k [2 3]]
        (let [G (hm/symmetric-group n)
              perm-act (fn [sigma x] (sigma x))
              {:keys [act domain]} (hm/subset-action perm-act (range n) k)]
          ;; Check for a sample of subsets
          (every? (fn [x]
                    (= (hm/order G)
                       (* (count (hm/orbit G act x))
                          (count (hm/stabilizer G act x)))))
                  (take 5 domain))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Fixed points
;;
;; $\text{Fix}(g) = \{x \in X : g \cdot x = x\}$

;; The identity fixes every point.

(let [G (hm/dihedral-group 6)
      act (dihedral-vertex-action 6)
      e (hm/id G)]
  (count (hm/fixed-points act e (range 6))))

(kind/test-last [= 6])

;; A rotation by 1 in $D_n$ fixes no vertices (for $n > 1$).

(let [results
      (for [n (range 3 13)]
        (let [act (dihedral-vertex-action n)]
          (= 0 (count (hm/fixed-points act [:r 1] (range n))))))]
  (every? true? results))

(kind/test-last [true?])

;; A reflection in $D_n$ fixes at most 2 vertices (for odd $n$, exactly 1).

(let [results
      (for [n (range 3 13)]
        (let [act (dihedral-vertex-action n)
              fix-count (count (hm/fixed-points act [:s 0] (range n)))]
          (if (odd? n)
            (= fix-count 1)
            (<= fix-count 2))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Burnside's lemma
;;
;; $$|\text{orbits}| = \frac{1}{|G|} \sum_{g \in G} |\text{Fix}(g)|$$
;;
;; We verify that Burnside's count matches the actual orbit count for
;; colorings of beads under cyclic and dihedral groups.
;;
;; The library provides `coloring-action` to generate all $k$-colorings
;; and lift a point action to colorings — analogous to `subset-action`
;; for $k$-element subsets.

;; ### Cyclic group on binary colorings

(let [results
      (for [n (range 2 9)
            k [2 3]]
        (let [G (hm/cyclic-group n)
              {:keys [domain act]} (hm/coloring-action (rotation-action n) n k)
              actual-orbits (count (hm/orbits G act domain))
              burnside (hm/burnside-count G act domain)]
          (= actual-orbits burnside)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Dihedral group on binary colorings (bracelets)

(let [results
      (for [n (range 3 9)
            k [2 3]]
        (let [G (hm/dihedral-group n)
              {:keys [domain act]} (hm/coloring-action (dihedral-vertex-action n) n k)
              actual-orbits (count (hm/orbits G act domain))
              burnside (hm/burnside-count G act domain)]
          (= actual-orbits burnside)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Known necklace and bracelet counts
;;
;; Binary necklaces (cyclic, OEIS A000031 for n≥1): 2, 3, 4, 6, 8, 14, 20, 36, 60, ...
;; Binary bracelets (dihedral, OEIS A000029 for n≥1): 2, 3, 4, 6, 8, 13, 18, 30, 46, ...

(def known-necklaces [2 3 4 6 8 14 20 36 60])
(def known-bracelets [2 3 4 6 8 13 18 30 46])

(let [results
      (for [n (range 1 (inc (count known-necklaces)))]
        (let [G (hm/cyclic-group n)
              {:keys [domain act]} (hm/coloring-action (rotation-action n) n 2)]
          (= (hm/burnside-count G act domain) (nth known-necklaces (dec n)))))]
  (every? true? results))

(kind/test-last [true?])

(let [results
      (for [n (range 1 (inc (count known-bracelets)))]
        (let [G (hm/dihedral-group n)
              {:keys [domain act]} (hm/coloring-action (dihedral-vertex-action n) n 2)]
          (= (hm/burnside-count G act domain) (nth known-bracelets (dec n)))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Cycle index
;;
;; The **cycle index** of a group action is a polynomial that encodes the
;; cycle structure of every group element's permutation of the domain:
;;
;; $$Z(G) = \frac{1}{|G|} \sum_{g \in G} \prod_i p_i^{c_i(g)}$$
;;
;; Coefficients are rational numbers summing to 1.

(let [G (hm/cyclic-group 4)
      act (rotation-action 4)
      ci (hm/cycle-index G act (range 4))]
  (= 1 (reduce + (vals ci))))

(kind/test-last [true?])

;; Cycle index coefficients sum to 1 for diverse groups and actions.

(let [results
      (concat
       (for [n (range 2 10)]
         (let [G (hm/cyclic-group n)
               act (rotation-action n)
               ci (hm/cycle-index G act (range n))]
           (= 1 (reduce + (vals ci)))))
       (for [n (range 3 10)]
         (let [G (hm/dihedral-group n)
               act (dihedral-vertex-action n)
               ci (hm/cycle-index G act (range n))]
           (= 1 (reduce + (vals ci))))))]
  (every? true? results))

(kind/test-last [true?])

;; ## Pólya enumeration
;;
;; Evaluating the cycle index with $p_i = k$ counts the number of distinct
;; $k$-colorings: $|colorings| = Z(G; p_1 = k, p_2 = k, \ldots)$.

;; ### Pólya = Burnside for cyclic colorings

(let [results
      (for [n (range 2 9)
            k [2 3 4]]
        (let [G (hm/cyclic-group n)
              act (rotation-action n)
              ci (hm/cycle-index G act (range n))
              polya (hm/polya-count ci k)
              {:keys [domain] act-col :act} (hm/coloring-action act n k)
              burnside (hm/burnside-count G act-col domain)]
          (= polya burnside)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Pólya = Burnside for dihedral colorings

(let [results
      (for [n (range 3 8)
            k [2 3]]
        (let [G (hm/dihedral-group n)
              act (dihedral-vertex-action n)
              ci (hm/cycle-index G act (range n))
              polya (hm/polya-count ci k)
              {:keys [domain] act-col :act} (hm/coloring-action act n k)
              burnside (hm/burnside-count G act-col domain)]
          (= polya burnside)))]
  (every? true? results))

(kind/test-last [true?])

;; ### Known Pólya counts
;;
;; Binary necklaces with $n$ beads and $k = 2$ colors under $C_n$ (OEIS A000031):

(let [results
      (for [n (range 1 10)]
        (let [G (hm/cyclic-group n)
              act (rotation-action n)
              ci (hm/cycle-index G act (range n))]
          (= (hm/polya-count ci 2)
             (nth known-necklaces (dec n)))))]
  (every? true? results))

(kind/test-last [true?])

;; ### Pólya for large $n$
;;
;; The formula works even when enumeration is infeasible.
;; 100-bead binary necklaces under $C_{100}$:

(let [G (hm/cyclic-group 100)
      act (rotation-action 100)
      ci (hm/cycle-index G act (range 100))]
  (hm/polya-count ci 2))

(kind/test-last [= 12676506002282305273966813560N])

;; ## Subset actions
;;
;; The symmetric group $S_n$ acts on $k$-element subsets. The number
;; of orbits is $\binom{n}{k}$ since the action is transitive (any
;; $k$-subset can be mapped to any other).

(let [results
      (for [n [4 5 6]
            k (range 1 n)]
        (let [G (hm/symmetric-group n)
              perm-act (fn [sigma x] (sigma x))
              {:keys [act domain]} (hm/subset-action perm-act (range n) k)
              orbit-count (count (hm/orbits G act domain))]
          (= orbit-count 1)))]
  (every? true? results))

(kind/test-last [true?])

;; Under the cyclic group, the number of orbits on $k$-subsets equals
;; the number of binary necklaces with $k$ ones and $n-k$ zeros, i.e.
;; the Burnside count for binary colorings with weight $k$.

(let [results
      (for [n [5 6 7]
            k [2 3]]
        (let [G (hm/cyclic-group n)
              point-act (rotation-action n)
              {:keys [act domain]} (hm/subset-action point-act (range n) k)
              subset-orbits (count (hm/orbits G act domain))
              ;; Binary colorings of weight k
              {all-cols :domain act-col :act} (hm/coloring-action point-act n 2)
              weighted-colorings (filter (fn [c] (= k (reduce + c))) all-cols)
              weighted-orbits (count (hm/orbits G act-col weighted-colorings))]
          (= subset-orbits weighted-orbits)))]
  (every? true? results))

(kind/test-last [true?])

;; ## Summary of verified identities
;;
;; This notebook verified:
;;
;; - **Orbit-stabilizer theorem** for dihedral groups ($n = 3, \ldots, 12$),
;;   cyclic groups, and subset actions
;; - **Burnside's lemma** matches actual orbit count for cyclic and dihedral
;;   colorings ($n$ up to 8, $k$ up to 3)
;; - **Known necklace counts** (OEIS A000031) for $n = 1, \ldots, 9$
;; - **Known bracelet counts** (OEIS A000029) for $n = 1, \ldots, 9$
;; - **Cycle index coefficients** sum to 1 for all groups tested
;; - **Pólya = Burnside** for cyclic and dihedral colorings
;; - **Large Pólya**: 100-bead binary necklaces computed via formula
;; - **Subset action transitivity** under $S_n$
;; - **Subset orbits = weighted necklaces** under $C_n$
;; - **Fixed point properties**: identity fixes all, rotation by 1 fixes none,
;;   reflection fixes at most 2

;; For applications of group actions, see
;; [Counting Necklaces](counting_necklaces.html) and
;; [Chord Geometry](chord_geometry.html).
