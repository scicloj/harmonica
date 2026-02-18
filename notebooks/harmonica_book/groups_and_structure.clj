;; # Groups and Structure
;;
;; A **[group](https://en.wikipedia.org/wiki/Group_(mathematics))** is a set with an operation that satisfies a few simple
;; rules. Groups appear everywhere: in clock arithmetic, in the symmetries
;; of a snowflake, in shuffling a deck of cards. This notebook introduces
;; the idea through concrete examples, building intuition before stating
;; the formal axioms.

(ns harmonica-book.groups-and-structure
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Integers under addition
;;
;; The most familiar group is the **integers** $\mathbb{Z}$ with addition.
;; You already know its structure:
;;
;; - **Identity**: adding 0 changes nothing. $n + 0 = 0 + n = n$.
;; - **Inverses**: every integer $n$ has an opposite $-n$. $n + (-n) = 0$.
;; - **Associativity**: $(a + b) + c = a + (b + c)$. You can group
;;   additions however you like.
;;
;; These three properties — identity, inverses, associativity — are exactly
;; the **group axioms**. The integers under addition form a group. It
;; happens to be an infinite one, but the same idea works for finite sets.

;; ## Clock arithmetic — the cyclic group
;;
;; A clock has 12 hours. After 12 o'clock comes 1 again — arithmetic
;; wraps around. This is **[modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic)**: addition mod $n$.
;;
;; The set $\{0, 1, \ldots, n{-}1\}$ with addition mod $n$ forms the
;; **[cyclic group](https://en.wikipedia.org/wiki/Cyclic_group)** $\mathbb{Z}/n\mathbb{Z}$. It has $n$ elements and
;; every element is a power of a single generator.
;;
;; In the [DFT notebook](dft_as_group_fourier.html) we saw that the Fourier
;; transform is secretly the character table of a cyclic group. Here we
;; look at the group itself.

(def C12 (hm/cyclic-group 12))

(hm/order C12)

(kind/test-last [= 12])

;; The identity is 0, just like in integer addition.

(hm/id C12)

(kind/test-last [= 0])

;; The operation is addition mod 12.

(hm/op C12 7 8)

(kind/test-last [= 3])

;; The inverse of 5 is 7, because $5 + 7 = 12 \equiv 0 \pmod{12}$.

(hm/inv C12 5)

(kind/test-last [= 7])

;; ## Symmetries of a square — the dihedral group
;;
;; Pick up a square coaster and put it back on the table. How many
;; ways can you do this so it looks the same? You can **rotate** it by
;; $0°$, $90°$, $180°$, or $270°$. You can also **flip** it over any of
;; four axes (two diagonals, two midlines). That gives $4 + 4 = 8$
;; symmetries.
;;
;; These symmetries form the **[dihedral group](https://en.wikipedia.org/wiki/Dihedral_group)** $D_4$.
;; In general, $D_n$ is the symmetry group of a regular $n$-gon,
;; with $n$ rotations and $n$ reflections for a total of $2n$ elements.
;;
;; Elements are tagged pairs:
;;
;; - $[:r\; k]$ — rotation by $2\pi k/n$
;; - $[:s\; k]$ — reflection

(def D4 (hm/dihedral-group 4))

(hm/order D4)

(kind/test-last [= 8])

;; The identity is rotation by 0.

(hm/id D4)

(kind/test-last [= [:r 0]])

;; All 8 elements:

(vec (hm/elements D4))

;; Two rotations compose by adding angles:

(hm/op D4 [:r 1] [:r 2])

(kind/test-last [= [:r 3]])

;; A rotation composed with a reflection gives a different reflection:

(hm/op D4 [:r 1] [:s 0])

;; Reflecting twice returns to the identity:

(hm/op D4 [:s 0] [:s 0])

(kind/test-last [= [:r 0]])

;; ### Non-commutativity
;;
;; Unlike clock arithmetic, the **order matters** for dihedral groups.
;; Rotating then reflecting is not the same as reflecting then rotating.
;; This is what makes $D_n$ a **non-abelian** group.

(hm/op D4 [:r 1] [:s 0])

(hm/op D4 [:s 0] [:r 1])

(let [a (hm/op D4 [:r 1] [:s 0])
      b (hm/op D4 [:s 0] [:r 1])]
  (not= a b))

(kind/test-last [true?])

;; ### The defining relations
;;
;; The dihedral group is completely determined by the
;; **[presentation](https://en.wikipedia.org/wiki/Presentation_of_a_group)**:
;; $r^n = s^2 = e$ and $s r s = r^{-1}$.
;;
;; In words: rotating $n$ times gets you back to start, reflecting
;; twice gets you back, and the interplay between rotation and
;; reflection is captured by the third relation.

(let [e (hm/id D4)
      r [:r 1]
      s [:s 0]
      r4 (reduce (fn [acc _] (hm/op D4 acc r)) e (range 4))
      s2 (hm/op D4 s s)
      srs (hm/op D4 s (hm/op D4 r s))
      r-inv (hm/inv D4 r)]
  {"r⁴ = e" (= r4 e)
   "s² = e" (= s2 e)
   "srs = r⁻¹" (= srs r-inv)})

(kind/test-last
 [(fn [m] (every? true? (vals m)))])

;; ## Rearranging objects — the symmetric group
;;
;; A **[permutation](https://en.wikipedia.org/wiki/Permutation)** rearranges $n$ objects. The set of all
;; permutations of $n$ objects forms the **[symmetric group](https://en.wikipedia.org/wiki/Symmetric_group)** $S_n$,
;; with $n!$ elements. We explore $S_n$ in depth in the
;; [next chapter](symmetric_groups.html); here, a quick preview.

(def S3 (hm/symmetric-group 3))

(hm/order S3)

(kind/test-last [= 6])

;; Permutations are stored as vectors: position $i$ maps to the value
;; at index $i$. Composition is right-to-left.

(hm/op S3 [1 2 0] [0 2 1])

;; $S_3$ is non-abelian — order of composition matters for $n \geq 3$.

(let [a [1 2 0] b [0 2 1]]
  (not= (hm/op S3 a b) (hm/op S3 b a)))

(kind/test-last [true?])

;; ## Combining groups — the product group
;;
;; Given two groups $G_1$ and $G_2$, their **[direct product](https://en.wikipedia.org/wiki/Direct_product_of_groups)**
;; $G_1 \times G_2$ has elements that are pairs $[g, h]$. The operation
;; acts on each component independently.
;;
;; The simplest non-trivial example is the **[Klein four-group](https://en.wikipedia.org/wiki/Klein_four-group)**
;; $V_4 = \mathbb{Z}/2\mathbb{Z} \times \mathbb{Z}/2\mathbb{Z}$:

(def V4 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2)))

(hm/order V4)

(kind/test-last [= 4])

(vec (hm/elements V4))

;; Every element is its own inverse — a distinctive property of $V_4$.

(every? (fn [g] (= (hm/op V4 g g) (hm/id V4)))
        (hm/elements V4))

(kind/test-last [true?])

;; Products can mix different group types. For instance,
;; $D_3 \times \mathbb{Z}/2\mathbb{Z}$ has $6 \times 2 = 12$ elements:

(hm/order (hm/product-group (hm/dihedral-group 3) (hm/cyclic-group 2)))

(kind/test-last [= 12])

;; ## The group axioms
;;
;; Every example above satisfies the same three rules. A **group** is a
;; set $G$ with a binary operation $\cdot$ such that:
;;
;; 1. **Identity**: There is an element $e$ with $e \cdot g = g \cdot e = g$
;;    for all $g$.
;; 2. **Inverses**: Every $g$ has an inverse $g^{-1}$ with
;;    $g \cdot g^{-1} = g^{-1} \cdot g = e$.
;; 3. **Associativity**: $(g \cdot h) \cdot k = g \cdot (h \cdot k)$
;;    for all $g, h, k$.
;;
;; Let's verify all three on $D_4$.

;; Identity:

(every? (fn [g]
          (and (= (hm/op D4 (hm/id D4) g) g)
               (= (hm/op D4 g (hm/id D4)) g)))
        (hm/elements D4))

(kind/test-last [true?])

;; Inverses:

(every? (fn [g]
          (let [gi (hm/inv D4 g)
                e (hm/id D4)]
            (and (= (hm/op D4 g gi) e)
                 (= (hm/op D4 gi g) e))))
        (hm/elements D4))

(kind/test-last [true?])

;; Associativity (all $8^3 = 512$ triples):

(let [elts (vec (hm/elements D4))]
  (every? (fn [[a b c]]
            (= (hm/op D4 (hm/op D4 a b) c)
               (hm/op D4 a (hm/op D4 b c))))
          (for [a elts b elts c elts] [a b c])))

(kind/test-last [true?])

;; If a group is **[abelian](https://en.wikipedia.org/wiki/Abelian_group)** (commutative), we also have
;; $g \cdot h = h \cdot g$ for all $g, h$. Cyclic groups and products
;; of cyclic groups are abelian. Symmetric groups ($n \geq 3$) and
;; dihedral groups ($n \geq 3$) are not.
;;
;; For exhaustive axiom verification across all group types, see
;; [Algebraic Identities](algebraic_identities.html).

;; ## Conjugacy classes
;;
;; Two elements $g$ and $h$ are **[conjugate](https://en.wikipedia.org/wiki/Conjugacy_class)** if there exists some
;; $x \in G$ with $x g x^{-1} = h$. Conjugation is an equivalence
;; relation — it partitions the group into **conjugacy classes**.
;;
;; Conjugate elements "play the same structural role" in the group.
;; In $S_n$, conjugacy classes are determined by
;; [cycle type](https://en.wikipedia.org/wiki/Cycle_type):
;; two permutations are conjugate if and only if they have the same cycle structure.

(let [classes (hm/conjugacy-classes D4)]
  (kind/table
   {:column-names ["Representative" "Size" "Elements"]
    :row-vectors (mapv (fn [c] [(str (:representative c))
                                (:size c)
                                (str (vec (:elements c)))])
                       classes)}))

;; The class sizes always sum to the group order:

(let [classes (hm/conjugacy-classes D4)]
  (reduce + (map :size classes)))

(kind/test-last [= 8])

;; The number of conjugacy classes determines the number of irreducible
;; representations — a key fact for character theory.

;; ## Cayley tables
;;
;; For a small group, the **[Cayley table](https://en.wikipedia.org/wiki/Cayley_table)** shows every product at a glance.
;; It is the complete multiplication table.

;; $\mathbb{Z}/4\mathbb{Z}$:

(kind/hiccup (hm/cayley-table-svg (hm/cyclic-group 4)))

;; $D_3$ (the symmetry group of a triangle):

(kind/hiccup (hm/cayley-table-svg (hm/dihedral-group 3)))

;; Notice that the Cayley table of $\mathbb{Z}/4\mathbb{Z}$ is symmetric
;; across the diagonal (abelian), while $D_3$'s is not (non-abelian).

;; ## The groups in the library
;;
;; The library provides four families:
;;
;; | Family | Notation | Order | Abelian? |
;; |:-------|:---------|:------|:---------|
;; | Cyclic | $\mathbb{Z}/n\mathbb{Z}$ | $n$ | Yes |
;; | Symmetric | $S_n$ | $n!$ | No ($n \geq 3$) |
;; | Dihedral | $D_n$ | $2n$ | No ($n \geq 3$) |
;; | Product | $G_1 \times G_2$ | $|G_1| \cdot |G_2|$ | If both factors are |

(kind/table
 {:column-names ["Group" "Order" "# Classes"]
  :row-vectors
  (mapv (fn [[label G]]
          [label (hm/order G) (count (hm/conjugacy-classes G))])
        [["Z/4Z" (hm/cyclic-group 4)]
         ["Z/12Z" (hm/cyclic-group 12)]
         ["S_3" (hm/symmetric-group 3)]
         ["S_4" (hm/symmetric-group 4)]
         ["S_5" (hm/symmetric-group 5)]
         ["D_4" (hm/dihedral-group 4)]
         ["D_6" (hm/dihedral-group 6)]
         ["D_12" (hm/dihedral-group 12)]
         ["Z/2Z × Z/2Z" V4]
         ["D_3 × Z/2Z" (hm/product-group (hm/dihedral-group 3) (hm/cyclic-group 2))]])})

;; ## What comes next
;;
;; With the group concept in hand:
;;
;; - **[Symmetric Groups](symmetric_groups.html)** — permutations, partitions,
;;   Young diagrams, and the combinatorial heart of $S_n$
;; - **[Character Theory](character_theory.html)** — the character table
;;   encodes irreducible representations as complex numbers
;; - **[Group Actions](group_actions.html)** — orbits, stabilizers, Burnside
;;   counting, and Pólya enumeration
;;
;; For applications, see [Counting Necklaces](counting_necklaces.html)
;; (Burnside meets combinatorics), [Chord Geometry](chord_geometry.html)
;; (music theory as group action), and
;; [Symmetry Sketchpad](symmetry_sketchpad.html)
;; (rosette patterns from dihedral groups).
