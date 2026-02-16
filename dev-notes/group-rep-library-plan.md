# Clojure Group Representation Theory Library — Project Plan

## Vision

A Clojure library for computational group theory and representation theory,
built on dtype-next for numerics and Clojure's protocol system for algebraic
abstractions. The unifying thesis: **Fourier analysis on finite groups**
generalizes the DFT, and provides a single framework connecting signal
processing, probability, combinatorics, and symmetry.

The library should be:
- **Educational**: each concept illustrated with a concrete, motivating example
- **Compositional**: groups, representations, and transforms compose via
  protocols and standard Clojure idioms (⊗, ⊕, restriction, induction)
- **Numerical first**: dtype-next matrices, with exact arithmetic as a future
  extension
- **Extensible**: new group families and algorithms slot in via protocols without
  modifying core code

---

## Motivating Applications

Each application demonstrates a different facet of the library and motivates
specific design decisions.

### 1. DFT as Fourier Analysis on ℤ/nℤ

**Story**: The DFT that every programmer knows is secretly the Fourier
transform on the cyclic group. The "frequencies" are the irreducible
representations (characters) of ℤ/nℤ, which are the n-th roots of unity.

**What it demonstrates**:
- Group protocol basics (cyclic group)
- Character table = DFT matrix
- Fourier transform and inverse as inner products with characters
- Convolution theorem: pointwise multiplication in frequency domain =
  convolution in group domain

**What it requires from the library**:
- `CyclicGroup` implementing Group protocol
- Character table computation (roots of unity matrix)
- `fourier-transform` and `inverse-fourier-transform` functions
- Convolution function on group algebra

**Stretch**: Show the DFT on ℤ/n₁ℤ × ℤ/n₂ℤ (2D DFT for images) as the
natural product group construction, recovering the standard 2D FFT.


### 2. Diaconis Card Shuffling Analysis

**Story**: How many times must you shuffle a deck of cards before it's
"random"? Diaconis showed that 7 riffle shuffles suffice for a 52-card deck.
The analysis works by taking the Fourier transform of the shuffle distribution
on S_n and measuring how fast each frequency component decays.

**What it demonstrates**:
- Symmetric group S_n
- Conjugacy classes indexed by partitions
- **Matrix-valued representations** (Young's orthogonal form)
- Fourier transform on a non-abelian group: f̂(ρ) = Σ_σ f(σ)ρ(σ) is a
  d_ρ × d_ρ matrix for each irrep ρ
- Upper bound lemma: ‖Q^*k − U‖² ≤ ¼ Σ_{ρ≠trivial} d_ρ · ‖f̂(ρ)‖²_F
- The "cutoff phenomenon" — sharp transition from far-from-random to random

**Two sub-examples with different requirements**:

(a) **Random transpositions** (Diaconis-Shahshahani):
The distribution is a class function, so f̂(ρ) is a scalar times identity.
Characters suffice. Simpler, good as a first step.

(b) **Riffle shuffles** (Gilbert-Shannon-Reeds / Bayer-Diaconis 1992):
The distribution is NOT a class function — it depends on the specific
permutation, not just its cycle type. This requires actual matrix
representations ρ(σ) and computing the matrix-valued Fourier transform.
This is the famous "7 shuffles" result.

**What it requires from the library**:
- `SymmetricGroup` implementing Group protocol
- Partitions as a data type
- Conjugacy classes of S_n (indexed by partitions of n)
- Character table of S_n (Murnaghan-Nakayama rule or precomputed)
- Matrix representations of S_n (Young's orthogonal form)
- Fourier transform producing matrix-valued coefficients
- Frobenius norm of matrix
- Total variation distance computation
- Convolution powers Q^*k via Fourier domain (multiply Fourier coefficients)

**Key references**:
- Diaconis, "Group Representations in Probability and Statistics" (1988), IMS
  Lecture Notes — the canonical reference, freely available
- Bayer & Diaconis, "Trailing the Dovetail Shuffle to its Lair" (1992)
- Diaconis & Shahshahani, "Generating a random permutation with random
  transpositions" (1981)


### 3. Burnside–Pólya Counting (Necklaces, Colorings)

**Story**: How many distinct necklaces can you make with n beads and k colors?
Two necklaces are "the same" if one is a rotation of the other. The answer
uses the cyclic group acting on colorings, and Burnside's lemma gives a
formula in terms of fixed points — which are counted via characters.

**What it demonstrates**:
- Group actions (a group acting on a set)
- Burnside's lemma: |orbits| = (1/|G|) Σ_{g∈G} |Fix(g)|
- Pólya enumeration: the cycle index polynomial
- Connection between symmetry and counting

**What it requires from the library**:
- Group actions as a protocol or function
- Fixed-point counting
- Cycle index computation for permutation groups
- Works with cyclic and dihedral groups

**Concrete examples**:
- Necklaces with n beads, k colors (cyclic group Cₙ)
- Bracelets (dihedral group Dₙ — includes flips)
- Colorings of cube faces (rotation group of the cube ≅ S₄)

**What it requires from the library (additions)**:
- `DihedralGroup` implementing Group protocol
- Group action protocol: `act(g, x)` and `fixed-points(g, domain)`
- Cycle index polynomial computation


### 4. Musical Pitch Classes and Chord Classification

**Story**: Western music theory uses ℤ/12ℤ (the 12 pitch classes). Intervals
are group elements. Transposition is the group action. Chord types (major,
minor, diminished, ...) are orbits under transposition. Inversion extends this
to the dihedral group D₁₂.

**What it demonstrates**:
- Concrete, tangible use of cyclic and dihedral groups
- Group actions on sets of pitch-class sets
- Orbit enumeration = chord type classification
- Connects to Burnside/Pólya counting
- Musical set theory (Forte numbers, interval vectors)

**What it requires from the library**:
- Cyclic group ℤ/12ℤ and dihedral group D₁₂
- Group actions on subsets of ℤ/12ℤ
- Orbit computation under group action
- (Optional) Interval vector as a Fourier-domain quantity


### 5. Symmetry-Reduced Optimization (stretch goal)

**Story**: Many optimization problems have symmetry. If a cost function is
invariant under a group action, the search space can be reduced by working in
the space of orbits, or by block-diagonalizing the problem using irreducible
representations. This is the RepLAB application domain.

**What it demonstrates**:
- Isotypic decomposition of a representation
- Block-diagonalization via change of basis
- Practical speedup from symmetry exploitation

**Lower priority** — include if time permits, but the mathematical payoff is
real and this connects the library to optimization practitioners.


---

## Architecture

### Core Protocols

```
┌─────────────────────────────────────────────────┐
│  Layer 0: Data Types                            │
│  - Permutation (cycle notation + one-line)      │
│  - Partition (sorted descending ints)           │
│  - StandardYoungTableau                         │
└─────────────────────────────────────────────────┘
         │
┌─────────────────────────────────────────────────┐
│  Layer 1: Group Protocol                        │
│  - op(g, h), inv(g), id                         │
│  - elements, order                              │
│  - Implementations: CyclicGroup, SymmetricGroup,│
│    DihedralGroup, ProductGroup                  │
└─────────────────────────────────────────────────┘
         │
┌─────────────────────────────────────────────────┐
│  Layer 2: Group Structure                       │
│  - conjugacy-classes(G) → seq of classes        │
│  - class-of(G, g) → class label                 │
│  - class-sizes(G) → map from class to size      │
│  - Group actions, orbits, fixed points          │
│  - Cycle index polynomial                       │
└─────────────────────────────────────────────────┘
         │
┌─────────────────────────────────────────────────┐
│  Layer 3: Characters & Character Tables         │
│  - CharacterTable as value (matrix + metadata)  │
│  - character-table(G) → CharacterTable          │
│  - character-inner-product                      │
│  - decompose: express a character as sum of     │
│    irreducible characters                       │
└─────────────────────────────────────────────────┘
         │
┌─────────────────────────────────────────────────┐
│  Layer 4: Representations                       │
│  - Representation protocol:                     │
│    rep(ρ, g) → matrix (dtype-next)              │
│    dim(ρ), group(ρ), label(ρ)                   │
│  - Irrep constructors per group family          │
│    (Young's orthogonal form for Sₙ,             │
│     roots of unity for ℤ/nℤ)                    │
│  - Operations: ⊗ (tensor), ⊕ (direct sum),     │
│    restrict, induce                             │
│  - Character of a representation = trace ∘ rep  │
└─────────────────────────────────────────────────┘
         │
┌─────────────────────────────────────────────────┐
│  Layer 5: Fourier Analysis on Finite Groups     │
│  - fourier-transform(f, G, reps)                │
│    For abelian G: returns scalar coefficients    │
│    For general G: returns matrix per irrep       │
│  - inverse-fourier-transform                    │
│  - convolution via Fourier domain               │
│  - total-variation-distance                     │
│  - Upper bound lemma computation                │
└─────────────────────────────────────────────────┘
```

### Protocol Definitions (Sketch)

```clojure
;; Layer 1
(defprotocol Group
  (op [G g h] "Group operation")
  (inv [G g] "Group inverse")
  (id [G] "Identity element"))

(defprotocol FiniteGroup
  (elements [G] "Sequence of all group elements")
  (order [G] "Number of elements"))

;; Layer 2
(defprotocol GroupStructure
  (conjugacy-classes [G] "Seq of {:representative _ :elements _ :size _}")
  (class-of [G g] "Conjugacy class label/index for element g"))

(defprotocol GroupAction
  (act [action g x] "Apply group element g to point x")
  (domain [action] "The set being acted on"))

;; Layer 4
(defprotocol Representation
  (rep-matrix [rho g] "Matrix (dtype-next) for group element g")
  (rep-dim [rho] "Dimension of the representation")
  (rep-group [rho] "The group this represents")
  (rep-label [rho] "Label for the representation (e.g., a partition)"))
```

### Key Design Decisions

**Elements as values, not objects.**
Group elements are plain Clojure data: integers mod n for cyclic groups,
vectors (one-line notation) or cycle-notation data for permutations. The
Group protocol takes the group as a first argument and operates on these
values. This is idiomatic Clojure and avoids the OOP pattern where elements
"belong to" a group.

**Character table as a value with metadata.**
A character table is a map:
```clojure
{:group G
 :classes [...] ;; seq of conjugacy class representatives/labels
 :class-sizes [...] ;; sizes of each class
 :irrep-labels [...] ;; labels for each irrep (e.g., partitions)
 :table matrix} ;; dtype-next matrix, rows=irreps, cols=classes
```
This makes it inspectable, serializable, and testable.

**Representations are closures with metadata.**
A representation is a reified thing that wraps a function g → matrix, plus
metadata about dimension, group, and label. This lets you compose them
(tensor, direct sum) by constructing new closures.

**dtype-next for all matrices.**
Representation matrices, character tables, Fourier coefficients — all are
dtype-next tensors. This gives you fast linear algebra and interop with the
rest of the Scicloj ecosystem.


---

## Implementation Roadmap

### Phase 1: Cyclic Groups + DFT Demo ✅

Implement:
- [x] Group and FiniteGroup protocols
- [x] CyclicGroup (elements are integers mod n)
- [x] Conjugacy classes for abelian groups (each element is its own class)
- [x] Character table for ℤ/nℤ (the n×n DFT matrix, ω^{jk})
- [x] `fourier-transform` for class functions on abelian groups
- [x] `inverse-fourier-transform`
- [x] `convolve` via Fourier domain
- [x] DFT demo notebook: show that the standard DFT is literally calling
  `fourier-transform` on ℤ/nℤ with a signal as a function on the group

This phase validates the protocol design with the simplest possible group.

### Phase 2: Symmetric Group Scaffolding ✅

Implement:
- [x] Permutation data type (one-line notation + cycle notation conversion)
- [x] Permutation composition, inverse, identity
- [x] SymmetricGroup implementing Group, FiniteGroup
- [x] Partition data type (sorted descending positive ints summing to n)
- [x] Partition enumeration for a given n
- [x] Conjugacy classes of S_n (indexed by partitions = cycle types)
- [x] Class sizes via the formula n! / (1^{a₁}a₁! · 2^{a₂}a₂! · ...)

### Phase 3: Characters of Sₙ + Random Transpositions Demo ✅

Implement:
- [x] Murnaghan-Nakayama rule for computing χ_λ(μ)
  (character of irrep λ evaluated at conjugacy class μ)
  — This is a recursive signed sum over rim hook tableaux.
  — Reference: James & Kerber, or Sagan "The Symmetric Group" ch. 4.6
- [x] Character table of S_n as a CharacterTable value
- [x] Verify against known tables (S_3, S_4, S_5)
- [x] `fourier-transform` for class functions on S_n (scalar coefficients
  because class functions → scalar Fourier coefficients)
- [x] Random transpositions demo: implement Q(σ) for random transpositions,
  compute total variation distance after k steps using characters,
  plot the convergence and cutoff phenomenon

### Phase 4: Matrix Representations of Sₙ + Riffle Shuffle Demo ✅

Implement:
- [x] Standard Young Tableaux enumeration for a partition λ
- [x] Young's orthogonal form: explicit matrix entries for the adjacent
  transposition generators (i, i+1) in each irrep
  — The matrix entry depends on axial distance in the tableau
  — Reference: James & Kerber, or Vershik-Okounkov approach
- [x] Extend representations to all of S_n by expressing any permutation
  as a product of adjacent transpositions and multiplying matrices
- [x] Representation protocol implementation for S_n irreps
- [x] `fourier-transform` generalized: for non-class-functions, compute
  f̂(ρ) = Σ_{σ∈Sₙ} f(σ)·ρ(σ) as a matrix
- [x] Frobenius norm ‖M‖²_F = tr(M*Mᵀ)
- [x] Gilbert-Shannon-Reeds distribution for riffle shuffles
- [x] Riffle shuffle demo: compute total variation distance after k shuffles,
  show the cutoff at k ≈ (3/2)log₂(n), reproduce "7 shuffles" for n=52
- [x] `tensor-product` and `direct-sum` of representations

### Phase 5: Group Actions + Burnside/Pólya + Music Demo

Implement:
- [x] GroupAction (implemented as plain functions in action.clj)
- [x] DihedralGroup (generated by rotation r and reflection s)
- [x] ProductGroup (for future 2D DFT and other constructions)
- [x] `orbits(action, G)` — partition domain into orbits
- [x] `fixed-points(action, g)` — elements fixed by g
- [x] Burnside's lemma: |orbits| = (1/|G|) Σ |Fix(g)|
- [x] Cycle index polynomial for a permutation group
- [x] Necklace counting demo (notebook: counting_necklaces)
- [x] Musical pitch class demo (notebook: chord_geometry)


See `backlog.md` for the full prioritized notebook list (Tiers 1–3).
### Phase 6: Polish + Packaging

- [ ] Consistent namespace structure
- [ ] Docstrings and clay notebooks for each demo
- [ ] Property-based tests using character orthogonality relations as
  invariants (the character table is a unitary matrix up to scaling)
- [ ] Performance benchmarks for S_n representations up to n=8 or so
- [ ] Cross-validation with Wolframite for character tables and Fourier
  transforms


---

## Namespace Structure (Proposed)

```
group-rep/
├── src/
│   └── group_rep/
│       ├── core.clj            ;; Protocols: Group, FiniteGroup,
│       │                       ;;   GroupStructure, Representation
│       ├── fourier.clj         ;; Fourier transform, inverse, convolution,
│       │                       ;;   total variation distance
│       ├── action.clj          ;; GroupAction protocol, orbits, fixed points,
│       │                       ;;   Burnside's lemma, cycle index
│       ├── groups/
│       │   ├── cyclic.clj      ;; CyclicGroup, characters, DFT matrix
│       │   ├── symmetric.clj   ;; SymmetricGroup, permutation ops
│       │   ├── dihedral.clj    ;; DihedralGroup
│       │   └── product.clj     ;; Direct product of groups
│       ├── representations/
│       │   ├── character_table.clj  ;; CharacterTable type, inner products,
│       │   │                        ;;   decomposition
│       │   └── young.clj       ;; Partitions, standard Young tableaux,
│       │                       ;;   Young's orthogonal form,
│       │                       ;;   Murnaghan-Nakayama rule
│       └── util/
│           ├── partition.clj   ;; Partition data type and enumeration
│           └── permutation.clj ;; Permutation data type, cycle notation,
│                               ;;   composition, adjacent transposition
│                               ;;   decomposition
├── notebooks/
│   ├── demo_dft.clj            ;; DFT as Fourier on ℤ/nℤ
│   ├── demo_shuffling.clj      ;; Diaconis shuffling analysis
│   ├── demo_necklaces.clj      ;; Burnside/Pólya counting
│   └── demo_music.clj          ;; Pitch class theory
└── test/
    └── ...
```


---

## Key Algorithms to Implement

### Murnaghan-Nakayama Rule (Phase 3)

Computes χ_λ(μ) — the character of the irrep labeled by partition λ,
evaluated at the conjugacy class labeled by partition μ.

```
χ_λ(μ) = Σ (-1)^{height(H)} · χ_{λ\H}(μ')
```

where the sum is over all rim hooks H of λ with length equal to the first
part of μ, λ\H is the partition obtained by removing H, μ' is μ with its
first part removed, and height(H) = (number of rows spanned by H) − 1.

Base case: χ_∅(∅) = 1.

This is naturally recursive and functional. The main sub-algorithms are:
- Enumerate rim hooks of a given length in a partition (work with the
  Young diagram as a set of coordinates)
- Remove a rim hook from a partition
- Compute the height (= leg length) of a rim hook

Reference: Sagan, "The Symmetric Group", Theorem 4.10.2.


### Young's Orthogonal Form (Phase 4)

For each partition λ of n, and each adjacent transposition sᵢ = (i, i+1),
the matrix of sᵢ in the irrep indexed by λ has entries determined by the
standard Young tableaux of shape λ.

For each pair of standard tableaux T, T':
- If sᵢ swaps entries in T that are in the same row or same column,
  the diagonal entry is ±1
- If sᵢ maps T to T' (by swapping i and i+1), the off-diagonal entries
  involve 1/ρ where ρ is the axial distance from i to i+1 in T

Specifically, if the axial distance is ρ (positive if i+1 is in a lower
row and to the right), then in the 2×2 block for {T, sᵢ(T)}:

```
         ┌         ┐
         │ 1/ρ   √(1 - 1/ρ²) │
  sᵢ  =  │                    │
         │ √(1 - 1/ρ²)  -1/ρ │
         └         ┘
```

The axial distance of positions (r₁,c₁) and (r₂,c₂) is
(c₂ - c₁) - (r₂ - r₁).

This gives you the matrix for each generator sᵢ. Any permutation σ can be
written as a product of adjacent transpositions, and you multiply the
corresponding matrices.

Reference: James & Kerber, "The Representation Theory of the Symmetric
Group", or Vershik & Okounkov (2005) for a modern treatment.


### Gilbert-Shannon-Reeds Riffle Shuffle (Phase 4)

The GSR model for a single riffle shuffle of an n-card deck:
1. Cut the deck binomially: choose k ~ Binomial(n, 1/2)
2. Interleave the two packets uniformly among all (n choose k) interleavings

The resulting probability of each permutation σ is:

```
Q(σ) = 1/2ⁿ · (number of rising sequences in σ⁻¹ + 1)
      -- actually: --
Q(σ) = (n + 2^a(σ)) / 2ⁿ  ... (simplified form not quite right)
```

More precisely, if d(σ) = number of descents of σ⁻¹ + 1, then:

```
Q(σ) = 1/2ⁿ · C(n+1, 2^{...})  -- see Bayer-Diaconis for exact formula
```

The exact formula is given in Bayer-Diaconis (1992). The key result is that
after k shuffles (= k-fold convolution Q^*k), the total variation distance
to uniform satisfies:

```
‖Q^*k − U‖_TV → { 1 if k ≪ (3/2)log₂(n)
                  { 0 if k ≫ (3/2)log₂(n)
```

with a sharp cutoff. For n=52, this gives k ≈ 7.


---

## Testing Strategy

### Algebraic Invariants as Properties

The character table satisfies strong mathematical identities that make
excellent property-based tests:

1. **Row orthogonality**: Σ_μ |C_μ| · χ_λ(μ) · χ̄_λ'(μ) = |G| · δ_{λλ'}
2. **Column orthogonality**: Σ_λ χ_λ(μ) · χ̄_λ(μ') = (|G|/|C_μ|) · δ_{μμ'}
3. **Dimension formula**: Σ_λ d_λ² = |G|  (sum of squared dimensions of
   irreps equals group order)
4. **Number of irreps = number of conjugacy classes**
5. **Representation matrices are unitary** (for Young's orthogonal form)
6. **ρ(g)ρ(h) = ρ(gh)** — the homomorphism property (spot-check)
7. **tr(ρ(g)) = χ(g)** — character is trace of representation

### Cross-Validation

- Use Wolframite to query `FiniteGroupData[{"SymmetricGroup", n}, "CharacterTable"]`
  and compare with computed tables
- Cross-check with hardcoded known tables for S₃, S₄, S₅
- For Fourier transforms: verify Parseval/Plancherel identity
  Σ_{g∈G} |f(g)|² = (1/|G|) Σ_ρ d_ρ · ‖f̂(ρ)‖²_F


---

## Dependencies

- **dtype-next** — matrices, tensors, linear algebra
- **fastmath** — numerical utilities (may be useful for special functions)
- **tablecloth** — for any tabular presentation of results
- **clay** — notebook rendering for demos
- **wolframite** — development/testing oracle only (optional dev dependency)


---

## License Considerations

Use **EPL-1.0** (standard Scicloj) or consider **MPL-2.0** for broader
compatibility.

All algorithms are implemented from published mathematical papers, not
translated from GPL-licensed code. Key safe references:

- Dixon (1967) — character computation algorithm
- Clausen (1989) — FFT on symmetric group
- James (1978) — representation theory of symmetric groups
- Murnaghan (1937) / Nakayama (1940) — character formula
- Diaconis (1988) — group representations in probability
- Bayer & Diaconis (1992) — riffle shuffle analysis
- Vershik & Okounkov (2005) — Young's orthogonal form, modern treatment
- Sagan, "The Symmetric Group" (textbook) — comprehensive algorithms

Do NOT translate source code from GAP (GPL) or MAGMA (proprietary).
RepLAB (MPL-2.0), Snob2 (MPL-2.0), AbstractAlgebra.jl (BSD),
HaskellForMaths (BSD), and SymPy (BSD) are safe to study for design
inspiration.

See companion document `license-landscape-group-theory-libraries.md` for
full details.
