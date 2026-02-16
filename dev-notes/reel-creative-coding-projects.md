# Reel — Creative Coding Projects

Ideas for interactive notebooks and live-coding explorations using the reel
library. Each project demonstrates group theory and representation theory
through a concrete, visual or auditory creative application.

These are intended as Clay notebooks in the `notebooks/` directory,
combining narrative explanation, code, and visual/audio output.

**Prioritization and status**: See `backlog.md` for the unified prioritized
list. Projects 1 and 2 below are complete. The remaining projects are
grouped into tiers based on library requirements.

---
## 1. The DFT You Already Know

**Concept**: Reveal that the Discrete Fourier Transform every programmer has
used is secretly the Fourier transform on the cyclic group ℤ/nℤ. The
"frequencies" are the irreducible representations (characters) of the group.
The DFT matrix *is* the character table.

**What to show**:
- Construct the cyclic group ℤ/nℤ with reel
- Compute its character table — observe it's the familiar DFT matrix
- Take a signal (e.g., a simple chord: sum of sinusoids), treat it as a
  function on ℤ/nℤ
- Apply `reel/fourier-transform` — get the familiar frequency spectrum
- Demonstrate convolution theorem: filtering in frequency domain =
  convolution in time domain, and both are just group algebra operations
- Show the 2D DFT as the product group ℤ/mℤ × ℤ/nℤ acting on a small image

**Punchline**: Every time you've used an FFT, you were doing representation
theory.

**Connections**: Leads naturally into "what happens when the group isn't
abelian?" — setting up the shuffling project.


## 2. Seven Shuffles Suffice

**Concept**: How many riffle shuffles does it take to randomize a deck of
cards? Diaconis proved the answer is 7 for a standard 52-card deck, with a
sharp "cutoff phenomenon" — after 6 shuffles the deck is far from random,
after 7 it's close, after 8 it's nearly perfect.

**What to show**:

*Part A — Random transpositions (simpler warmup)*:
- Model: at each step, pick two cards uniformly at random and swap them
- The distribution after k steps is a class function on S_n (depends only
  on cycle type), so characters suffice
- Plot total variation distance to uniform as a function of k
- Show the cutoff at k ≈ ½ n ln(n) for an n-card deck

*Part B — Riffle shuffles (the famous result)*:
- Gilbert-Shannon-Reeds model: cut binomially, interleave uniformly
- This distribution is NOT a class function — need full matrix
  representations (Young's orthogonal form)
- Compute the matrix-valued Fourier transform for small n (say n = 8 or 10)
- Plot total variation distance, show cutoff at k ≈ (3/2) log₂(n)
- Scale up to n = 52: the "7 shuffles" result

**Visualizations**:
- Heatmap of permutation probabilities after k shuffles
- Total variation distance curve with the sharp cliff
- Fourier coefficient decay by irrep (labeled by partition/Young diagram)
- Side-by-side: "almost sorted" (k=1) → "structured chaos" (k=5) →
  "looks random" (k=8) shown as matrices or card-position plots

**Companion talk**: This notebook supports a separate talk on *visualizing*
card shuffling, providing the probabilistic backbone.


## 3. Symmetry Sketchpad

**Concept**: Draw a freehand motif. Choose a symmetry group. Watch the
group action replicate your drawing into a symmetric pattern. The mess
you drew becomes order — and the *kind* of order depends precisely on
which group you chose.

**What to show**:

*Rosette patterns (finite groups acting on the plane)*:
- Cyclic group Cₙ: rotational symmetry only (like a pinwheel)
- Dihedral group Dₙ: rotational + reflective symmetry (like a snowflake)
- User controls: choose n, toggle reflection, draw a motif in the
  fundamental domain (a wedge of the circle)
- The group action tiles the wedge around the origin

*Frieze patterns (the 7 frieze groups)*:
- A frieze is an infinite strip with translational symmetry plus optional
  rotations, reflections, and glide reflections
- There are exactly 7 types — every decorative border ever made falls
  into one of these 7 categories
- Draw a motif, select a frieze type, generate the infinite strip
- Show real-world examples: Greek key, running dog, braid, zigzag

*Wallpaper patterns (the 17 wallpaper groups)*:
- Same idea, but now tiling the whole plane
- 17 possible symmetry types — all 17 appear in Islamic geometric art,
  all 17 appear in the Alhambra
- Draw a motif in the fundamental domain, pick a wallpaper group,
  fill the screen
- This is the most visually spectacular version

**Implementation notes**:
- The fundamental domain is the region of the plane that, under the group
  action, tiles the whole plane (or strip, or rosette)
- For each point in the output, find which group element maps it back to
  the fundamental domain, look up the color there
- For SVG/canvas output: replicate the motif path by applying each group
  element as an affine transformation
- For rosettes and friezes, the group elements are simple matrices
  (rotation, reflection, translation) — use dtype-next

**Punchline**: There are exactly 7 kinds of border and exactly 17 kinds of
wallpaper. Group theory is why.


## 4. Breaking Symmetry

**Concept**: Perfect symmetry is static. Beauty lives in *almost*-symmetry —
slight departures from perfect regularity. This notebook explores symmetry
breaking as a creative parameter.

**What to show**:
- Start with a pattern with high symmetry (e.g., D₆ — hexagonal, like a
  snowflake)
- Introduce controlled perturbations that break specific symmetries:
  - Break one reflection → D₆ becomes C₆ (rotation only)
  - Break rotational order → C₆ becomes C₃ or C₂
  - Break all symmetry → C₁ (trivial group, no symmetry)
- At each stage, show the pattern AND the group
- Visualize the subgroup lattice: which groups are subgroups of which,
  and what breaks when you descend

**Creative exploration**:
- Parameter slider: "symmetry level" from 0 (fully broken) to 1 (perfect)
- At each level, show which group elements survive
- Generate patterns at each level — observe how slight asymmetry creates
  visual interest, movement, tension
- Show real-world examples: a nearly-symmetric face, a hand-thrown pot
  (almost C∞, actually C₁), a slightly irregular tiling

**Deeper point**: In physics, symmetry breaking explains phase transitions,
the origin of mass, the asymmetry of matter and antimatter. In design, it
explains why hand-made things feel more alive than machine-made things. The
group theory gives you precise language for "how much" and "what kind" of
symmetry is broken.

**Punchline**: The interesting part isn't the symmetry. It's where it breaks.


## 5. Hearing Symmetry — Permutation Music

**Concept**: Apply group elements to a musical motif and listen to the result.
Different representations of the same group produce different musical
transformations.

**What to show**:
- Start with a short melody (4–8 notes)
- The melody is a function from time-positions to pitches
- Apply group elements:
  - **Retrograde** (reverse): play the melody backwards
  - **Inversion**: flip intervals (ascending → descending)
  - **Retrograde inversion**: both
  - These four operations form the Klein four-group V₄ ≅ ℤ/2ℤ × ℤ/2ℤ
- Listen to all four versions, see them on a staff or piano roll

*Extending to the cyclic group*:
- **Transposition**: shift all pitches by k semitones
- The 12 transpositions form ℤ/12ℤ
- Play the motif in all 12 transpositions — this is literally the
  character table of ℤ/12ℤ acting on pitch space

*Extending to the dihedral group*:
- Combine transposition (rotation) with inversion (reflection)
- This gives D₁₂, the full group of pitch-class transformations
- 24 versions of the motif, all related by symmetry

*Twelve-tone music*:
- Schoenberg's method: arrange all 12 pitch classes in a row, then
  compose using only the 48 transformations (12 transpositions ×
  {original, retrograde, inversion, retrograde-inversion})
- This is the orbit of the row under V₄ × ℤ/12ℤ ≅ D₁₂
- Generate all 48 forms of a tone row, play selected ones

**Audio output**: Use Clojure sound synthesis (e.g., Overtone, or simple
WAV generation) to make the transformations audible, not just visible.

**Punchline**: Bach was doing group theory. Schoenberg made it explicit.


## 6. Counting Necklaces — Burnside's Lemma Visualized

**Concept**: How many distinct necklaces can you make with n beads and k
colors, if two necklaces that differ only by rotation are considered the
same? What if reflections (flipping the necklace over) also count as "the
same"?

**What to show**:
- Draw all possible colorings of n beads on a circle
- Group them into equivalence classes under rotation (cyclic group Cₙ)
- Animate: rotate each necklace and highlight which others it matches
- Count the orbits — this is Burnside's lemma in action:
  |orbits| = (1/|G|) Σ_{g∈G} |Fix(g)|
- Show the fixed-point count for each group element: a rotation by k
  positions fixes a coloring iff the coloring has period dividing k
- Extend to the dihedral group (add reflections) — fewer distinct
  necklaces because more things are identified

**Visualizations**:
- Grid of all colorings, grouped by orbit, with orbit representatives
  highlighted
- The cycle index polynomial as a generating function
- Pólya enumeration: how many necklaces with exactly j red beads and
  (n−j) blue beads?
- Scale up: large n where enumeration is impossible but the formula
  still works

**Concrete examples**:
- Binary necklaces (2 colors): how many with n=6? (answer: 13 for Cₙ, 8
  for Dₙ)
- RGB necklaces: the numbers grow fast
- Coloring faces of a cube with k colors: the rotation group of the cube
  is S₄ (order 24), giving surprisingly few distinct colorings

**Punchline**: Symmetry is an equivalence relation, and group theory counts
the equivalence classes.


## 7. Chord Geometry — Music Theory as Group Action

**Concept**: Western music's 12 pitch classes form the group ℤ/12ℤ. Chords
are subsets. Two chords related by transposition (shifting all notes by the
same interval) are the "same type" — C major and D major are both "major."
Chord types are orbits under the group action.

**What to show**:

*Pitch classes on the circle*:
- Draw the 12 notes on a clock face (the "pitch class circle")
- A chord is a polygon inscribed in the circle
- Transposition = rotation of the polygon
- Inversion = reflection

*Classifying chords*:
- All (12 choose 3) = 220 three-note subsets of ℤ/12ℤ
- Under transposition (C₁₂): these fall into 19 orbit types
  (the 19 trichord types, or "set classes")
- Under transposition + inversion (D₁₂): fewer orbit types (12)
  because some trichords are identified with their inversions
- Compute these orbits using reel, display them as polygons on the circle

*Interval vectors as Fourier transforms*:
- The interval vector of a chord counts how many of each interval it
  contains
- This is closely related to the Fourier transform of the chord's
  characteristic function on ℤ/12ℤ
- Two chords with the same interval vector but different Fourier
  transforms: the Z-relation (a genuine mystery in music theory)

*Interactive exploration*:
- Click notes on the circle to build a chord
- See its orbit under C₁₂ and D₁₂
- See its interval vector and Fourier transform
- Hear it (play the chord)
- Identify it by Forte number (the standard catalog of set classes)

**Punchline**: Every chord voicing you've ever heard is a point in an orbit
space. Music theory is the study of equivalence classes under symmetry.


## 8. Chladni Figures — Vibration Modes as Representations

**Concept**: When you vibrate a metal plate at specific frequencies, sand
collects on the nodal lines (places that don't move), forming beautiful
symmetric patterns. These patterns are the irreducible representations of
the plate's symmetry group, made visible.

**What to show**:
- Model: a square or circular plate with a discrete symmetry group
  (D₄ for the square, approximate Cₙ for the circle)
- The vibration modes of the plate decompose into irreducible
  representations of the symmetry group
- Each irrep produces a different nodal pattern
- Simulate and draw the nodal lines for each irrep
- Show how higher representations (higher "frequency") produce more
  complex patterns with more nodal lines

**For a square plate** (D₄ symmetry):
- The vibration modes are eigenfunctions of the Laplacian on the square
- They can be classified by irrep of D₄: some are symmetric under all
  rotations, some change sign under reflection, etc.
- Each irrep's nodal pattern has exactly the symmetry predicted by the
  representation

**For a circular drum** (continuous rotational symmetry, approximate with Cₙ):
- The modes are Bessel functions × trigonometric functions
- The angular part is exactly the characters of the cyclic group
- Recovers the standard "drum modes" visualization

**Implementation**: Solve the discrete Laplacian eigenvalue problem on a
grid using dtype-next. Classify eigenvectors by their transformation
properties under the symmetry group. Plot the nodal lines (zero contours).

**Punchline**: Those sand patterns on a vibrating plate? Each one is an
irreducible representation you can see.


---

## Notebook Conventions

Each notebook should follow this structure:

1. **Hook** — A striking image, question, or demo (minimal code, maximal
   impact)
2. **Exploration** — Interactive code building up the concept step by step
3. **The math** — Brief explanation of the group theory, introduced only
   after the reader has seen it in action
4. **Going further** — Connections to other notebooks, references,
   open questions
5. **Minimal imports** — Each notebook should require only `reel.core` plus
   the relevant group family namespace

Visual output through Clay (SVG, Hiccup). Audio output where relevant.
All notebooks should work standalone — a reader should be able to pick any
one without having read the others.
