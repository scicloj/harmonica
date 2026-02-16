# Backlog

Tracking ongoing and future work items for the reel project.

## Code quality

- [x] Review source code to use dtype-next and fastmath where appropriate
  - `fourier.clj`: Extracted `split-vec2`, `join-vec2`, `forward-split`, `inverse-split` helpers. `convolve` now avoids Vec2 intermediates entirely.
  - `characters.clj`: `character-inner-product` rewritten to use dfn operations on split double arrays instead of reduce + c/mult + c/add.
  - Structural code (partitions, permutations, MN rule) correctly uses Clojure collections / mutable arrays — no changes needed.

## Phase 4 deferred items

- [ ] `tensor-product` of representations
- [ ] `direct-sum` of representations

## Notebooks — Prioritized

Unified from `group-rep-library-plan.md` (Motivating Applications) and
`reel-creative-coding-projects.md`. See those files for detailed descriptions.

### Done

- [x] **The DFT You Already Know** — DFT as Fourier on ℤ/nℤ (notebook: `dft_as_group_fourier`)
- [x] **Random Transpositions** — class-function Fourier analysis on S_n (notebook: `random_transpositions`)
- [x] **Seven Shuffles Suffice** — riffle shuffles, matrix Fourier transform, cutoff (notebook: `riffle_shuffle`)
- [x] **Counting Necklaces** — Burnside's lemma, cycle index, Pólya enumeration (notebook: `counting_necklaces`)
- [x] **Chord Geometry** — pitch classes as ℤ/12ℤ, trichord classification, D₁₂ (notebook: `chord_geometry`)

### Tier 2 — Phase 5 extensions (creative applications)

Build on Phase 5 with additional creative dimensions.

- [ ] **Hearing Symmetry** — Permutation music. Apply group elements to a
  melody: retrograde, inversion, retrograde-inversion (Klein four-group V₄).
  Transposition as ℤ/12ℤ action. Twelve-tone rows and their 48 forms under
  V₄ × ℤ/12ℤ. Audio output.

- [ ] **Symmetry Sketchpad** — Draw a motif, choose a symmetry group, watch
  the group action replicate it. Rosette patterns (Cₙ, Dₙ), frieze patterns
  (7 frieze groups), wallpaper patterns (17 wallpaper groups). Rosettes and
  friezes are doable with Phase 5 library; wallpaper groups are a larger effort.

### Tier 3 — Beyond Phase 5 (new library infrastructure needed)

- [ ] **Breaking Symmetry** — Start with high symmetry (D₆), progressively
  break it. Subgroup lattice visualization. Needs subgroup enumeration and
  lattice computation.

- [ ] **Chladni Figures** — Vibration modes of symmetric plates as irreducible
  representations. Needs eigenvalue problems on discrete Laplacian, continuous
  symmetry approximation. More numerical than algebraic.

- [ ] **Symmetry-Reduced Optimization** — Block-diagonalization via irreducible
  representations. Isotypic decomposition, change of basis. Connects to RepLAB
  domain. Needs advanced representation theory infrastructure.

## Notebooks — Style

- [ ] Make notebook prose more conceptual and didactic
  - Focus on the "why" and intuition, not just the "what"
  - Add more connecting narrative between code blocks
  - Ensure each notebook tells a self-contained story

## Complex matrix backend (future)

See `dev-notes/ejml-complex-matrices.md` for detailed research notes on EJML `ZMatrixRMaj`.

Two candidate approaches:
1. **EJML `ZMatrixRMaj`** — built-in complex multiply/add/trace/conjugate-transpose/Frobenius norm; no eigenvalue/SVD; new dependency
2. **SoA `{:re RealMatrix, :im RealMatrix}`** — reuses fastmath; manual 4-product multiply; Kronecker via `fm/kronecker`

Decision deferred until we need genuinely complex irreps (beyond S_n).
