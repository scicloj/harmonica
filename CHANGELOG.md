# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] — 2026-03-05

### Changed

- **Migrated to lalinea** for complex arithmetic and linear algebra
- **Simplified analysis layer** (`fourier.clj`, `characters.clj`) using lalinea's field dispatch

### Removed

- `scicloj.harmonica.linalg.complex` — superseded by `scicloj.lalinea.tensor` / `scicloj.lalinea.elementwise`
- `scicloj.harmonica.linalg.ejml` — superseded by `scicloj.lalinea.linalg`

## [0.1.0] — 2026-02-23

Initial release.

### Added

- **Groups**: cyclic (`Z/nZ`), symmetric (`S_n`), dihedral (`D_n`), direct product (`G₁ × G₂`)
- **Group operations**: `op`, `inv`, `id`, `elements`, `order`, `conjugacy-classes`
- **Permutations**: `cycles`, `cycle-type`, `sign`, `adjacent-transposition-decomposition`, `identity-perm`, `transposition`
- **Partitions**: `partitions`, `partition-conjugate`, `standard-young-tableaux`, `hook-length-dimension`
- **Character tables**: Murnaghan-Nakayama rule for `S_n`, exact for cyclic/dihedral/product groups
- **Character display**: `format-cx`, `show-character-table` (Kindly-annotated)
- **Character analysis**: `character-inner-product`
- **Representation matrices**: Young's orthogonal form (`irrep`, `rep-matrix`, `rep-generators`, `rep-character`, `rep-dimension`)
- **Representation operations**: `tensor-product`, `direct-sum`, `restrict-rep`, `induce-rep`, `irrep-multiplicities`, `branching-rule`
- **Matrix Fourier transform**: `matrix-fourier-transform`, `frobenius-norm`, `frobenius-norm-sq`
- **Fourier analysis**: `fourier-transform`, `inverse-fourier-transform`, `convolve`, `total-variation-distance`
- **Group actions**: `orbit`, `orbits`, `fixed-points`, `stabilizer`, `burnside-count`, `cycle-index`, `polya-count`, `subset-action`, `coloring-action`
- **Riffle shuffles**: `gsr-probability`, `gsr-distribution-vec`, `rising-sequences`
- **SVG visualizations**: `young-diagram-svg`, `young-hooks-svg`, `syt-svg`, `cycle-diagram-svg`, `cayley-table-svg`, `cayley-graph-svg`
- **ComplexTensor**: interleaved `double[]` complex arithmetic
- **EJML interop**: zero-copy bridge between ComplexTensor and `ZMatrixRMaj`
