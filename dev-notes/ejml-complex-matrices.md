# EJML for Complex Matrices — Research Notes

## Overview

EJML (Efficient Java Matrix Library) provides `ZMatrixRMaj` for complex double-precision
dense matrices. Explored as a potential backend for complex matrix operations in reel.

**License**: Apache 2.0 — fully compatible with EPL-1.0.
**Maven**: `org.ejml/ejml-zdense {:mvn/version "0.44.0"}` (or `ejml-all` for everything).
**Requires**: Java 11+ (bytecode), Java 17+ to build from source.

## Key API: `CommonOps_ZDRM`

| Operation | Method | Notes |
|:----------|:-------|:------|
| Create | `new ZMatrixRMaj(rows, cols)` | |
| Identity | `CommonOps_ZDRM.identity(n)` | |
| Add/Subtract | `.add(a, b, c)` / `.subtract(a, b, c)` | c = a ± b |
| Multiply | `.mult(a, b, c)` | c = a·b |
| Multiply+Add | `.multAdd(a, b, c)` | c += a·b (ideal for Fourier transform accumulation) |
| Scale | `.scale(re, im, a)` | In-place α·A |
| Transpose | `.transpose(a, out)` | |
| Conjugate transpose | `.transposeConjugate(a, out)` | A^H |
| Trace | `.trace(a, out)` | Returns `Complex_F64` |
| Determinant | `.det(a)` | Returns `Complex_F64` |
| Inverse | `.invert(a, out)` | |
| Solve | `.solve(a, b, x)` | A·x = b |
| Element-wise mul | `.elementMultiply(a, b, out)` | |
| Frobenius norm | `NormOps_ZDRM.normF(a)` | Single method in NormOps_ZDRM |
| Extract real/imag | `.real(a, out)` / `.imaginary(a, out)` | → `DMatrixRMaj` |
| Convert real→complex | `.convert(realMat, out)` | |
| Conjugate | `.conjugate(input, output)` | Negate imaginary parts |
| Element access | `.getReal(r,c)` / `.getImag(r,c)` / `.set(r,c,re,im)` | |

Also available: `multTransA` (c = A^H · B), `multTransB`, `multTransAB` and all
`multAdd` variants.

## Storage Format

Interleaved `double[]`: `[re00, im00, re01, im01, ...]`. Total array size = rows × cols × 2.
Cache-friendly for matrix multiply but means dfn ops can't be applied directly to
real/imaginary parts without extraction.

## Decompositions Available (via `DecompositionFactory_ZDRM`)

| Decomposition | Available | Factory method |
|:--------------|:----------|:---------------|
| LU | Yes | `DecompositionFactory_ZDRM.lu(rows, cols)` |
| QR | Yes | `DecompositionFactory_ZDRM.qr(rows, cols)` |
| Cholesky | Yes | `DecompositionFactory_ZDRM.chol(size, lower)` |
| SVD | **No** | Not in factory |
| Eigenvalue | **No** | Open issue #93 since 2020, still unresolved |

## Limitations

- **No eigenvalue decomposition** for complex matrices (GitHub issue #93, open since 2020)
- **No SVD** for complex matrices
- **No Kronecker product** built-in (would need hand-rolled implementation)
- Adds a dependency beyond what fastmath already provides

## Comparison with SoA Approach

The alternative (already noted in backlog.md) is `{:re RealMatrix, :im RealMatrix}` using
fastmath's existing Apache Commons Math backend:

| Aspect | EJML ZMatrixRMaj | SoA {:re RealMatrix, :im RealMatrix} |
|:-------|:-----------------|:-------------------------------------|
| New dependency | Yes (ejml-zdense) | No (reuses fastmath) |
| Complex multiply | Built-in `.mult` | Manual 4-product: `(A_r B_r - A_i B_i)` + `(A_r B_i + A_i B_r)` |
| Kronecker product | Not built-in | Via `fm/kronecker` on each part |
| Trace | `.trace` → Complex_F64 | `(+ (fm/trace re) (* i (fm/trace im)))` |
| Conj transpose | `.transposeConjugate` | Transpose both, negate imaginary |
| Frobenius norm | `NormOps_ZDRM.normF` | Manual from parts |
| multAdd (c += a·b) | Built-in | Manual |
| dfn interop | Need extract first | Real parts are already double[][] |

## When to Use

EJML ZMatrixRMaj would be valuable when:
- Moving beyond S_n to groups with genuinely complex irreps (e.g., cyclic group
  matrix representations, or general finite group irreps)
- The `multAdd` pattern (c += α·ρ(σ)) becomes a hot path for complex matrix
  Fourier transforms
- Missing eigenvalue/SVD is not a blocker for Fourier analysis on finite groups
  (irreps are computed algebraically, not via eigenvalue decomposition)

## References

- [EJML GitHub](https://github.com/lessthanoptimal/ejml)
- [EJML Complex Tutorial](https://ejml.org/wiki/index.php?title=Tutorial_Complex)
- [ZMatrixRMaj Javadoc](https://ejml.org/javadoc/org/ejml/data/ZMatrixRMaj.html)
- [CommonOps_ZDRM Javadoc](https://ejml.org/javadoc/org/ejml/dense/row/CommonOps_ZDRM.html)
- [DecompositionFactory_ZDRM Javadoc](https://ejml.org/javadoc/org/ejml/dense/row/factory/DecompositionFactory_ZDRM.html)
- [Issue #93 — Complex eigenvalue decomposition](https://github.com/lessthanoptimal/ejml/issues/93)
