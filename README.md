# Harmonica

**Computational group theory and representation theory in Clojure**

Harmonica provides group-theoretic Fourier analysis on finite groups, built on
[dtype-next](https://github.com/cnuernber/dtype-next) for numerics and
[fastmath](https://generateme.github.io/fastmath/clay) for complex arithmetic.

The unifying idea: the Discrete Fourier Transform is secretly the Fourier
transform on the cyclic group. Harmonica makes this connection explicit and
generalizes it to symmetric groups and beyond.

Current modules:

- **Cyclic groups** Z/nZ — character tables, Fourier transform, convolution theorem
- **Symmetric groups** S_n — permutations, partitions, conjugacy classes, character tables
- **Fourier analysis** — on both abelian and non-abelian groups

## General info
|||
|-|-|
|Website | [https://scicloj.github.io/harmonica/](https://scicloj.github.io/harmonica/)
|Source |[![(GitHub repo)](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/scicloj/harmonica)|
|Deps |[![Clojars Project](https://img.shields.io/clojars/v/org.scicloj/harmonica.svg)](https://clojars.org/org.scicloj/harmonica)|
|License |[MIT](https://github.com/scicloj/harmonica/blob/main/LICENSE)|
|Status |🛠alpha🛠|

## Features

### Cyclic Groups

- **Group protocol** — `op`, `inv`, `id`, `elements`, `order`, `conjugacy-classes`
- **Character tables** — the DFT matrix as a character table of Z/nZ
- **Fourier transform** — vectorized via [dtype-next](https://github.com/cnuernber/dtype-next) on split real/imaginary arrays
- **Inverse transform** — perfect reconstruction
- **Convolution** — via pointwise multiplication in the Fourier domain
- **Total variation distance** — for comparing probability distributions

### Symmetric Groups

- **Permutations** — 0-indexed one-line notation vectors, composition, inverse
- **Cycle notation** — decomposition, cycle type, sign (parity)
- **Partitions** — enumeration, conjugate, validation
- **Conjugacy classes** — indexed by partitions (cycle types), class sizes via formula
- **Character tables** — computed via the Murnaghan-Nakayama rule
- **Irrep dimensions** — hook-length formula

## Installation

Add to your `deps.edn`:

```clojure
{:deps {org.scicloj/harmonica {:mvn/version "0.1.0"}}}
```

## Documentation

See the [Harmonica book](https://scicloj.github.io/harmonica/) for tutorials:

- **[Quickstart](https://scicloj.github.io/harmonica/harmonica_book.quickstart.html)** — minimal introduction
- **[The DFT as Group Fourier Transform](https://scicloj.github.io/harmonica/harmonica_book.dft_as_group_fourier.html)** — the full story connecting DFT to group theory
- **[Symmetric Groups](https://scicloj.github.io/harmonica/harmonica_book.symmetric_groups.html)** — permutations, partitions, conjugacy classes
- **[Random Transpositions](https://scicloj.github.io/harmonica/harmonica_book.random_transpositions.html)** — Diaconis-Shahshahani cutoff phenomenon

## API Namespaces

| Namespace | Purpose |
|:----------|:--------|
| `scicloj.harmonica` | Public API — groups, permutations, partitions, characters, Fourier |
| `scicloj.harmonica.protocols` | Core protocols: Group, FiniteGroup, GroupStructure, GroupType |
| `scicloj.harmonica.characters` | Character table computation (multimethod on group type) |
| `scicloj.harmonica.fourier` | Fourier transform, inverse, convolution, total variation distance |

## Development

```bash
clojure -M:dev -m nrepl.cmdline   # start REPL
./run_tests.sh                     # run tests
clojure -T:build ci                # test + build JAR
```

## References

- Diaconis, P. (1988). *Group Representations in Probability and Statistics*. IMS Lecture Notes.
- Diaconis, P. & Shahshahani, M. (1981). Generating a random permutation with random transpositions. *Z. Wahrscheinlichkeitstheorie*, 57, 159–179.
- Sagan, B. (2001). *The Symmetric Group: Representations, Combinatorial Algorithms, and Symmetric Functions*. Springer.
- James, G. & Kerber, A. (1981). *The Representation Theory of the Symmetric Group*. Addison-Wesley.

## License

MIT License — see LICENSE file.

---

Part of the [scicloj](https://scicloj.github.io/) ecosystem for scientific computing in Clojure.
