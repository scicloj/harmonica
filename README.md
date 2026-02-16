# Reel

**Computational group theory and representation theory in Clojure**

Reel provides group-theoretic Fourier analysis on finite groups, built on
[dtype-next](https://github.com/cnuernber/dtype-next) for numerics and
[fastmath](https://generateme.github.io/fastmath/clay) for complex arithmetic.

The unifying idea: the Discrete Fourier Transform is secretly the Fourier
transform on the cyclic group. Reel makes this connection explicit and
generalizes it to symmetric groups and beyond.

Current modules:

- **Cyclic groups** Z/nZ — character tables, Fourier transform, convolution theorem
- **Symmetric groups** S_n — permutations, partitions, conjugacy classes

## General info
|||
|-|-|
|Website | [https://scicloj.github.io/reel/](https://scicloj.github.io/reel/)
|Source |[![(GitHub repo)](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/scicloj/reel)|
|Deps |[![Clojars Project](https://img.shields.io/clojars/v/org.scicloj/reel.svg)](https://clojars.org/org.scicloj/reel)|
|License |[MIT](https://github.com/scicloj/reel/blob/main/LICENSE)|
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

## Installation

Add to your `deps.edn`:

```clojure
{:deps {org.scicloj/reel {:mvn/version "0.1.0"}}}
```

## Quick Start

### Cyclic Groups and the DFT

```clojure
(require '[scicloj.reel.core :as reel]
         '[fastmath.complex :as c])

;; Create Z/24Z — the cyclic group of order 24
(def G (reel/cyclic-group 24))

(reel/order G)  ;; => 24
(reel/op G 15 9)  ;; => 0 (addition mod 24)

;; The character table is the DFT matrix
(def ct (reel/character-table G))

;; Fourier transform of a signal (monthly temperatures over 2 years)
(def temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3
   3 4 8 13 18 23 26 25 20 14 8 4])

(def f-hat
  (reel/fourier-transform ct (mapv #(c/complex (double %)) temperatures)))

;; DC component = sum of all values
(c/re (f-hat 0))  ;; => 320.0

;; Dominant frequency k=2 = annual cycle (2 full cycles in 24 months)
```

### Symmetric Groups

```clojure
;; Create S_4 — permutations of {0, 1, 2, 3}
(def S4 (reel/symmetric-group 4))

(reel/order S4)  ;; => 24

;; Permutations are plain vectors
(reel/op S4 [1 0 2 3] [0 1 3 2])  ;; => [1 0 3 2]
(reel/inv S4 [1 2 3 0])  ;; => [3 0 1 2]

;; Cycle notation and cycle type
(reel/cycles [1 2 3 0])  ;; => [[0 1 2 3]]
(reel/cycle-type [1 0 3 2])  ;; => [2 2]
(reel/sign [1 0 2 3])  ;; => -1

;; Partitions of 4
(reel/partitions 4)  ;; => [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]

;; Conjugacy classes indexed by partitions
(map (juxt :cycle-type :size) (reel/conjugacy-classes S4))
;; => ([[4] 6] [[3 1] 8] [[2 2] 3] [[2 1 1] 6] [[1 1 1 1] 1])
```

## Documentation

See the [Reel book](https://scicloj.github.io/reel/) for tutorials:

- **[Quickstart](https://scicloj.github.io/reel/reel_book.quickstart.html)** — minimal introduction
- **[The DFT as Group Fourier Transform](https://scicloj.github.io/reel/reel_book.dft_as_group_fourier.html)** — the full story connecting DFT to group theory
- **[Symmetric Groups](https://scicloj.github.io/reel/reel_book.symmetric_groups.html)** — permutations, partitions, conjugacy classes

## API Namespaces

| Namespace | Purpose |
|:----------|:--------|
| `scicloj.reel.core` | Public API — groups, permutations, partitions, characters, Fourier |
| `scicloj.reel.protocols` | Core protocols: Group, FiniteGroup, GroupStructure, GroupType |
| `scicloj.reel.characters` | Character table computation (multimethod on group type) |
| `scicloj.reel.fourier` | Fourier transform, inverse, convolution, total variation distance |

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
