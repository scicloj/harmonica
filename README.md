# Harmonica

**Computational [group theory](https://en.wikipedia.org/wiki/Group_theory) and [representation theory](https://en.wikipedia.org/wiki/Representation_theory) in [Clojure](https://clojure.org/)**

Harmonica is a library for working with [finite groups](https://en.wikipedia.org/wiki/Finite_group), their [representations](https://en.wikipedia.org/wiki/Group_representation),
and [Fourier analysis](https://en.wikipedia.org/wiki/Fourier_analysis_on_finite_groups). It covers [cyclic](https://en.wikipedia.org/wiki/Cyclic_group), [dihedral](https://en.wikipedia.org/wiki/Dihedral_group), [symmetric](https://en.wikipedia.org/wiki/Symmetric_group), and [product](https://en.wikipedia.org/wiki/Direct_product_of_groups) groups,
with applications to combinatorics, signal processing, and card shuffling.

## General info
|||
|-|-|
|Website | [https://scicloj.github.io/harmonica/](https://scicloj.github.io/harmonica/)
|Source |[![(GitHub repo)](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)](https://github.com/scicloj/harmonica)|
|Deps |[![Clojars Project](https://img.shields.io/clojars/v/org.scicloj/harmonica.svg)](https://clojars.org/org.scicloj/harmonica)|
|License |[MIT](https://github.com/scicloj/harmonica/blob/main/LICENSE)|
|Status |🛠alpha🛠|

## Features

### Groups

- **Cyclic groups** Z/nZ, **dihedral groups** D_n, **symmetric groups** S_n, **product groups** G₁ × G₂
- Uniform protocol — `op`, `inv`, `id`, `elements`, `order`, [`conjugacy-classes`](https://en.wikipedia.org/wiki/Conjugacy_class)

### Permutations and partitions

- [Cycle decomposition](https://en.wikipedia.org/wiki/Cyclic_permutation#Cycle_notation), cycle type, [sign](https://en.wikipedia.org/wiki/Parity_of_a_permutation), composition, inverse
- [Partition](https://en.wikipedia.org/wiki/Partition_(number_theory)) enumeration, [conjugate partitions](https://en.wikipedia.org/wiki/Partition_(number_theory)#Conjugate_and_self-conjugate_partitions)
- [Standard Young tableaux](https://en.wikipedia.org/wiki/Young_tableau), [hook-length formula](https://en.wikipedia.org/wiki/Hook_length_formula)

### [Character tables](https://en.wikipedia.org/wiki/Character_table)

- Cyclic ([DFT matrix](https://en.wikipedia.org/wiki/Discrete_Fourier_transform)), dihedral, symmetric ([Murnaghan-Nakayama rule](https://en.wikipedia.org/wiki/Murnaghan%E2%80%93Nakayama_rule)), product ([Kronecker product](https://en.wikipedia.org/wiki/Kronecker_product))
- Character inner product, [orthogonality relations](https://en.wikipedia.org/wiki/Schur_orthogonality_relations)

### Fourier analysis

- Forward and inverse Fourier transform on any finite [abelian group](https://en.wikipedia.org/wiki/Abelian_group)
- [Convolution](https://en.wikipedia.org/wiki/Convolution) via pointwise multiplication in the Fourier domain
- [Total variation distance](https://en.wikipedia.org/wiki/Total_variation_distance_of_probability_measures) between distributions

### [Representations](https://en.wikipedia.org/wiki/Irreducible_representation)

- Irreducible representations of S_n via [Young's orthogonal form](https://en.wikipedia.org/wiki/Young%27s_orthogonal_form)
- Matrix Fourier transform (matrix-valued coefficients)
- [Tensor product](https://en.wikipedia.org/wiki/Tensor_product_of_representations), [direct sum](https://en.wikipedia.org/wiki/Direct_sum_of_modules), [restriction](https://en.wikipedia.org/wiki/Restricted_representation), [induction](https://en.wikipedia.org/wiki/Induced_representation), [branching rule](https://en.wikipedia.org/wiki/Branching_rule)

### [Group actions](https://en.wikipedia.org/wiki/Group_action)

- [Orbits](https://en.wikipedia.org/wiki/Group_action#Orbits_and_stabilizers), [stabilizers](https://en.wikipedia.org/wiki/Group_action#Orbits_and_stabilizers), fixed points
- [Burnside's lemma](https://en.wikipedia.org/wiki/Burnside%27s_lemma), [cycle index](https://en.wikipedia.org/wiki/Cycle_index), [Pólya enumeration](https://en.wikipedia.org/wiki/P%C3%B3lya_enumeration_theorem)
- Subset actions, coloring actions

### Visualization

- SVG: [Young diagrams](https://en.wikipedia.org/wiki/Young_diagram), hook diagrams, standard Young tableaux, cycle diagrams
- SVG: [Cayley tables](https://en.wikipedia.org/wiki/Cayley_table), [Cayley graphs](https://en.wikipedia.org/wiki/Cayley_graph)

## Installation

Add to your `deps.edn`:

```clojure
{:deps {org.scicloj/harmonica {:mvn/version "0.1.0"}}}
```

## Documentation

The [Harmonica book](https://scicloj.github.io/harmonica/) is organized for
incremental learning, interleaving theory with applications — from the DFT
and rosette patterns through necklace counting, music theory, and card shuffling.

## API

See the [API reference](https://scicloj.github.io/harmonica/harmonica_book.api_reference.html) for the full list of functions with examples.

Most users need only two namespaces:

```clojure
(require '[scicloj.harmonica :as hm])
(require '[scicloj.harmonica.linalg.complex :as cx])
```

`scicloj.harmonica` is the public API — groups, character tables, Fourier
transforms, representations, group actions, and visualization. `cx` provides
ComplexTensor operations for working with complex-valued results.

Internal namespaces (`analysis.characters`, `analysis.fourier`,
`analysis.representations`, `linalg.ejml`, `protocols`, `action`, etc.) are
available but rarely needed directly.

## Built on

- [dtype-next](https://github.com/cnuernber/dtype-next) — array/tensor numerics
- [fastmath](https://github.com/generateme/fastmath) — matrix operations

The [book notebooks](https://scicloj.github.io/harmonica/) also use
[tablecloth](https://github.com/scicloj/tablecloth),
[tableplot](https://github.com/scicloj/tableplot), and
[kindly](https://github.com/scicloj/kindly) (included in the `:dev` and `:test` aliases).

## Development

```bash
clojure -M:dev -m nrepl.cmdline   # start REPL
./run_tests.sh                     # run tests (487 tests, 3498 assertions)
clojure -T:build ci                # test + build JAR
```

## References

- Diaconis, P. (1988). *Group Representations in Probability and Statistics*. IMS Lecture Notes.
- Diaconis, P. & Shahshahani, M. (1981). Generating a random permutation with random transpositions. *Z. Wahrscheinlichkeitstheorie*, 57, 159–179.
- Bayer, D. & Diaconis, P. (1992). Trailing the dovetail shuffle to its lair. *Annals of Applied Probability*, 2(2), 294–313.
- Sagan, B. (2001). *The Symmetric Group: Representations, Combinatorial Algorithms, and Symmetric Functions*. Springer.
- James, G. & Kerber, A. (1981). *The Representation Theory of the Symmetric Group*. Addison-Wesley.

## License

MIT License — see LICENSE file.

---

Part of the [scicloj](https://scicloj.github.io/) ecosystem for scientific computing in Clojure.
