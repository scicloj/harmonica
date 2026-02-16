# License Landscape: Computational Group Theory & Representation Theory Libraries

## Summary Table

| Library | Language | License | Copyleft? | Can wrap/link? | Can reimplement algorithms? | Notes |
|---|---|---|---|---|---|---|
| **GAP** | GAP lang | GPL-2.0+ | **Yes (strong)** | Only in GPL projects | Yes (algorithms are math) | Packages may vary |
| **GAP Repsn package** | GAP lang | GPL-2.0+ | **Yes (strong)** | Only in GPL projects | Yes | Part of GAP ecosystem |
| **SageMath** | Python | GPL-2.0+ | **Yes (strong)** | Only in GPL projects | Yes | Wraps GAP internally |
| **PySymmetry** | Python/Sage | **No license file found** | **Unknown** | **⚠️ Risky** | Yes (algorithms are math) | Built on SageMath (GPL) |
| **MAGMA** | Proprietary | **Commercial/Closed** | N/A | **No** | Yes (published algorithms) | Cannot access source |
| **Wolfram Language** | Proprietary | **Commercial** | N/A | Via Wolframite only | Yes (published algorithms) | Requires paid license |
| **Wolframite** | Clojure | MPL-2.0 | Weak (file-level) | ✅ Yes | N/A | Bridge only; WL needs license |
| **RepLAB** | MATLAB | MPL-2.0 | Weak (file-level) | ✅ Yes | ✅ Yes | Best architectural inspiration |
| **Snob2** | C++ | MPL-2.0 | Weak (file-level) | ✅ Yes (if JNI) | ✅ Yes | S_n FFT algorithms well-documented |
| **SymPy** | Python | BSD-3-Clause | No | ✅ Yes | ✅ Yes | Permissive; no rep theory yet |
| **AbstractAlgebra.jl** | Julia | BSD-2-Clause | No | ✅ Yes | ✅ Yes | Protocol design inspiration |
| **Nemo.jl** | Julia | BSD (Julia code) | No (but links GPL C libs) | ✅ Julia code; ⚠️ C deps are GPL | ✅ Yes | Wraps FLINT/Arb (LGPL/GPL) |
| **NumericalRepresentationTheory.jl** | Julia | MIT (presumed) | No | ✅ Yes | ✅ Yes | No explicit LICENSE file found |
| **FiniteGroups.jl** | Julia | **No license file found** | **Unknown** | **⚠️ Risky** | Yes | Small; no explicit license |
| **HaskellForMaths** | Haskell | BSD-3-Clause | No | ✅ Yes | ✅ Yes | Educational; great design reference |
| **group-theory** (Haskell) | Haskell | BSD-3-Clause | No | ✅ Yes | ✅ Yes | Typeclass hierarchy inspiration |
| **noetherpy** | Python | **No license file found** | **Unknown** | **⚠️ Risky** | Yes | Educational; small |
| **symchar** | Python | **No license file found** | **Unknown** | **⚠️ Risky** | Yes | Toy project, minimal |
| **GTPack** | Mathematica | **Unknown** | Unknown | Requires Mathematica | Yes | Third-party Mathematica package |

## License Categories Explained

### 🟢 Safe to draw inspiration from (permissive licenses)

**BSD / MIT licensed** — You can freely study the source code, reimplement algorithms in Clojure, and even wrap these libraries. The only obligation is including the copyright notice if you redistribute their code.

- **SymPy** (BSD-3): Solid permutation group infrastructure. Safe to study Todd-Coxeter, Schreier-Sims implementations.
- **AbstractAlgebra.jl** (BSD-2): Protocol/type hierarchy design is directly transferable to Clojure protocols. Safe to study extensively.
- **HaskellForMaths** (BSD-3): Module structure and typeclass design maps cleanly to Clojure. Educational code, safe to study.
- **group-theory Haskell** (BSD-3): Constructive group theory typeclasses. Safe reference for protocol design.

### 🟡 Safe to draw inspiration from, with care (weak copyleft)

**MPL-2.0 licensed** — Mozilla Public License is "file-level copyleft": if you modify *their files*, the modified files must stay MPL. But you can freely combine MPL code with code under other licenses in a larger project. Reimplementing algorithms in new files is completely fine.

- **RepLAB** (MPL-2.0): Your best architectural reference. You can study the category-based design, random sampling from commutant algebra, numerical decomposition approach — and reimplement all of it in Clojure with no licensing concern.
- **Snob2** (MPL-2.0): Clausen's FFT, Young diagram handling, branching rule implementation. All safe to reimplement from the documented algorithms.
- **Wolframite** (MPL-2.0): Safe to use as a dependency in your Clojure library (MPL is compatible with EPL). But remember the Wolfram kernel itself requires a commercial license.

### 🔴 Caution required (strong copyleft)

**GPL-2.0+ licensed** — If you *link* to or *wrap* GPL code, your entire project must be GPL. However, *reading* GPL source code to understand algorithms and *reimplementing from mathematical descriptions* is legal — algorithms and mathematical ideas are not copyrightable. The key is to reimplement from the mathematical specification, not to translate GPL code line-by-line.

- **GAP** (GPL-2.0+): The gold standard for algorithms. You can study GAP's documentation and the mathematical papers it references (Dixon-Schneider, Burnside-Dixon, etc.) to understand how algorithms work, then implement them independently. Do NOT copy-paste or directly translate GAP source code.
- **SageMath** (GPL-2.0+): Same principle. Study the mathematical approach, reimplement independently.
- **GAP packages** (varies): Each GAP package has its own license. The Repsn package is GPL. The CTblLib (character table library) contains *data* — factual mathematical data (character tables) cannot be copyrighted, but the specific selection/arrangement/formatting could be.

### ⚫ Cannot use (proprietary)

- **MAGMA**: Closed source, commercial. Cannot see code. Published algorithms from MAGMA papers are fair game (they're math), but the software itself is off-limits.
- **Wolfram Language**: Commercial. Can access via Wolframite (which is MPL), but the WL kernel needs a license. Fine for development/testing, not as a runtime dependency for an open-source library.

### ⚪ No license = all rights reserved

Several smaller libraries have no explicit license file:
- **PySymmetry**, **noetherpy**, **symchar**, **FiniteGroups.jl**

Technically, no license means the author retains all rights, and you cannot legally copy, modify, or distribute the code. However, you can still:
- Read the code for educational understanding
- Reimplement algorithms from the underlying mathematical papers they cite
- Use them as test oracles (run their code to verify your outputs)

If you find a library particularly valuable, consider contacting the author to request they add a license.

## Practical Recommendations for Your Clojure Library

### Algorithms are math — always safe to reimplement

The core algorithms in this field (Dixon-Schneider, Clausen FFT, Burnside method, Murnaghan-Nakayama rule, Young's orthogonal form, etc.) are published mathematical results. They cannot be copyrighted. You can implement any of them regardless of the license of any existing implementation, as long as you work from the mathematical description rather than translating someone else's code.

Key papers to implement from directly:
- Dixon (1967): "High speed computation of group characters" — the Dixon-Schneider algorithm
- Clausen (1989): "Fast generalized Fourier transforms" — the S_n FFT
- James (1978): "The Representation Theory of Symmetric Groups" — character computation via partitions
- Kondor thesis (2008): Comprehensive description of Clausen's FFT with implementation details

### Recommended license for your library

Given the Scicloj ecosystem uses EPL-1.0 predominantly, and you want maximum compatibility:

- **EPL-1.0** is compatible with: MIT, BSD, Apache-2.0, MPL-2.0
- **EPL-1.0** is NOT compatible with: GPL-2.0 (this is a known friction point)
- This means: do NOT wrap or link GPL libraries (GAP, SageMath). Reimplement algorithms independently.

### Safe inspiration sources ranked by value

1. **RepLAB** (MPL-2.0) — Best architecture reference. Category-based design, numerical methods.
2. **AbstractAlgebra.jl** (BSD) — Best protocol hierarchy reference.
3. **HaskellForMaths** (BSD) — Best educational/pedagogical reference.
4. **Snob2** (MPL-2.0) — Best reference for S_n FFT implementation.
5. **SymPy** (BSD) — Best reference for permutation group infrastructure.
6. **GAP documentation + cited papers** — Best reference for algorithms (implement from math, not from code).
7. **Wolfram via Wolframite** (MPL-2.0 + commercial) — Best test oracle during development.

### What to avoid

- Do not create a GAP wrapper (would force GPL on your library)
- Do not translate GAP source code to Clojure (derivative work under GPL)
- Do not depend on Wolfram Language at runtime (commercial license required for users)
- Do not copy code from unlicensed repositories without author permission
