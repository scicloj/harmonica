;; # The DFT as Fourier Transform on a Group
;;
;; The [Discrete Fourier Transform](https://en.wikipedia.org/wiki/Discrete_Fourier_transform) (DFT) is one of the most important algorithms
;; in computing. It decomposes a signal into "frequencies" — but what are these
;; frequencies, really?
;;
;; The answer comes from group theory: the DFT is the **Fourier transform on
;; the [cyclic group](https://en.wikipedia.org/wiki/Cyclic_group)** $\mathbb{Z}/n\mathbb{Z}$. The "frequencies" are the **[irreducible
;; representations](https://en.wikipedia.org/wiki/Irreducible_representation)** (characters) of this group. The DFT matrix **is** the
;; [character table](https://en.wikipedia.org/wiki/Character_table).
;;
;; This notebook makes the connection explicit, step by step.

(ns harmonica-book.dft-as-group-fourier
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
   [harmonica-book.book-helpers :refer [allclose?]]
   [fastmath.transform :as t]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [tech.v3.datatype.convolve :as dt-conv]
   [tech.v3.tensor :as tensor]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [scicloj.kindly.v4.kind :as kind]))

;; ## A signal is a function on a group

;; Suppose we record monthly average temperatures (°C) over two years:

(def temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3
   3 4 8 13 18 23 26 25 20 14 8 4])

(-> (tc/dataset {:month (range 24) :temp temperatures})
    (plotly/base {:=x :month :=y :temp
                  :=title "Monthly temperatures — two years of data"
                  :=x-title "month" :=y-title "°C"})
    (plotly/layer-line)
    (plotly/layer-point {:=mark-size 5})
    plotly/plot)

;; These 24 numbers implicitly define a periodic pattern — after month 23,
;; the cycle repeats. The clear seasonal pattern (cold winters, warm summers)
;; repeats twice.
;;
;; Mathematically, the indices $0, \ldots, 23$ form the **cyclic group
;; $\mathbb{Z}/24\mathbb{Z}$**: integers with addition mod 24. A signal of length 24
;; is a function on this group. The "mod" in modular arithmetic captures
;; the periodicity: position 24 wraps around to position 0, just as
;; January follows December.

(def G (hm/cyclic-group 24))

(hm/elements G)

(kind/test-last
 [= (range 24)])

;; The group operation is addition mod 24.

(hm/op G 15 9)

(kind/test-last
 [= 0])

(hm/op G 18 10)

(kind/test-last
 [= 4])

;; Every element has an inverse.

(hm/inv G 15)

(kind/test-last
 [= 9])

;; ## Characters: rotations at different speeds

;; A **character** of $\mathbb{Z}/n\mathbb{Z}$ is a [homomorphism](https://en.wikipedia.org/wiki/Group_homomorphism) from the group to the complex
;; numbers of magnitude 1 — that is, a mapping that preserves the group
;; operation and lands on the unit circle.
;;
;; For $\mathbb{Z}/n\mathbb{Z}$, the characters are:
;;
;; $$\chi_k(g) = \omega^{kg}$$
;;
;; where $\omega = e^{2\pi i/n}$ is the primitive $n$-th [root of unity](https://en.wikipedia.org/wiki/Root_of_unity) and
;; $k = 0, 1, \ldots, n-1$.
;;
;; Each character $\chi_k$ is a "rotation at speed $k$" — it goes around the
;; unit circle $k$ times as $g$ goes from 0 to $n-1$. This is exactly the
;; "rotations at different speeds" intuition from signal processing.

;; ## The character table is the DFT matrix

;; The **character table** collects all characters into a matrix. Row $k$
;; gives the values of character $\chi_k$ at each group element.

;; Why are characters the "right" basis? Consider the **shift operator**
;; $T_1 f(g) = f(g - 1)$, which translates a signal by one step. A character
;; $\chi_k$ is an **[eigenvector](https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors)** of every shift: $T_a \chi_k(g) = \chi_k(g - a)
;; = \omega^{-ka} \chi_k(g)$. The eigenvalue $\omega^{-ka}$ depends on the
;; shift amount but not on $g$ — this is what makes characters "pure frequencies."
;; Decomposing into characters diagonalizes all shifts simultaneously.

(def ct (hm/character-table G))

ct

;; All entries have magnitude 1 (they lie on the unit circle).

(allclose? (cx/cabs (:table ct)) 1.0)

(kind/test-last
 [true?])

;; The first row ($k = 0$) is the **trivial character** — all ones.

(allclose? (cx/re ((:table ct) 0)) 1.0)

(kind/test-last
 [true?])

;; Let's visualize a few characters as rotations. Character $\chi_k$
;; completes $k$ full rotations over the 24 group elements.

(-> (tc/dataset
     (let [table (:table ct)]
       (for [k [0 1 2 3]
             g (range 24)]
         {:month g
          :real-part (cx/re ((table k) g))
          :character (str "chi_" k)})))
    (plotly/base {:=x :month
                  :=y :real-part
                  :=color :character
                  :=x-title "Group element g (month)"
                  :=y-title "Re(chi_k(g))"
                  :=title "Characters of Z/24Z — real parts (cosine components)"})
    (plotly/layer-line)
    (plotly/layer-point {:=mark-size 6})
    plotly/plot)

;; Each character oscillates at a different rate — exactly the cosine waves
;; at different frequencies that the DFT decomposes a signal into.
;; Character $\chi_2$ completes two full cycles in 24 months — the annual
;; frequency.

;; ## Fourier transform on the group

;; The **Fourier transform** of a function $f$ on a finite group is:
;;
;; $$\hat{f}(k) = \sum_{g \in G} f(g) \cdot \overline{\chi_k(g)}$$
;;
;; This is an inner product: "how much does $f$ align with character $\chi_k$?"
;; For $\mathbb{Z}/n\mathbb{Z}$, this is exactly the DFT formula.

(def signal (cx/complex-tensor-real temperatures))

signal

(def f-hat (hm/fourier-transform ct signal))

f-hat

;; The $k = 0$ coefficient is the sum of all values (the DC component).

(cx/re (f-hat 0))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 320.0)) 1e-10))])

;; Let's see the magnitude spectrum — how strong each "frequency" is.

(-> (tc/dataset
     {:frequency (range 24)
      :magnitude (vec (cx/cabs f-hat))})
    (plotly/base {:=x :frequency
                  :=y :magnitude
                  :=x-title "Frequency k (character index)"
                  :=y-title "|f-hat(k)|"
                  :=title "Fourier spectrum of monthly temperatures on Z/24Z"})
    (plotly/layer-line)
    (plotly/layer-point {:=mark-size 6})
    plotly/plot)

;; The dominant oscillating component is $k = 2$ — two complete cycles
;; over the 24-month window, i.e. the **annual cycle** (period = 12 months).
;;
;; You'll also notice a matching peak at $k = 22$. This is not a separate
;; physical phenomenon — for real-valued signals, the DFT spectrum is always
;; symmetric: the component at $k$ and the component at $n - k$ are complex
;; conjugates of each other ($n = 24$ here, so $k = 2$ and $k = 22 = 24 - 2$
;; carry the same information).
;;
;; Beyond the annual cycle, a few smaller components are visible above the
;; noise floor: $k = 4$ and $k = 20$ (the semi-annual harmonic, period = 6
;; months) and $k = 1$ and $k = 23$ (a slow drift over the 2-year window).
;; Each pair is again a component and its conjugate mirror.

;; ## Comparison with the standard FFT

;; Let's verify that our group-theoretic Fourier transform gives the same
;; result as the standard FFT from [fastmath](https://github.com/generateme/fastmath).
;;
;; The fastmath FFT returns interleaved `[re_0, im_0, re_1, im_1, ...]` for
;; the first $N/2$ coefficients (exploiting Hermitian symmetry). We extract
;; them and compare magnitudes with our full result.

(let [fft-result (t/forward-1d (t/transformer :real :fft) temperatures)
      fft-coefficients (let [data (vec fft-result)
                             n (/ (count data) 2)]
                         (cx/complex-tensor
                          (mapv (fn [k] (data (* 2 k))) (range n))
                          (mapv (fn [k] (data (inc (* 2 k)))) (range n))))]
  (allclose? (dtype/sub-buffer (cx/cabs f-hat) 0 12)
             (cx/cabs fft-coefficients)
             1e-8))
(kind/test-last
 [true?])

;; The magnitudes match. The group-theoretic Fourier transform and the
;; standard DFT compute exactly the same thing — because they **are** the
;; same thing.

;; ## Character orthogonality

;; The characters of a finite group satisfy beautiful [orthogonality relations](https://en.wikipedia.org/wiki/Schur_orthogonality_relations).
;; These are not just mathematical curiosities — they are what make the
;; Fourier transform invertible.
;;
;; **Row orthogonality**: different characters are orthogonal.
;;
;; $$\frac{1}{|G|} \sum_{g \in G} \chi_j(g) \overline{\chi_k(g)} = \delta_{jk}$$

(def orthogonality-matrix
  (let [table (:table ct)
        sizes (:class-sizes ct)
        n 24]
    (tensor/compute-tensor
     [n n]
     (fn [j k]
       (cx/cabs (hm/character-inner-product (table j) (table k) sizes n)))
     :float64)))

orthogonality-matrix

;; The matrix is the 24x24 identity (to numerical precision) — 1 on the
;; diagonal, effectively 0 everywhere else.

(allclose? orthogonality-matrix
           (tensor/compute-tensor [24 24] (fn [j k] (if (= j k) 1.0 0.0)) :float64))

(kind/test-last
 [true?])

;; ## Perfect reconstruction

;; The inverse Fourier transform recovers the original signal exactly.
;;
;; $$f(g) = \frac{1}{|G|} \sum_{k} \hat{f}(k) \cdot \chi_k(g)$$

(let [reconstructed (hm/inverse-fourier-transform ct f-hat)]
  (dfn/reduce-max (cx/cabs (cx/csub reconstructed signal))))
(kind/test-last
 [(fn [err] (< err 1e-10))])

;; ## The convolution theorem

;; On any finite group, convolution in the group domain corresponds to
;; pointwise multiplication in the Fourier domain.
;;
;; $$(f * h)(g) = \sum_{x \in G} f(x) \cdot h(x^{-1}g)$$
;;
;; In the Fourier domain:
;;
;; $$\widehat{f * h}(k) = \hat{f}(k) \cdot \hat{h}(k)$$
;;
;; For $\mathbb{Z}/n\mathbb{Z}$, this is the familiar **[cyclic convolution theorem](https://en.wikipedia.org/wiki/Convolution_theorem)**.
;; It is the reason fast convolution is possible: instead of $O(n^2)$
;; direct summation, we can compute forward FFT, pointwise multiply, and
;; inverse FFT in $O(n \log n)$.

(def f-fn (cx/complex-tensor-real
           [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))
(def h-fn (cx/complex-tensor-real
           [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))

;; Convolve via the library (which uses the Fourier domain internally).

(def convolved (hm/convolve ct f-fn h-fn))

(mapv #(Math/round %) (vec (cx/re convolved)))

(kind/test-last
 [= [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

;; Verify: the Fourier transform of the convolution equals the pointwise
;; product of the individual transforms.
(let [f-fn-hat (hm/fourier-transform ct f-fn)
      h-fn-hat (hm/fourier-transform ct h-fn)
      convolved-hat (hm/fourier-transform ct convolved)
      pointwise-product (cx/cmul f-fn-hat h-fn-hat)]
  (< (dfn/reduce-max (cx/cabs (cx/csub convolved-hat pointwise-product))) 1e-8))

(kind/test-last
 [true?])

;; ## [Parseval's theorem](https://en.wikipedia.org/wiki/Parseval%27s_theorem) (energy conservation)

;; The total "energy" of a signal is preserved under the Fourier transform.
;; This guarantees that the transform is an isometry — no information is
;; lost or amplified when changing between the time and frequency domains.
;;
;; $$\sum_{g} |f(g)|^2 = \frac{1}{|G|} \sum_{k} |\hat{f}(k)|^2$$

(let [mag-s (cx/cabs signal)
      mag-f (cx/cabs f-hat)
      energy-time (dfn/sum (dfn/* mag-s mag-s))
      energy-freq (/ (dfn/sum (dfn/* mag-f mag-f))
                     (double (hm/order G)))]
  (< (Math/abs (- energy-time energy-freq)) 1e-8))
(kind/test-last
 [true?])

;; ## Connection to dtype-next convolution
;;
;; The [dtype-next](https://github.com/cnuernber/dtype-next) library provides `convolve1d` for efficient real-valued
;; linear convolution. For signals on cyclic groups, **cyclic** convolution
;; can be obtained from a full linear convolution by folding the overflow
;; back around.

(def f-real
  [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3])
(def h-real
  [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])

;; Full linear convolution has length $2n - 1$.

(def linear-conv
  (vec (dt-conv/convolve1d f-real h-real {:mode :full :edge-mode :zero})))

(count linear-conv)

(kind/test-last
 [= 47])

;; To get cyclic convolution, fold the tail back onto the first $n$ elements.

(def cyclic-from-linear
  (let [n 24]
    (mapv (fn [i]
            (+ (linear-conv i)
               (if (< (+ i n) (count linear-conv))
                 (linear-conv (+ i n))
                 0.0)))
          (range n))))

cyclic-from-linear

(kind/test-last
 [(fn [v] (= (mapv long v) [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))])

;; This matches our group-theoretic convolution exactly.

(let [group-conv (cx/re (hm/convolve ct
                                     (cx/complex-tensor-real f-real)
                                     (cx/complex-tensor-real h-real)))]
  (allclose? cyclic-from-linear group-conv))

(kind/test-last
 [true?])

;; The connection: harmonica's `convolve` computes cyclic convolution via the
;; group Fourier transform (pointwise multiply in the frequency domain),
;; which is equivalent to folding a linear convolution. For real-valued
;; signals, dtype-next's `convolve1d` provides the fast underlying
;; linear convolution; harmonica adds the group-theoretic structure.

;; ## What comes next
;;
;; The cyclic group is the simplest case. The same framework — groups,
;; characters, Fourier transform — extends to every finite group:
;;
;; - **Product groups** $G_1 \times G_2$ — componentwise operations, giving
;;   the 2D DFT as a special case. See the
;;   [next chapter](product_group_dft.html).
;;
;; - **Dihedral groups** $D_n$ — symmetries of regular polygons, used for
;;   [rosette patterns](symmetry_sketchpad.html),
;;   [Burnside counting](counting_necklaces.html), and
;;   [musical pitch classes](chord_geometry.html).
;;
;; - **Symmetric groups** $S_n$ — where characters are indexed by
;;   partitions and the Fourier transform produces matrix-valued
;;   coefficients. See [Symmetric Groups](symmetric_groups.html),
;;   [Character Theory](character_theory.html), and
;;   [Random Transpositions](random_transpositions.html).
;;
;; The harmonica library builds all of these on the same protocol foundation
;; demonstrated here.
