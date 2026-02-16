;; # The DFT as Fourier Transform on a Group
;;
;; The Discrete Fourier Transform (DFT) is one of the most important algorithms
;; in computing. It decomposes a signal into "frequencies" — but what are these
;; frequencies, really?
;;
;; The answer comes from group theory: the DFT is the **Fourier transform on
;; the cyclic group** $\mathbb{Z}/n\mathbb{Z}$. The "frequencies" are the **irreducible
;; representations** (characters) of this group. The DFT matrix **is** the
;; character table.
;;
;; This notebook makes the connection explicit, step by step.

(ns reel-book.dft-as-group-fourier
  (:require
   [scicloj.reel.core :as reel]
   [fastmath.complex :as c]
   [fastmath.transform :as t]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [tech.v3.datatype.convolve :as dt-conv]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [scicloj.kindly.v4.kind :as kind]))

;; ## A signal is a function on a group

;; Suppose we record monthly average temperatures (°C) over two years:

(def temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3
   3 4 8 13 18 23 26 25 20 14 8 4])

;; These 24 numbers implicitly define a periodic pattern — after month 23,
;; the cycle repeats. The clear seasonal pattern (cold winters, warm summers)
;; repeats twice.
;;
;; Mathematically, the indices $0, \ldots, 23$ form the **cyclic group
;; $\mathbb{Z}/24\mathbb{Z}$**: integers with addition mod 24. A signal of length 24
;; is a function on this group.

(def G (reel/cyclic-group 24))

(reel/elements G)

(kind/test-last
 [= (range 24)])

;; The group operation is addition mod 24.

(reel/op G 15 9)

(kind/test-last
 [= 0])

(reel/op G 18 10)

(kind/test-last
 [= 4])

;; Every element has an inverse.

(reel/inv G 15)

(kind/test-last
 [= 9])

;; ## Characters: rotations at different speeds

;; A **character** of $\mathbb{Z}/n\mathbb{Z}$ is a homomorphism from the group to the complex
;; numbers of magnitude 1 — that is, a mapping that preserves the group
;; operation and lands on the unit circle.
;;
;; For $\mathbb{Z}/n\mathbb{Z}$, the characters are:
;;
;; $$\chi_k(g) = \omega^{kg}$$
;;
;; where $\omega = e^{2\pi i/n}$ is the primitive $n$-th root of unity and
;; $k = 0, 1, \ldots, n-1$.
;;
;; Each character $\chi_k$ is a "rotation at speed $k$" — it goes around the
;; unit circle $k$ times as $g$ goes from 0 to $n-1$. This is exactly the
;; "rotations at different speeds" intuition from signal processing.

;; ## The character table is the DFT matrix

;; The **character table** collects all characters into a matrix. Row $k$
;; gives the values of character $\chi_k$ at each group element.

(def ct (reel/character-table G))

;; All entries have magnitude 1 (they lie on the unit circle).

(let [table (:table ct)
      n (reel/order G)]
  (every? (fn [v] (< (Math/abs (- (c/abs v) 1.0)) 1e-10))
          (for [k (range n) g (range n)]
            ((table k) g))))

(kind/test-last
 [true?])

;; The first row ($k = 0$) is the **trivial character** — all ones.

(every? #(< (Math/abs (- (c/re %) 1.0)) 1e-10) ((:table ct) 0))

(kind/test-last
 [true?])

;; Let's visualize a few characters as rotations. Character $\chi_k$
;; completes $k$ full rotations over the 24 group elements.

(-> (tc/dataset
     (let [table (:table ct)]
       (for [k [0 1 2 3]
             g (range 24)]
         {:month g
          :real-part (c/re ((table k) g))
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

(def signal (mapv #(c/complex (double %)) temperatures))

(def f-hat (reel/fourier-transform ct signal))

;; The $k = 0$ coefficient is the sum of all values (the DC component).

(c/re (f-hat 0))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 320.0)) 1e-10))])

;; Let's see the magnitude spectrum — how strong each "frequency" is.

(-> (tc/dataset
     {:frequency (range 24)
      :magnitude (mapv c/abs f-hat)})
    (plotly/base {:=x :frequency
                  :=y :magnitude
                  :=x-title "Frequency k (character index)"
                  :=y-title "|f-hat(k)|"
                  :=title "Fourier spectrum of monthly temperatures on Z/24Z"})
    (plotly/layer-line)
    (plotly/layer-point {:=mark-size 6})
    plotly/plot)

;; The dominant oscillating component is $k = 2$ — two complete cycles
;; over the 24-month window, which is the **annual cycle** (period = 12
;; months). This is the seasonal pattern. The $k = 22$ peak is its conjugate
;; mirror (the spectrum of real signals is symmetric).
;;
;; The small peak at $k = 1$ captures the slight year-over-year warming trend
;; (one cycle per 24 months = a 2-year period).

;; ## Comparison with the standard FFT

;; Let's verify that our group-theoretic Fourier transform gives the same
;; result as the standard FFT from fastmath.
;;
;; The fastmath FFT returns interleaved `[re_0, im_0, re_1, im_1, ...]` for
;; the first $N/2$ coefficients (exploiting Hermitian symmetry). We extract
;; them and compare magnitudes with our full result.

(let [fft-result (t/forward-1d (t/transformer :real :fft) temperatures)
      fft-coefficients (let [data (vec fft-result)
                             n (/ (count data) 2)]
                         (mapv (fn [k]
                                 (c/complex (data (* 2 k))
                                            (data (inc (* 2 k)))))
                               (range n)))]
  (every? true?
          (map (fn [a b] (< (Math/abs (- a b)) 1e-8))
               (mapv c/abs (take 12 f-hat))
               (mapv c/abs fft-coefficients))))

(kind/test-last
 [true?])

;; The magnitudes match. The group-theoretic Fourier transform and the
;; standard DFT compute exactly the same thing — because they **are** the
;; same thing.

;; ## Character orthogonality

;; The characters of a finite group satisfy beautiful orthogonality relations.
;; These are not just mathematical curiosities — they are what make the
;; Fourier transform invertible.
;;
;; **Row orthogonality**: different characters are orthogonal.
;;
;; $$\frac{1}{|G|} \sum_{g \in G} \chi_j(g) \overline{\chi_k(g)} = \delta_{jk}$$

(def orthogonality-data
  (let [table (:table ct)
        sizes (:class-sizes ct)
        n 24]
    (for [j (range 4)
          k (range 4)]
      {:j j :k k
       :inner-product-magnitude
       (c/abs (reel/character-inner-product
               (table j) (table k) sizes n))})))

(kind/table
 {:column-names ["$j$" "$k$" "$|\\langle\\chi_j, \\chi_k\\rangle|$"]
  :row-vectors (mapv (fn [{:keys [j k inner-product-magnitude]}]
                       [j k (format "%.10f" inner-product-magnitude)])
                     orthogonality-data)})

;; Diagonal entries are 1.0, off-diagonal entries are 0.0 (to numerical precision).

(every? (fn [{:keys [j k inner-product-magnitude]}]
          (if (= j k)
            (< (Math/abs (- inner-product-magnitude 1.0)) 1e-10)
            (< inner-product-magnitude 1e-10)))
        orthogonality-data)

(kind/test-last
 [true?])

;; ## Perfect reconstruction

;; The inverse Fourier transform recovers the original signal exactly.
;;
;; $$f(g) = \frac{1}{|G|} \sum_{k} \hat{f}(k) \cdot \chi_k(g)$$

(let [reconstructed (reel/inverse-fourier-transform ct f-hat)]
  (apply max (map (fn [orig recon]
                    (c/abs (c/sub recon orig)))
                  signal
                  reconstructed)))

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
;; For $\mathbb{Z}/n\mathbb{Z}$, this is the familiar **cyclic convolution theorem**.

(def f-fn (mapv #(c/complex (double %))
                [1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3]))
(def h-fn (mapv #(c/complex (double %))
                [0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))

;; Convolve via the library (which uses the Fourier domain internally).

(def convolved (reel/convolve ct f-fn h-fn))

(mapv #(Math/round (c/re %)) convolved)

(kind/test-last
 [= [3 4 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

;; Verify: the Fourier transform of the convolution equals the pointwise
;; product of the individual transforms.

(let [f-fn-hat (reel/fourier-transform ct f-fn)
      h-fn-hat (reel/fourier-transform ct h-fn)
      convolved-hat (reel/fourier-transform ct convolved)
      pointwise-product (mapv c/mult f-fn-hat h-fn-hat)]
  (every? true?
          (map (fn [a b] (< (c/abs (c/sub a b)) 1e-8))
               convolved-hat
               pointwise-product)))

(kind/test-last
 [true?])

;; ## Parseval's theorem (energy conservation)

;; The total "energy" of a signal is preserved under the Fourier transform.
;;
;; $$\sum_{g} |f(g)|^2 = \frac{1}{|G|} \sum_{k} |\hat{f}(k)|^2$$

(let [energy-time (reduce + (map #(let [m (c/abs %)] (* m m)) signal))
      energy-freq (/ (reduce + (map #(let [m (c/abs %)] (* m m)) f-hat))
                     (double (reel/order G)))]
  (< (Math/abs (- energy-time energy-freq)) 1e-8))

(kind/test-last
 [true?])

;; ## Connection to dtype-next convolution
;;
;; The dtype-next library provides `convolve1d` for efficient real-valued
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

(let [group-conv (let [f (mapv #(c/complex (double %)) f-real)
                       h (mapv #(c/complex (double %)) h-real)]
                   (mapv #(c/re %) (reel/convolve ct f h)))]
  (every? #(< (Math/abs (double %)) 1e-10)
          (map - cyclic-from-linear group-conv)))

(kind/test-last
 [true?])

;; The connection: reel's `convolve` computes cyclic convolution via the
;; group Fourier transform (pointwise multiply in the frequency domain),
;; which is equivalent to folding a linear convolution. For real-valued
;; signals, dtype-next's `convolve1d` provides the fast underlying
;; linear convolution; reel adds the group-theoretic structure.

;; ## What comes next
;;
;; The cyclic group is the simplest case. The same framework — groups,
;; characters, Fourier transform — extends to:
;;
;; - **Dihedral groups** (symmetries of regular polygons) — for Burnside
;;   counting and musical pitch class theory
;;
;; - **Symmetric groups** $S_n$ (permutations) — where characters are indexed
;;   by partitions and the Fourier transform produces matrix-valued
;;   coefficients. This is the setting for Diaconis's card shuffling analysis.
;;
;; - **Product groups** $\mathbb{Z}/n_1\mathbb{Z} \times \mathbb{Z}/n_2\mathbb{Z}$ — giving the 2D DFT for image
;;   processing
;;
;; The reel library builds all of these on the same protocol foundation
;; demonstrated here.
