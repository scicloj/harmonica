;; # The 2D DFT as Product Group Fourier Transform
;;
;; The 1D [Discrete Fourier Transform](https://en.wikipedia.org/wiki/Discrete_Fourier_transform) is the Fourier transform on
;; the [cyclic group](https://en.wikipedia.org/wiki/Cyclic_group) $\mathbb{Z}/n\mathbb{Z}$
;; (see [The DFT as Group Fourier Transform](dft_as_group_fourier.html)).
;;
;; What about the **2D DFT** used in image processing? It's the Fourier
;; transform on the **[direct product](https://en.wikipedia.org/wiki/Direct_product_of_groups)**
;; $\mathbb{Z}/m\mathbb{Z} \times \mathbb{Z}/n\mathbb{Z}$.
;;
;; This notebook makes that connection explicit: we compute the character
;; table of a product group and show that it reproduces the standard 2D DFT.

(ns harmonica-book.product-group-dft
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.linalg.complex :as cx]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Product Groups
;;
;; The [direct product](https://en.wikipedia.org/wiki/Direct_product_of_groups)
;; $G_1 \times G_2$ pairs elements from two groups. The group operation
;; acts componentwise.

(def G1 (hm/cyclic-group 4))
(def G2 (hm/cyclic-group 3))
(def G (hm/product-group G1 G2))

(hm/order G)

(kind/test-last [= 12])

;; Elements are pairs $[g_1, g_2]$:

(take 6 (hm/elements G))

;; The group operation is componentwise addition:

(hm/op G [1 2] [3 1])

(kind/test-last [= [0 0]])

;; ## Character Table of a Product Group
;;
;; For a direct product $G_1 \times G_2$, the irreducible representations
;; are tensor products $\chi_j \otimes \chi_k$ of irreps from each factor.
;; The character table is the
;; [Kronecker product](https://en.wikipedia.org/wiki/Kronecker_product) of
;; the individual character tables.
;;
;; For cyclic groups, each character table is a DFT matrix, so the product
;; group's character table is the **2D DFT matrix**.

(def ct (hm/character-table G))

ct

(count (:irrep-labels ct))

(kind/test-last [= 12])

;; The character table has 12 rows (one per irrep) and 12 columns
;; (one per conjugacy class). For abelian groups, every element is its
;; own conjugacy class, so this is a $12 \times 12$ matrix.

;; ## The 2D DFT on a Small Grid
;;
;; Consider a $4 \times 3$ grid of values — a tiny "image."
;; Each pixel is addressed by a pair $(i, j) \in \mathbb{Z}/4\mathbb{Z} \times \mathbb{Z}/3\mathbb{Z}$.

(def image-data
  [1.0 0.0 0.0
   0.0 2.0 0.0
   0.0 0.0 3.0
   1.0 1.0 1.0])

(kind/plotly
 {:data [{:type "heatmap"
          :z (partition 3 image-data)
          :colorscale "Viridis"
          :showscale true}]
  :layout {:title "4×3 image"
           :xaxis {:title "column" :dtick 1}
           :yaxis {:title "row" :autorange "reversed" :dtick 1}
           :width 300 :height 300
           :margin {:t 40 :b 40 :l 40 :r 40}}})

;; We flatten this into a function on the product group.
;; The group elements are ordered by `hm/elements`, so we need a map
;; from group elements to pixel values.

(let [elts (vec (hm/elements G))
      f-map (into {} (map-indexed (fn [i e] [e (nth image-data i)]) elts))]
  (count f-map))

(kind/test-last [= 12])

;; Now apply the Fourier transform:

(def signal (cx/complex-tensor-real image-data))

(def f-hat (hm/fourier-transform ct signal))

f-hat

;; The DC component ($k = 0$) is the sum of all pixel values:

(cx/re (f-hat 0))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 9.0)) 1e-10))])

;; ## Round-Trip: Inverse Transform
;;
;; The inverse Fourier transform recovers the original image exactly.

(let [recovered (hm/inverse-fourier-transform ct f-hat)
      max-err (apply max (vec (cx/cabs (cx/csub recovered signal))))]
  (< max-err 1e-10))

(kind/test-last [true?])

;; ## Separability of the 2D DFT
;;
;; A key property of the product group Fourier transform: it's
;; **separable**. The 2D DFT of an $m \times n$ image equals applying
;; the 1D DFT along rows, then along columns (or vice versa).
;;
;; The algorithm has three steps:
;;
;; 1. Transform each row with the 1D DFT of the column group
;; 2. Extract columns from the result, transform each with the row group DFT
;; 3. Flatten back to a single vector and compare with the direct product DFT
;;
;; Let's verify this by computing both ways.

(let [m 4 n 3
      ct1 (hm/character-table G1)
      ct2 (hm/character-table G2)
      ;; Reshape image into rows
      rows (mapv (fn [i]
                   (cx/complex-tensor-real
                    (subvec image-data (* i n) (* (inc i) n))))
                 (range m))
      ;; Transform each row (along columns)
      rows-transformed (mapv #(hm/fourier-transform ct2 %) rows)
      ;; Extract columns from transformed rows and transform along rows
      cols-of-transformed (mapv (fn [j]
                                  (cx/complex-tensor
                                   (mapv (fn [row] [(cx/re (row j))
                                                    (cx/im (row j))])
                                         rows-transformed)))
                                (range n))
      cols-transformed (mapv #(hm/fourier-transform ct1 %) cols-of-transformed)
      ;; Flatten back: result[i*n + j] = cols-transformed[j][i]
      separable-result (cx/complex-tensor
                        (mapv (fn [idx]
                                (let [i (quot idx n)
                                      j (rem idx n)
                                      v ((cols-transformed j) i)]
                                  [(cx/re v) (cx/im v)]))
                              (range (* m n))))
      ;; Compare with direct product group transform
      max-err (apply max (vec (cx/cabs (cx/csub separable-result f-hat))))]
  (< max-err 1e-10))

(kind/test-last [true?])

;; The row-then-column approach gives the same result as the product
;; group transform — confirming separability.

;; ## Parseval's Theorem in 2D
;;
;; Energy is preserved: $\sum |f(g)|^2 = \frac{1}{|G|} \sum |\hat{f}(k)|^2$.

(let [energy-space (dfn/sum (dfn/* (cx/re signal) (cx/re signal)))
      mags-sq (mapv (fn [i] (let [v (f-hat i)
                                  r (cx/re v) im (cx/im v)]
                              (+ (* r r) (* im im))))
                    (range (hm/order G)))
      energy-freq (/ (reduce + mags-sq) (double (hm/order G)))]
  (< (Math/abs (- energy-space energy-freq)) 1e-10))

(kind/test-last [true?])

;; ## Convolution in 2D
;;
;; Convolution on the product group corresponds to 2D convolution of
;; images. The convolution theorem holds: convolution in the spatial
;; domain equals pointwise multiplication in the frequency domain.

(let [f (cx/complex-tensor-real
         [1.0 0.0 0.0
          0.0 0.0 0.0
          0.0 0.0 0.0
          0.0 0.0 0.0])
      h (cx/complex-tensor-real
         [1.0 1.0 0.0
          1.0 0.0 0.0
          0.0 0.0 0.0
          0.0 0.0 0.0])
      conv (hm/convolve ct f h)
      ;; Verify via Fourier domain
      f-hat (hm/fourier-transform ct f)
      h-hat (hm/fourier-transform ct h)
      product (cx/cmul f-hat h-hat)
      conv-from-freq (hm/inverse-fourier-transform ct product)
      max-err (apply max (vec (cx/cabs (cx/csub conv conv-from-freq))))]
  (< max-err 1e-10))

(kind/test-last [true?])

;; ## Visualizing the 2D Spectrum
;;
;; The magnitude spectrum shows which "spatial frequencies" are present
;; in the image. Let's visualize it for a pattern with a strong diagonal.

(let [m 6 n 6
      G-sq (hm/product-group (hm/cyclic-group m) (hm/cyclic-group n))
      ct-sq (hm/character-table G-sq)
      ;; Diagonal stripe pattern
      stripe (mapv (fn [idx]
                     (let [i (quot idx n) j (rem idx n)]
                       (if (= (mod (+ i j) 3) 0) 1.0 0.0)))
                   (range (* m n)))
      f-hat-sq (hm/fourier-transform ct-sq (cx/complex-tensor-real stripe))
      ;; Build 2D grids directly
      image-grid (vec (for [i (range m)]
                        (vec (for [j (range n)]
                               (stripe (+ (* i n) j))))))
      mag-grid (vec (for [i (range m)]
                      (vec (for [j (range n)]
                             (let [v (f-hat-sq (+ (* i n) j))]
                               (Math/sqrt (+ (* (cx/re v) (cx/re v))
                                             (* (cx/im v) (cx/im v)))))))))]
  (kind/plotly
   {:data [{:type "heatmap"
            :z image-grid
            :colorscale "Greys"
            :reversescale true
            :showscale false
            :xaxis "x" :yaxis "y"}
           {:type "heatmap"
            :z mag-grid
            :colorscale "Viridis"
            :showscale false
            :xaxis "x2" :yaxis "y2"}]
    :layout {:title "Diagonal stripe and its 2D Fourier spectrum"
             :grid {:rows 1 :columns 2 :pattern "independent"}
             :xaxis {:title "column" :domain [0 0.45] :dtick 1}
             :yaxis {:title "row" :autorange "reversed" :dtick 1}
             :xaxis2 {:title "freq col" :domain [0.55 1] :dtick 1}
             :yaxis2 {:title "freq row" :autorange "reversed" :dtick 1}
             :width 700 :height 350
             :margin {:t 40 :b 40 :l 40 :r 40}}}))

;; The spectrum shows energy concentrated along the diagonal frequency
;; direction — exactly what we expect from a diagonal stripe pattern.

;; ## Summary
;;
;; This notebook demonstrated:
;;
;; - The **2D DFT** is the Fourier transform on $\mathbb{Z}/m\mathbb{Z} \times \mathbb{Z}/n\mathbb{Z}$
;; - The **character table** of a product group encodes both dimensions
;; - **Separability**: row-then-column transform equals the product group transform
;; - **Parseval's theorem** and the **convolution theorem** generalize to 2D
;; - The magnitude spectrum reveals spatial frequency structure
;;
;; For the 1D story, see
;; [The DFT as Group Fourier Transform](dft_as_group_fourier.html).
;; For convolution as random walks and convergence to uniform, see
;; [Random Walks and Convolution](random_walks.html).
;; For groups acting on geometric objects and combinatorial structures, see
;; [Symmetry Sketchpad](symmetry_sketchpad.html) and
;; [Counting Necklaces](counting_necklaces.html).
;; For non-abelian Fourier analysis, see
;; [Random Transpositions](random_transpositions.html) and
;; [Riffle Shuffles](riffle_shuffle.html).
