(ns
 harmonica-book.product-group-dft-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l28 (def G1 (hm/cyclic-group 4)))


(def v4_l29 (def G2 (hm/cyclic-group 3)))


(def v5_l30 (def G (hm/product-group G1 G2)))


(def v6_l32 (hm/order G))


(deftest t7_l34 (is (= v6_l32 12)))


(def v9_l38 (take 6 (hm/elements G)))


(def v11_l42 (hm/op G [1 2] [3 1]))


(deftest t12_l44 (is (= v11_l42 [0 0])))


(def v14_l57 (def ct (hm/character-table G)))


(def v15_l59 ct)


(def v16_l61 (count (:irrep-labels ct)))


(deftest t17_l63 (is (= v16_l61 12)))


(def
 v19_l74
 (def image-data [1.0 0.0 0.0 0.0 2.0 0.0 0.0 0.0 3.0 1.0 1.0 1.0]))


(def
 v20_l80
 (kind/plotly
  {:data
   [{:type "heatmap",
     :z (partition 3 image-data),
     :colorscale "Viridis",
     :showscale true}],
   :layout
   {:title "4×3 image",
    :xaxis {:title "column", :dtick 1},
    :yaxis {:title "row", :autorange "reversed", :dtick 1},
    :width 300,
    :height 300,
    :margin {:t 40, :b 40, :l 40, :r 40}}}))


(def
 v22_l95
 (let
  [elts
   (vec (hm/elements G))
   f-map
   (into {} (map-indexed (fn [i e] [e (nth image-data i)]) elts))]
  (count f-map)))


(deftest t23_l99 (is (= v22_l95 12)))


(def v25_l103 (def signal (cx/complex-tensor-real image-data)))


(def v26_l105 (def f-hat (hm/fourier-transform ct signal)))


(def v27_l107 f-hat)


(def v29_l111 (cx/re (f-hat 0)))


(deftest
 t30_l113
 (is ((fn [v] (< (Math/abs (- v 9.0)) 1.0E-10)) v29_l111)))


(def
 v32_l120
 (let
  [recovered
   (hm/inverse-fourier-transform ct f-hat)
   max-err
   (apply max (vec (cx/cabs (cx/csub recovered signal))))]
  (< max-err 1.0E-10)))


(deftest t33_l124 (is (true? v32_l120)))


(def
 v35_l140
 (let
  [m
   4
   n
   3
   ct1
   (hm/character-table G1)
   ct2
   (hm/character-table G2)
   rows
   (mapv
    (fn
     [i]
     (cx/complex-tensor-real
      (subvec image-data (* i n) (* (inc i) n))))
    (range m))
   rows-transformed
   (mapv (fn* [p1__73238#] (hm/fourier-transform ct2 p1__73238#)) rows)
   cols-of-transformed
   (mapv
    (fn
     [j]
     (cx/complex-tensor
      (mapv
       (fn [row] [(cx/re (row j)) (cx/im (row j))])
       rows-transformed)))
    (range n))
   cols-transformed
   (mapv
    (fn* [p1__73239#] (hm/fourier-transform ct1 p1__73239#))
    cols-of-transformed)
   separable-result
   (cx/complex-tensor
    (mapv
     (fn
      [idx]
      (let
       [i (quot idx n) j (rem idx n) v ((cols-transformed j) i)]
       [(cx/re v) (cx/im v)]))
     (range (* m n))))
   max-err
   (apply max (vec (cx/cabs (cx/csub separable-result f-hat))))]
  (< max-err 1.0E-10)))


(deftest t36_l170 (is (true? v35_l140)))


(def
 v38_l179
 (let
  [energy-space
   (dfn/sum (dfn/* (cx/re signal) (cx/re signal)))
   mags-sq
   (mapv
    (fn
     [i]
     (let
      [v (f-hat i) r (cx/re v) im (cx/im v)]
      (+ (* r r) (* im im))))
    (range (hm/order G)))
   energy-freq
   (/ (reduce + mags-sq) (double (hm/order G)))]
  (< (Math/abs (- energy-space energy-freq)) 1.0E-10)))


(deftest t39_l187 (is (true? v38_l179)))


(def
 v41_l195
 (let
  [f
   (cx/complex-tensor-real
    [1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0])
   h
   (cx/complex-tensor-real
    [1.0 1.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0])
   conv
   (hm/convolve ct f h)
   f-hat
   (hm/fourier-transform ct f)
   h-hat
   (hm/fourier-transform ct h)
   product
   (cx/cmul f-hat h-hat)
   conv-from-freq
   (hm/inverse-fourier-transform ct product)
   max-err
   (apply max (vec (cx/cabs (cx/csub conv conv-from-freq))))]
  (< max-err 1.0E-10)))


(deftest t42_l214 (is (true? v41_l195)))


(def
 v44_l221
 (let
  [m
   6
   n
   6
   G-sq
   (hm/product-group (hm/cyclic-group m) (hm/cyclic-group n))
   ct-sq
   (hm/character-table G-sq)
   stripe
   (mapv
    (fn
     [idx]
     (let
      [i (quot idx n) j (rem idx n)]
      (if (= (mod (+ i j) 3) 0) 1.0 0.0)))
    (range (* m n)))
   f-hat-sq
   (hm/fourier-transform ct-sq (cx/complex-tensor-real stripe))
   mag-data
   (mapv
    (fn
     [idx]
     (let
      [i
       (quot idx n)
       j
       (rem idx n)
       v
       (f-hat-sq idx)
       r
       (cx/re v)
       im
       (cx/im v)]
      {:row i,
       :col j,
       :type "Spectrum",
       :value (Math/sqrt (+ (* r r) (* im im)))}))
    (range (* m n)))
   space-data
   (mapv
    (fn
     [idx]
     (let
      [i (quot idx n) j (rem idx n)]
      {:row i, :col j, :type "Image", :value (nth stripe idx)}))
    (range (* m n)))]
  (kind/plotly
   {:data
    [{:reversescale true,
      :y (mapv :row space-data),
      :colorscale "Greys",
      :name "Image",
      :type "heatmap",
      :xaxis "x",
      :z
      (let
       [arr (vec (repeat m (vec (repeat n 0.0))))]
       (reduce
        (fn [a {:keys [row col value]}] (assoc-in a [row col] value))
        arr
        space-data)),
      :yaxis "y",
      :x (mapv :col space-data)}
     {:type "heatmap",
      :x (mapv :col mag-data),
      :y (mapv :row mag-data),
      :z
      (let
       [arr (vec (repeat m (vec (repeat n 0.0))))]
       (reduce
        (fn [a {:keys [row col value]}] (assoc-in a [row col] value))
        arr
        mag-data)),
      :colorscale "Viridis",
      :name "Spectrum",
      :xaxis "x2",
      :yaxis "y2"}],
    :layout
    {:grid {:rows 1, :columns 2, :pattern "independent"},
     :xaxis2 {:title "freq col", :domain [0.55 1]},
     :width 700,
     :xaxis {:title "column", :domain [0 0.45]},
     :title "Diagonal stripe and its 2D Fourier spectrum",
     :yaxis {:title "row", :autorange "reversed"},
     :yaxis2 {:title "freq row", :autorange "reversed"},
     :height 350,
     :margin {:t 40, :b 40, :l 40, :r 40}}})))
