(ns
 harmonica-book.product-group-dft-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l29 (def G1 (hm/cyclic-group 4)))


(def v4_l30 (def G2 (hm/cyclic-group 3)))


(def v5_l31 (def G (hm/product-group G1 G2)))


(def v6_l33 (hm/order G))


(deftest t7_l35 (is (= v6_l33 12)))


(def v9_l39 (take 6 (hm/elements G)))


(def v11_l43 (hm/op G [1 2] [3 1]))


(deftest t12_l45 (is (= v11_l43 [0 0])))


(def v14_l58 (def ct (hm/character-table G)))


(def v15_l60 ct)


(def v16_l62 (count (:irrep-labels ct)))


(deftest t17_l64 (is (= v16_l62 12)))


(def
 v19_l75
 (def image-data [1.0 0.0 0.0 0.0 2.0 0.0 0.0 0.0 3.0 1.0 1.0 1.0]))


(def
 v20_l81
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
 v22_l96
 (let
  [elts
   (vec (hm/elements G))
   f-map
   (into {} (map-indexed (fn [i e] [e (nth image-data i)]) elts))]
  (count f-map)))


(deftest t23_l100 (is (= v22_l96 12)))


(def v25_l104 (def signal (t/complex-tensor-real image-data)))


(def v26_l106 (def f-hat (hm/fourier-transform ct signal)))


(def v27_l108 f-hat)


(def v29_l112 (el/re (f-hat 0)))


(deftest
 t30_l114
 (is ((fn [v] (< (Math/abs (- v 9.0)) 1.0E-10)) v29_l112)))


(def
 v32_l121
 (let
  [recovered
   (hm/inverse-fourier-transform ct f-hat)
   max-err
   (apply max (vec (el/abs (el/- recovered signal))))]
  (< max-err 1.0E-10)))


(deftest t33_l125 (is (true? v32_l121)))


(def
 v35_l141
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
     (t/complex-tensor-real (subvec image-data (* i n) (* (inc i) n))))
    (range m))
   rows-transformed
   (mapv (fn* [p1__66522#] (hm/fourier-transform ct2 p1__66522#)) rows)
   cols-of-transformed
   (mapv
    (fn
     [j]
     (t/complex-tensor
      (mapv
       (fn [row] [(el/re (row j)) (el/im (row j))])
       rows-transformed)))
    (range n))
   cols-transformed
   (mapv
    (fn* [p1__66523#] (hm/fourier-transform ct1 p1__66523#))
    cols-of-transformed)
   separable-result
   (t/complex-tensor
    (mapv
     (fn
      [idx]
      (let
       [i (quot idx n) j (rem idx n) v ((cols-transformed j) i)]
       [(el/re v) (el/im v)]))
     (range (* m n))))
   max-err
   (apply max (vec (el/abs (el/- separable-result f-hat))))]
  (< max-err 1.0E-10)))


(deftest t36_l171 (is (true? v35_l141)))


(def
 v38_l180
 (let
  [energy-space
   (dfn/sum (dfn/* (el/re signal) (el/re signal)))
   mags-sq
   (mapv
    (fn
     [i]
     (let
      [v (f-hat i) r (el/re v) im (el/im v)]
      (+ (* r r) (* im im))))
    (range (hm/order G)))
   energy-freq
   (/ (reduce + mags-sq) (double (hm/order G)))]
  (< (Math/abs (- energy-space energy-freq)) 1.0E-10)))


(deftest t39_l188 (is (true? v38_l180)))


(def
 v41_l196
 (let
  [f
   (t/complex-tensor-real
    [1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0])
   h
   (t/complex-tensor-real
    [1.0 1.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0])
   conv
   (hm/convolve ct f h)
   f-hat
   (hm/fourier-transform ct f)
   h-hat
   (hm/fourier-transform ct h)
   product
   (el/* f-hat h-hat)
   conv-from-freq
   (hm/inverse-fourier-transform ct product)
   max-err
   (apply max (vec (el/abs (el/- conv conv-from-freq))))]
  (< max-err 1.0E-10)))


(deftest t42_l215 (is (true? v41_l196)))


(def
 v44_l222
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
   (hm/fourier-transform ct-sq (t/complex-tensor-real stripe))
   image-grid
   (vec
    (for
     [i (range m)]
     (vec (for [j (range n)] (stripe (+ (* i n) j))))))
   mag-grid
   (vec
    (for
     [i (range m)]
     (vec
      (for
       [j (range n)]
       (let
        [v (f-hat-sq (+ (* i n) j))]
        (Math/sqrt
         (+ (* (el/re v) (el/re v)) (* (el/im v) (el/im v)))))))))]
  (kind/plotly
   {:data
    [{:type "heatmap",
      :z image-grid,
      :colorscale "Greys",
      :reversescale true,
      :showscale false,
      :xaxis "x",
      :yaxis "y"}
     {:type "heatmap",
      :z mag-grid,
      :colorscale "Viridis",
      :showscale false,
      :xaxis "x2",
      :yaxis "y2"}],
    :layout
    {:grid {:rows 1, :columns 2, :pattern "independent"},
     :xaxis2 {:title "freq col", :domain [0.55 1], :dtick 1},
     :width 700,
     :xaxis {:title "column", :domain [0 0.45], :dtick 1},
     :title "Diagonal stripe and its 2D Fourier spectrum",
     :yaxis {:title "row", :autorange "reversed", :dtick 1},
     :yaxis2 {:title "freq row", :autorange "reversed", :dtick 1},
     :height 350,
     :margin {:t 40, :b 40, :l 40, :r 40}}})))
