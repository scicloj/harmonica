(ns
 harmonica-book.quickstart-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.lalinea.tensor :as t]
  [scicloj.lalinea.elementwise :as el]
  [harmonica-book.book-helpers :refer [allclose?]]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l32 (defn rotation-action [n] (fn [g x] (mod (+ x g) n))))


(def
 v4_l35
 (let
  [G
   (hm/cyclic-group 8)
   ci
   (hm/cycle-index G (rotation-action 8) (range 8))]
  (hm/polya-count ci 3)))


(deftest t5_l39 (is (= v4_l35 834)))


(def
 v7_l45
 (defn
  dihedral-action
  [n]
  (fn [[t k] x] (case t :r (mod (+ x k) n) :s (mod (- k x) n)))))


(def
 v8_l51
 (let
  [G
   (hm/dihedral-group 8)
   ci
   (hm/cycle-index G (dihedral-action 8) (range 8))]
  (hm/polya-count ci 3)))


(deftest t9_l55 (is (= v8_l51 498)))


(def v11_l69 (def G (hm/cyclic-group 24)))


(def v12_l70 (def ct (hm/character-table G)))


(def
 v14_l74
 (def
  temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3 3 4 8 13 18 23 26 25 20 14 8 4]))


(def
 v15_l78
 (->
  (tc/dataset {:month (range 24), :temp temperatures})
  (plotly/base
   {:=x :month,
    :=y :temp,
    :=title "Monthly temperatures (°C)",
    :=x-title "month",
    :=y-title "°C"})
  (plotly/layer-line)
  (plotly/layer-point {:=mark-size 5})
  plotly/plot))


(def
 v16_l86
 (def
  f-hat
  (hm/fourier-transform ct (t/complex-tensor-real temperatures))))


(def v17_l88 f-hat)


(def
 v19_l92
 (let
  [n
   24
   magnitudes
   (mapv
    (fn [k] {:k k, :magnitude (el/abs (f-hat k))})
    (range (inc (/ n 2))))]
  (->
   (tc/dataset magnitudes)
   (plotly/base {:=x :k, :=y :magnitude})
   (plotly/layer-bar)
   (plotly/update-data
    (fn
     [d]
     (assoc
      d
      :=layout
      {:title "Fourier magnitudes |f̂(k)|",
       :xaxis {:title "frequency k", :dtick 1},
       :yaxis {:title "|f̂(k)|"},
       :width 500,
       :height 300})))
   plotly/plot)))


(def v21_l109 (el/re (f-hat 0)))


(deftest
 t22_l111
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v21_l109)))


(def
 v24_l116
 (let
  [mags
   (mapv (fn [k] [k (el/abs (f-hat k))]) (range 1 (inc (/ 24 2))))]
  (first (apply max-key second mags))))


(deftest t25_l119 (is (= v24_l116 2)))


(def
 v27_l123
 (let
  [recovered (el/re (hm/inverse-fourier-transform ct f-hat))]
  (allclose? recovered temperatures)))


(deftest t28_l126 (is (true? v27_l123)))


(def
 v30_l137
 (defn
  make-rosette
  [n motif]
  (let
   [matrices
    (mapcat
     (fn
      [k]
      (let
       [a (* 2.0 Math/PI (/ (double k) (double n)))]
       [[[(Math/cos a) (- (Math/sin a))] [(Math/sin a) (Math/cos a)]]
        [[(Math/cos a) (Math/sin a)]
         [(Math/sin a) (- (Math/cos a))]]]))
     (range n))]
   (mapv
    (fn
     [[[a b] [c d]]]
     (mapv
      (fn [[x y]] [(+ (* a x) (* b y)) (+ (* c x) (* d y))])
      motif))
    matrices))))


(def
 v31_l153
 (let
  [motif
   (mapv
    (fn
     [i]
     (let
      [t (/ (double i) 30) r (+ 0.3 (* 0.5 t)) a (* t 0.9)]
      [(* r (Math/cos a)) (* r (Math/sin a))]))
    (range 31))
   copies
   (make-rosette 6 motif)
   colors
   (cycle
    ["#e74c3c"
     "#3498db"
     "#2ecc71"
     "#f39c12"
     "#9b59b6"
     "#e67e22"
     "#c0392b"
     "#2980b9"
     "#27ae60"
     "#d35400"
     "#8e44ad"
     "#1abc9c"])]
  (kind/plotly
   {:data
    (vec
     (map-indexed
      (fn
       [i copy]
       {:type "scatter",
        :mode "lines",
        :x (mapv first copy),
        :y (mapv second copy),
        :line {:color (nth colors i), :width 2},
        :showlegend false})
      copies)),
    :layout
    {:title "D₆ rosette — 12 copies of one curve",
     :xaxis {:visible false, :scaleanchor "y"},
     :yaxis {:visible false},
     :width 400,
     :height 400}})))


(def
 v33_l189
 (def V4 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2))))


(def
 v35_l195
 (def
  motif
  [[62 0.8]
   [69 0.8]
   [65 0.8]
   [62 0.5]
   [61 0.5]
   [62 0.5]
   [64 0.5]
   [65 0.8]]))


(def
 v36_l197
 (defn
  apply-v4
  [pivot [r i] melody]
  (let
   [m
    (if
     (= i 1)
     (mapv (fn [[p d]] [(- (* 2 pivot) p) d]) melody)
     melody)]
   (if (= r 1) (vec (reverse m)) m))))


(def
 v38_l203
 (let
  [pivot (ffirst motif)]
  {:original (mapv first motif),
   :retrograde (mapv first (apply-v4 pivot [1 0] motif)),
   :inversion (mapv first (apply-v4 pivot [0 1] motif)),
   :retrograde-inv (mapv first (apply-v4 pivot [1 1] motif))}))


(def v40_l211 (def sample-rate 44100.0))


(def
 v41_l213
 (defn
  play
  [melody]
  (let
   [amp
    2500.0
    offsets
    (reductions + 0 (map (fn [[_ d]] (long (* d sample-rate))) melody))
    total
    (last offsets)]
   (kind/audio
    {:sample-rate sample-rate,
     :samples
     (t/make-reader
      :float32
      total
      (let
       [[note-idx note-start]
        (loop
         [i 0]
         (if
          (< idx (nth offsets (inc i)))
          [i (nth offsets i)]
          (recur (inc i))))
        [pitch dur]
        (melody note-idx)
        n-note
        (long (* dur sample-rate))
        t
        (- idx note-start)
        attack
        (long (* 0.015 sample-rate))
        sounding
        (long (* 0.85 n-note))
        release
        (long (* 0.06 sample-rate))
        freq
        (* 440.0 (Math/pow 2.0 (/ (- (double pitch) 69.0) 12.0)))
        phase
        (/ (double t) sample-rate)]
       (if
        (>= t sounding)
        (float 0.0)
        (let
         [env
          (cond
           (< t attack)
           (/ (double t) attack)
           (> t (- sounding release))
           (*
            (Math/exp (* -1.5 (/ (double (- t attack)) sounding)))
            (/ (double (- sounding t)) release))
           :else
           (Math/exp (* -1.5 (/ (double (- t attack)) sounding))))
          wave
          (+
           (* 0.65 (Math/sin (* 2.0 Math/PI freq phase)))
           (* 0.25 (Math/sin (* 2.0 Math/PI 2.0 freq phase)))
           (* 0.1 (Math/sin (* 2.0 Math/PI 3.0 freq phase))))]
         (float (* amp env wave))))))}))))


(def v43_l250 (play motif))


(def v44_l252 (play (apply-v4 (ffirst motif) [1 0] motif)))


(def v45_l254 (play (apply-v4 (ffirst motif) [0 1] motif)))


(def v46_l256 (play (apply-v4 (ffirst motif) [1 1] motif)))
