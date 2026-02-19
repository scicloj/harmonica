(ns
 harmonica-book.quickstart-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [tech.v3.datatype.functional :as dfn]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l31
 (defn rotation-action [n] (fn [g x] (mod (+ (long x) (long g)) n))))


(def
 v4_l34
 (let
  [G
   (hm/cyclic-group 8)
   ci
   (hm/cycle-index G (rotation-action 8) (range 8))]
  (hm/polya-count ci 3)))


(deftest t5_l38 (is (= v4_l34 834)))


(def
 v7_l44
 (defn
  dihedral-action
  [n]
  (fn
   [[t k] x]
   (case
    t
    :r
    (mod (+ (long x) (long k)) n)
    :s
    (mod (- (long k) (long x)) n)))))


(def
 v8_l50
 (let
  [G
   (hm/dihedral-group 8)
   ci
   (hm/cycle-index G (dihedral-action 8) (range 8))]
  (hm/polya-count ci 3)))


(deftest t9_l54 (is (= v8_l50 498)))


(def v11_l68 (def G (hm/cyclic-group 24)))


(def v12_l69 (def ct (hm/character-table G)))


(def
 v14_l73
 (def
  temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3 3 4 8 13 18 23 26 25 20 14 8 4]))


(def
 v15_l77
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
 v16_l85
 (def
  f-hat
  (hm/fourier-transform ct (cx/complex-tensor-real temperatures))))


(def v17_l87 f-hat)


(def
 v19_l91
 (let
  [n
   24
   magnitudes
   (mapv
    (fn [k] {:k k, :magnitude (cx/cabs (f-hat k))})
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


(def v21_l108 (cx/re (f-hat 0)))


(deftest
 t22_l110
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v21_l108)))


(def
 v24_l115
 (let
  [mags
   (mapv (fn [k] [k (cx/cabs (f-hat k))]) (range 1 (inc (/ 24 2))))]
  (first (apply max-key second mags))))


(deftest t25_l118 (is (= v24_l115 2)))


(def
 v27_l122
 (let
  [recovered (cx/re (hm/inverse-fourier-transform ct f-hat))]
  (<
   (dfn/reduce-max (dfn/abs (dfn/- recovered temperatures)))
   1.0E-10)))


(deftest t28_l125 (is (true? v27_l122)))


(def
 v30_l136
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
 v31_l152
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
