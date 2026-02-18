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
 (def
  f-hat
  (hm/fourier-transform ct (cx/complex-tensor-real temperatures))))


(def
 v17_l81
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


(def v19_l98 (cx/re (f-hat 0)))


(deftest
 t20_l100
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v19_l98)))


(def
 v22_l105
 (let
  [mags
   (mapv (fn [k] [k (cx/cabs (f-hat k))]) (range 1 (inc (/ 24 2))))]
  (first (apply max-key second mags))))


(deftest t23_l108 (is (= v22_l105 2)))


(def
 v25_l112
 (let
  [recovered (cx/re (hm/inverse-fourier-transform ct f-hat))]
  (<
   (dfn/reduce-max (dfn/abs (dfn/- recovered temperatures)))
   1.0E-10)))


(deftest t26_l115 (is (true? v25_l112)))


(def
 v28_l126
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
 v29_l142
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
