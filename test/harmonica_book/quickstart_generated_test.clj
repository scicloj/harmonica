(ns
 harmonica-book.quickstart-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l28
 (defn rotation-action [n] (fn [g x] (mod (+ (long x) (long g)) n))))


(def
 v4_l31
 (let
  [G
   (hm/cyclic-group 8)
   ci
   (hm/cycle-index G (rotation-action 8) (range 8))]
  (hm/polya-count ci 3)))


(deftest t5_l35 (is (= v4_l31 834)))


(def
 v7_l41
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
 v8_l47
 (let
  [G
   (hm/dihedral-group 8)
   ci
   (hm/cycle-index G (dihedral-action 8) (range 8))]
  (hm/polya-count ci 3)))


(deftest t9_l51 (is (= v8_l47 498)))


(def v11_l65 (def G (hm/cyclic-group 24)))


(def v12_l66 (def ct (hm/character-table G)))


(def
 v14_l70
 (def
  temperatures
  [2 3 7 12 17 22 25 24 19 13 7 3 3 4 8 13 18 23 26 25 20 14 8 4]))


(def
 v15_l74
 (def
  f-hat
  (hm/fourier-transform ct (cx/complex-tensor-real temperatures))))


(def v17_l79 (cx/re (f-hat 0)))


(deftest
 t18_l81
 (is ((fn [v] (< (Math/abs (- v 320.0)) 1.0E-10)) v17_l79)))


(def
 v20_l86
 (let
  [recovered (hm/inverse-fourier-transform ct f-hat)]
  (every?
   (fn* [p1__85588#] (< (Math/abs (double p1__85588#)) 1.0E-10))
   (map - (vec (cx/re recovered)) temperatures))))


(deftest t21_l90 (is (true? v20_l86)))


(def
 v23_l101
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
 v24_l117
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
