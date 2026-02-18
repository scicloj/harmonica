(ns
 harmonica-book.symmetry-sketchpad-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.protocols :as p]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l28
 (defn
  rotation-matrix
  "2D rotation matrix for angle theta (radians)."
  [theta]
  [[(Math/cos theta) (- (Math/sin theta))]
   [(Math/sin theta) (Math/cos theta)]]))


(def
 v4_l34
 (defn
  reflection-matrix
  "2D reflection matrix across the line at angle theta/2 from x-axis."
  [theta]
  [[(Math/cos theta) (Math/sin theta)]
   [(Math/sin theta) (- (Math/cos theta))]]))


(def
 v5_l40
 (defn
  apply-matrix
  "Apply a 2x2 matrix to a point [x y]."
  [[[a b] [c d]] [x y]]
  [(+ (* a x) (* b y)) (+ (* c x) (* d y))]))


(def
 v7_l48
 (let
  [theta 1.23 [[a b] [c d]] (rotation-matrix theta)]
  (< (Math/abs (- (- (* a d) (* b c)) 1.0)) 1.0E-14)))


(deftest t8_l52 (is (true? v7_l48)))


(def
 v10_l56
 (let
  [theta 0.7 [[a b] [c d]] (reflection-matrix theta)]
  (< (Math/abs (- (- (* a d) (* b c)) -1.0)) 1.0E-14)))


(deftest t11_l60 (is (true? v10_l56)))


(def
 v13_l65
 (defn
  dihedral-action
  "Action of D_n on a 2D point."
  [n [tag k] [x y]]
  (let
   [angle (* 2.0 Math/PI (/ (double k) (double n)))]
   (case
    tag
    :r
    (apply-matrix (rotation-matrix angle) [x y])
    :s
    (apply-matrix (reflection-matrix angle) [x y])))))


(def
 v14_l73
 (defn
  cyclic-action
  "Action of C_n on a 2D point."
  [n g [x y]]
  (let
   [angle (* 2.0 Math/PI (/ (double g) (double n)))]
   (apply-matrix (rotation-matrix angle) [x y]))))


(def
 v16_l84
 (def
  simple-motif
  "A leaf-like curve in the first wedge."
  (let
   [steps 30]
   (mapv
    (fn
     [i]
     (let
      [t (/ (double i) steps) r (+ 0.3 (* 0.5 t)) angle (* t 0.9)]
      [(* r (Math/cos angle)) (* r (Math/sin angle))]))
    (range (inc steps))))))


(def
 v17_l95
 (kind/plotly
  {:data
   [{:type "scatter",
     :mode "lines",
     :x (mapv first simple-motif),
     :y (mapv second simple-motif),
     :line {:color "#e74c3c", :width 2},
     :showlegend false}],
   :layout
   {:title "The motif — one leaf-like curve",
    :xaxis {:visible false, :scaleanchor "y"},
    :yaxis {:visible false},
    :width 300,
    :height 300,
    :margin {:t 40, :b 10, :l 10, :r 10}}}))


(def
 v19_l112
 (defn
  make-rosette-cn
  "Replicate a motif under C_n."
  [n motif]
  (mapv
   (fn [g] (mapv (fn [pt] (cyclic-action n g pt)) motif))
   (range n))))


(def
 v20_l119
 (defn
  make-rosette-dn
  "Replicate a motif under D_n."
  [n motif]
  (let
   [G (hm/dihedral-group n)]
   (mapv
    (fn [g] (mapv (fn [pt] (dihedral-action n g pt)) motif))
    (hm/elements G)))))


(def
 v22_l131
 (let
  [n
   5
   copies
   (make-rosette-cn n simple-motif)
   colors
   ["#e74c3c" "#3498db" "#2ecc71" "#f39c12" "#9b59b6"]
   traces
   (mapv
    (fn
     [i copy]
     {:type "scatter",
      :mode "lines",
      :x (mapv first copy),
      :y (mapv second copy),
      :line {:color (colors i), :width 2},
      :showlegend false})
    (range)
    copies)]
  (kind/plotly
   {:data traces,
    :layout
    {:title (str "C₅ rosette — rotational symmetry"),
     :xaxis {:visible false, :scaleanchor "y"},
     :yaxis {:visible false},
     :width 400,
     :height 400}})))


(def v24_l150 (count (make-rosette-cn 5 simple-motif)))


(deftest t25_l152 (is (= v24_l150 5)))


(def
 v27_l159
 (let
  [n
   5
   copies
   (make-rosette-dn n simple-motif)
   colors
   (cycle
    ["#e74c3c"
     "#3498db"
     "#2ecc71"
     "#f39c12"
     "#9b59b6"
     "#e67e22"
     "#1abc9c"
     "#8e44ad"
     "#c0392b"
     "#2980b9"])
   traces
   (mapv
    (fn
     [i copy]
     {:type "scatter",
      :mode "lines",
      :x (mapv first copy),
      :y (mapv second copy),
      :line {:color (nth colors i), :width 2},
      :showlegend false})
    (range)
    copies)]
  (kind/plotly
   {:data traces,
    :layout
    {:title (str "D₅ rosette — rotational + reflective symmetry"),
     :xaxis {:visible false, :scaleanchor "y"},
     :yaxis {:visible false},
     :width 400,
     :height 400}})))


(def v29_l179 (count (make-rosette-dn 5 simple-motif)))


(deftest t30_l181 (is (= v29_l179 10)))


(def
 v32_l188
 (let
  [plots
   (mapv
    (fn
     [n]
     (let
      [copies
       (make-rosette-dn n simple-motif)
       colors
       (cycle
        ["#e74c3c"
         "#3498db"
         "#2ecc71"
         "#f39c12"
         "#9b59b6"
         "#e67e22"
         "#1abc9c"
         "#8e44ad"
         "#c0392b"
         "#2980b9"
         "#27ae60"
         "#d35400"
         "#2c3e50"
         "#f1c40f"
         "#7f8c8d"
         "#16a085"])
       traces
       (mapv
        (fn
         [i copy]
         {:type "scatter",
          :mode "lines",
          :x (mapv first copy),
          :y (mapv second copy),
          :line {:color (nth colors i), :width 1.5},
          :showlegend false})
        (range)
        copies)]
      (kind/plotly
       {:data traces,
        :layout
        {:title (str "D" n " (" (* 2 n) " elements)"),
         :xaxis {:visible false, :scaleanchor "y"},
         :yaxis {:visible false},
         :width 300,
         :height 300,
         :margin {:t 40, :b 10, :l 10, :r 10}}})))
    [3 4 5 6 7 8])]
  (kind/fragment plots)))


(def
 v34_l217
 (let
  [results
   (for
    [n [3 4 5 6 7 8]]
    (let
     [G
      (hm/dihedral-group n)
      test-pt
      [0.5 0.3]
      elts
      (vec (hm/elements G))]
     (every?
      (fn
       [[g h]]
       (let
        [gh
         (hm/op G g h)
         via-compose
         (dihedral-action n g (dihedral-action n h test-pt))
         via-product
         (dihedral-action n gh test-pt)
         err
         (Math/sqrt
          (+
           (Math/pow (- (first via-compose) (first via-product)) 2)
           (Math/pow
            (- (second via-compose) (second via-product))
            2)))]
        (< err 1.0E-10)))
      (for [a elts b elts] [a b]))))]
  (every? identity results)))


(deftest t35_l232 (is (true? v34_l217)))


(def
 v37_l240
 (def
  asymmetric-motif
  "A clearly asymmetric hook shape."
  (let
   [steps 20]
   (concat
    (mapv
     (fn
      [i]
      (let
       [t (/ (double i) steps) r (+ 0.3 (* 0.4 t)) angle (* t 0.7)]
       [(* r (Math/cos angle)) (* r (Math/sin angle))]))
     (range (inc steps)))
    (mapv
     (fn
      [i]
      (let
       [t
        (/ (double i) 8)
        base-r
        0.7
        base-angle
        0.7
        hook-r
        (- base-r (* 0.15 t))
        hook-angle
        (+ base-angle (* 0.4 t))]
       [(* hook-r (Math/cos hook-angle))
        (* hook-r (Math/sin hook-angle))]))
     (range 1 9))))))


(def
 v38_l263
 (let
  [n
   6
   cn-copies
   (make-rosette-cn n asymmetric-motif)
   dn-copies
   (make-rosette-dn n asymmetric-motif)
   make-traces
   (fn
    [copies colors]
    (mapv
     (fn
      [i copy]
      {:type "scatter",
       :mode "lines",
       :x (mapv first copy),
       :y (mapv second copy),
       :line {:color (nth colors i), :width 2},
       :showlegend false})
     (range)
     copies))
   colors-6
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
  (kind/fragment
   [(kind/plotly
     {:data (make-traces cn-copies colors-6),
      :layout
      {:title "C₆ — all copies have same handedness",
       :xaxis {:visible false, :scaleanchor "y"},
       :yaxis {:visible false},
       :width 350,
       :height 350,
       :margin {:t 40, :b 10, :l 10, :r 10}}})
    (kind/plotly
     {:data (make-traces dn-copies colors-6),
      :layout
      {:title "D₆ — includes mirror images",
       :xaxis {:visible false, :scaleanchor "y"},
       :yaxis {:visible false},
       :width 350,
       :height 350,
       :margin {:t 40, :b 10, :l 10, :r 10}}})])))


(def
 v40_l300
 (let
  [n
   7
   pt
   [0.6 0.2]
   G
   (hm/dihedral-group n)
   orbit-pts
   (mapv (fn [g] (dihedral-action n g pt)) (hm/elements G))
   xs
   (mapv first orbit-pts)
   ys
   (mapv second orbit-pts)]
  (count (set orbit-pts))))


(deftest t41_l308 (is (= v40_l300 14)))
