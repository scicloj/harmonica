;; # Symmetry Sketchpad — Rosette Patterns
;;
;; Draw a freehand motif. Choose a symmetry group. Watch the group action
;; replicate your drawing into a symmetric pattern. The mess you drew
;; becomes order — and the *kind* of order depends precisely on which group
;; you chose.

(ns harmonica-book.symmetry-sketchpad
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.harmonica.protocols :as p]
   [scicloj.kindly.v4.kind :as kind]))

;; ## Group Actions on the Plane
;;
;; A **[rosette pattern](https://en.wikipedia.org/wiki/Rosette_(design))** has rotational symmetry around a central point.
;; The symmetry group is either:
;;
;; - $C_n$ (cyclic): rotations only — like a pinwheel
;; - $D_n$ (dihedral): rotations + reflections — like a snowflake
;;
;; Each group element acts as a linear transformation on $\mathbb{R}^2$.

(defn rotation-matrix
  "2D rotation matrix for angle theta (radians)."
  [theta]
  [[(Math/cos theta) (- (Math/sin theta))]
   [(Math/sin theta) (Math/cos theta)]])

(defn reflection-matrix
  "2D reflection matrix across the line at angle theta/2 from x-axis."
  [theta]
  [[(Math/cos theta) (Math/sin theta)]
   [(Math/sin theta) (- (Math/cos theta))]])

(defn apply-matrix
  "Apply a 2x2 matrix to a point [x y]."
  [[[a b] [c d]] [x y]]
  [(+ (* a x) (* b y))
   (+ (* c x) (* d y))])

;; The dihedral group $D_n$ acts on points: rotations by multiples of
;; $2\pi/n$, and reflections across $n$ axes.

(defn dihedral-action
  "Action of D_n on a 2D point."
  [n [tag k] [x y]]
  (let [angle (* 2.0 Math/PI (/ (double k) (double n)))]
    (case tag
      :r (apply-matrix (rotation-matrix angle) [x y])
      :s (apply-matrix (reflection-matrix angle) [x y]))))

(defn cyclic-action
  "Action of C_n on a 2D point."
  [n g [x y]]
  (let [angle (* 2.0 Math/PI (/ (double g) (double n)))]
    (apply-matrix (rotation-matrix angle) [x y])))

;; ## A Simple Motif
;;
;; Let's define a motif — a short path in the fundamental domain
;; (a wedge of the circle).

(def simple-motif
  "A leaf-like curve in the first wedge."
  (let [steps 30]
    (mapv (fn [i]
            (let [t (/ (double i) steps)
                  r (+ 0.3 (* 0.5 t))
                  angle (* t 0.9)]
              [(* r (Math/cos angle))
               (* r (Math/sin angle))]))
          (range (inc steps)))))

;; ## Generating a Rosette
;;
;; To create a rosette, we apply every group element to every point of
;; the motif.

(defn make-rosette-cn
  "Replicate a motif under C_n."
  [n motif]
  (mapv (fn [g]
          (mapv (fn [pt] (cyclic-action n g pt)) motif))
        (range n)))

(defn make-rosette-dn
  "Replicate a motif under D_n."
  [n motif]
  (let [G (hm/dihedral-group n)]
    (mapv (fn [g]
            (mapv (fn [pt] (dihedral-action n g pt)) motif))
          (hm/elements G))))

;; ## $C_5$: Rotational Symmetry Only
;;
;; Like a pinwheel — the motif repeats by rotation but not reflection.

(let [n 5
      copies (make-rosette-cn n simple-motif)
      colors ["#e74c3c" "#3498db" "#2ecc71" "#f39c12" "#9b59b6"]
      traces (mapv (fn [i copy]
                     {:type "scatter" :mode "lines"
                      :x (mapv first copy)
                      :y (mapv second copy)
                      :line {:color (colors i) :width 2}
                      :showlegend false})
                   (range) copies)]
  (kind/plotly
   {:data traces
    :layout {:title (str "C₅ rosette — rotational symmetry")
             :xaxis {:visible false :scaleanchor "y"}
             :yaxis {:visible false}
             :width 400 :height 400}}))

;; ## $D_5$: Rotational + Reflective Symmetry
;;
;; Like a snowflake or a flower — the motif repeats by rotation AND
;; by reflection. Notice there are twice as many copies (10 = 2×5).

(let [n 5
      copies (make-rosette-dn n simple-motif)
      colors (cycle ["#e74c3c" "#3498db" "#2ecc71" "#f39c12" "#9b59b6"
                     "#e67e22" "#1abc9c" "#8e44ad" "#c0392b" "#2980b9"])
      traces (mapv (fn [i copy]
                     {:type "scatter" :mode "lines"
                      :x (mapv first copy)
                      :y (mapv second copy)
                      :line {:color (nth colors i) :width 2}
                      :showlegend false})
                   (range) copies)]
  (kind/plotly
   {:data traces
    :layout {:title (str "D₅ rosette — rotational + reflective symmetry")
             :xaxis {:visible false :scaleanchor "y"}
             :yaxis {:visible false}
             :width 400 :height 400}}))

;; ## Comparing Symmetry Orders
;;
;; The visual complexity increases with the order of the group.
;; Here are rosettes for $D_3$ through $D_8$:

(let [plots
      (mapv (fn [n]
              (let [copies (make-rosette-dn n simple-motif)
                    colors (cycle ["#e74c3c" "#3498db" "#2ecc71" "#f39c12"
                                   "#9b59b6" "#e67e22" "#1abc9c" "#8e44ad"
                                   "#c0392b" "#2980b9" "#27ae60" "#d35400"
                                   "#2c3e50" "#f1c40f" "#7f8c8d" "#16a085"])
                    traces (mapv (fn [i copy]
                                   {:type "scatter" :mode "lines"
                                    :x (mapv first copy)
                                    :y (mapv second copy)
                                    :line {:color (nth colors i) :width 1.5}
                                    :showlegend false})
                                 (range) copies)]
                (kind/plotly
                 {:data traces
                  :layout {:title (str "D" n " (" (* 2 n) " elements)")
                           :xaxis {:visible false :scaleanchor "y"}
                           :yaxis {:visible false}
                           :width 300 :height 300
                           :margin {:t 40 :b 10 :l 10 :r 10}}})))
            [3 4 5 6 7 8])]
  (kind/fragment plots))

;; ## Verifying the Action
;;
;; The dihedral action is a genuine group homomorphism: applying $g$ then
;; $h$ is the same as applying $g \cdot h$.

(let [results
      (for [n [3 4 5 6 7 8]]
        (let [G (hm/dihedral-group n)
              test-pt [0.5 0.3]
              elts (vec (hm/elements G))]
          (every? (fn [[g h]]
                    (let [gh (hm/op G g h)
                          via-compose (dihedral-action n g (dihedral-action n h test-pt))
                          via-product (dihedral-action n gh test-pt)
                          err (Math/sqrt (+ (Math/pow (- (first via-compose) (first via-product)) 2)
                                           (Math/pow (- (second via-compose) (second via-product)) 2)))]
                      (< err 1e-10)))
                  (for [a elts b elts] [a b]))))]
  (every? identity results))

(kind/test-last [true?])

;; ## Cyclic vs Dihedral: The Difference
;;
;; With a **non-symmetric motif**, the difference between $C_n$ and $D_n$
;; is stark. $C_n$ preserves the "handedness" of the motif (all copies
;; spin the same way), while $D_n$ includes mirror images.

(def asymmetric-motif
  "A clearly asymmetric hook shape."
  (let [steps 20]
    (concat
     ;; Outward arc
     (mapv (fn [i]
             (let [t (/ (double i) steps)
                   r (+ 0.3 (* 0.4 t))
                   angle (* t 0.7)]
               [(* r (Math/cos angle))
                (* r (Math/sin angle))]))
           (range (inc steps)))
     ;; Hook at the end
     (mapv (fn [i]
             (let [t (/ (double i) 8)
                   base-r 0.7
                   base-angle 0.7
                   hook-r (- base-r (* 0.15 t))
                   hook-angle (+ base-angle (* 0.4 t))]
               [(* hook-r (Math/cos hook-angle))
                (* hook-r (Math/sin hook-angle))]))
           (range 1 9)))))

(let [n 6
      cn-copies (make-rosette-cn n asymmetric-motif)
      dn-copies (make-rosette-dn n asymmetric-motif)
      make-traces (fn [copies colors]
                    (mapv (fn [i copy]
                            {:type "scatter" :mode "lines"
                             :x (mapv first copy)
                             :y (mapv second copy)
                             :line {:color (nth colors i) :width 2}
                             :showlegend false})
                          (range) copies))
      colors-6 (cycle ["#e74c3c" "#3498db" "#2ecc71" "#f39c12" "#9b59b6" "#e67e22"
                        "#c0392b" "#2980b9" "#27ae60" "#d35400" "#8e44ad" "#1abc9c"])]
  (kind/fragment
   [(kind/plotly
     {:data (make-traces cn-copies colors-6)
      :layout {:title "C₆ — all copies have same handedness"
               :xaxis {:visible false :scaleanchor "y"}
               :yaxis {:visible false}
               :width 350 :height 350
               :margin {:t 40 :b 10 :l 10 :r 10}}})
    (kind/plotly
     {:data (make-traces dn-copies colors-6)
      :layout {:title "D₆ — includes mirror images"
               :xaxis {:visible false :scaleanchor "y"}
               :yaxis {:visible false}
               :width 350 :height 350
               :margin {:t 40 :b 10 :l 10 :r 10}}})]))

;; The $C_6$ rosette is a pinwheel — all hooks curl the same direction.
;; The $D_6$ rosette is a snowflake — alternating hooks curl opposite ways.

;; ## Orbit of a Point
;;
;; The orbit of a single point under the group action traces out the
;; vertices of a regular polygon (for $C_n$) or a doubled polygon (for $D_n$).

(let [n 7
      pt [0.6 0.2]
      G (hm/dihedral-group n)
      orbit-pts (mapv (fn [g] (dihedral-action n g pt)) (hm/elements G))
      xs (mapv first orbit-pts)
      ys (mapv second orbit-pts)]
  (count (set orbit-pts)))

(kind/test-last [= 14])

;; The orbit has $|D_7| = 14$ distinct points (the point has trivial stabilizer).

;; ## Summary
;;
;; This notebook demonstrated:
;;
;; - **2D group actions**: rotation and reflection matrices
;; - **Rosette patterns**: replicating a motif under $C_n$ or $D_n$
;; - **Visual difference**: $C_n$ preserves handedness, $D_n$ doesn't
;; - **Homomorphism verification**: the action respects group composition
;; - **Symmetry order**: higher $n$ creates more intricate patterns
