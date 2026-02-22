;; # Symmetry Sketchpad — Rosette Patterns
;;
;; A **[group action](https://en.wikipedia.org/wiki/Group_action)** is
;; a way for a group to transform a set — each group element becomes a
;; transformation, and composing transformations matches group multiplication.
;;
;; Draw a freehand motif. Choose a symmetry group. Watch the group action
;; replicate your drawing into a symmetric pattern. The mess you drew
;; becomes order — and the *kind* of order depends precisely on which group
;; you chose.

(ns harmonica-book.symmetry-sketchpad
  (:require
   [scicloj.harmonica :as hm]
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

;;  Rotation matrices have determinant 1 (distance-preserving):

(let [theta 1.23
      [[a b] [c d]] (rotation-matrix theta)]
  (< (Math/abs (- (- (* a d) (* b c)) 1.0)) 1e-14))

(kind/test-last [true?])

;; Reflection matrices have determinant -1:

(let [theta 0.7
      [[a b] [c d]] (reflection-matrix theta)]
  (< (Math/abs (- (- (* a d) (* b c)) -1.0)) 1e-14))

(kind/test-last [true?])

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

(kind/plotly
 {:data [{:type "scatter" :mode "lines"
          :x (mapv first simple-motif)
          :y (mapv second simple-motif)
          :line {:color "#e74c3c" :width 2}
          :showlegend false}]
  :layout {:title "The motif — one leaf-like curve"
           :xaxis {:visible false :scaleanchor "y"}
           :yaxis {:visible false}
           :width 300 :height 300
           :margin {:t 40 :b 10 :l 10 :r 10}}})

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

;;  $C_n$ produces exactly $n$ copies of the motif:

(count (make-rosette-cn 5 simple-motif))

(kind/test-last [= 5])

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

;;  $D_n$ produces exactly $2n$ copies (rotations + reflections):

(count (make-rosette-dn 5 simple-motif))

(kind/test-last [= 10])

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


;; ## Under the hood: fastmath for 2D linear algebra
;;
;; The rotation and reflection matrices above were built by hand from
;; trigonometric formulas. In practice,
;; [fastmath](https://github.com/generateme/fastmath) provides
;; fixed-size matrix types (`Mat2x2`, `Mat3x3`, `Mat4x4`) with fast,
;; unboxed arithmetic — no allocation overhead per operation.
;;
;; `fastmath.matrix` and `fastmath.vector` give us everything we need.

(require '[fastmath.matrix :as fm]
         '[fastmath.vector :as fv])

;; **Rotation matrix** — built-in:

(fm/rotation-matrix-2d (/ Math/PI 3))

;; **Reflection matrix** — construct with `mat2x2`. Reflection across
;; the line at angle $\theta/2$ has the matrix
;; $\begin{pmatrix}\cos\theta & \sin\theta \\ \sin\theta & -\cos\theta\end{pmatrix}$:

(defn fm-reflection [theta]
  (fm/mat2x2 (Math/cos theta) (Math/sin theta)
             (Math/sin theta) (- (Math/cos theta))))

(fm-reflection (/ Math/PI 3))

;; **Matrix-vector product** — `mulv` applies a transformation to a point:

(fm/mulv (fm/rotation-matrix-2d (/ Math/PI 4)) (fv/vec2 1.0 0.0))

;; The result is the point $(1, 0)$ rotated by $45°$: roughly
;; $(0.707, 0.707)$.

;; **Determinant** — rotations have $\det = 1$, reflections have $\det = -1$:

(fm/det (fm/rotation-matrix-2d 1.23))

(kind/test-last
 [(fn [v] (< (Math/abs (- v 1.0)) 1e-14))])

(fm/det (fm-reflection 0.7))

(kind/test-last
 [(fn [v] (< (Math/abs (- v -1.0)) 1e-14))])

;; **Matrix composition** — `mulm` composes transformations. Two rotations
;; compose to a single rotation:

(let [ab (fm/mulm (fm/rotation-matrix-2d 1.0)
                  (fm/rotation-matrix-2d 0.5))
      direct (fm/rotation-matrix-2d 1.5)]
  (< (fm/norm (fm/sub ab direct)) 1e-14))

(kind/test-last [true?])

;; A rotation followed by a reflection is a reflection:

(let [rs (fm/mulm (fm/rotation-matrix-2d (/ Math/PI 3))
                  (fm-reflection 0.0))]
  (fm/det rs))

(kind/test-last
 [(fn [v] (< (Math/abs (- v -1.0)) 1e-14))])

;; **Inverse** — the inverse of a rotation is the opposite rotation:

(let [r (fm/rotation-matrix-2d 1.0)
      product (fm/mulm r (fm/inverse r))
      identity (fm/mat2x2 1 0 0 1)]
  (< (fm/norm (fm/sub product identity)) 1e-14))

(kind/test-last [true?])

;; These operations are what power the rosette construction: each group
;; element becomes a `Mat2x2`, and `mulv` maps it over the motif points.
;; The fastmath types are drop-in replacements for the nested-vector
;; matrices used earlier in this notebook — same mathematics, better
;; performance.

;; ## Summary
;;
;; This notebook demonstrated:
;;
;; - **2D group actions**: rotation and reflection matrices
;; - **Rosette patterns**: replicating a motif under $C_n$ or $D_n$
;; - **Visual difference**: $C_n$ preserves handedness, $D_n$ doesn't
;; - **Homomorphism verification**: the action respects group composition
;; - **Symmetry order**: higher $n$ creates more intricate patterns

;; For the algebraic theory behind group actions, see
;; [Group Actions](group_actions.html).
;;
;; For orbit counting using the same dihedral groups, see
;; [Counting Necklaces](counting_necklaces.html).

