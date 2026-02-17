(ns scicloj.reel.svg
  "SVG visualization functions for group theory objects.

  Provides Hiccup-style SVG generators for:
  - Young diagrams (partitions)
  - Hook-length diagrams
  - Standard Young tableaux
  - Permutation cycle diagrams
  - Cayley tables"
  (:require [scicloj.reel.protocols :as p]
            [scicloj.reel.impl.partition :as part]))

(defn young-diagram-svg
  "Render a partition as an SVG Young diagram.

  Options:
    :cell-size — pixel size of each cell (default 28)
    :fill — cell fill color (default \"#4a90d9\")
    :stroke — cell stroke color (default \"#2c3e50\")"
  [lambda & {:keys [cell-size fill stroke]
             :or {cell-size 28 fill "#4a90d9" stroke "#2c3e50"}}]
  (let [rows (count lambda)
        max-cols (if (seq lambda) (first lambda) 0)
        w (+ (* max-cols cell-size) 2)
        h (+ (* rows cell-size) 2)]
    (into [:svg {:width w :height h
                 :xmlns "http://www.w3.org/2000/svg"}]
          (for [r (range rows)
                c (range (nth lambda r))]
            [:rect {:x (+ 1 (* c cell-size))
                    :y (+ 1 (* r cell-size))
                    :width (dec cell-size)
                    :height (dec cell-size)
                    :fill fill
                    :stroke stroke
                    :stroke-width 1.5
                    :rx 2}]))))

(defn young-hooks-svg
  "Render a Young diagram with hook lengths displayed in each cell.

  Options:
    :cell-size — pixel size of each cell (default 36)"
  [lambda & {:keys [cell-size] :or {cell-size 36}}]
  (let [conj (part/conjugate lambda)
        rows (count lambda)
        max-cols (first lambda)
        w (+ (* max-cols cell-size) 2)
        h (+ (* rows cell-size) 2)]
    (into [:svg {:width w :height h
                 :xmlns "http://www.w3.org/2000/svg"}]
          (for [r (range rows)
                c (range (nth lambda r))
                :let [hook (+ (- (nth lambda r) c)
                              (- (nth conj c) r)
                              -1)]]
            [:g
             [:rect {:x (+ 1 (* c cell-size))
                     :y (+ 1 (* r cell-size))
                     :width (dec cell-size)
                     :height (dec cell-size)
                     :fill "#ecf0f1"
                     :stroke "#2c3e50"
                     :stroke-width 1.5
                     :rx 2}]
             [:text {:x (+ 1 (* c cell-size) (/ cell-size 2))
                     :y (+ 1 (* r cell-size) (/ cell-size 2) 5)
                     :text-anchor "middle"
                     :font-family "monospace"
                     :font-size 14
                     :fill "#2c3e50"}
              (str hook)]]))))

(defn syt-svg
  "Render a standard Young tableau as SVG with numbers in cells.

  Options:
    :cell-size — pixel size of each cell (default 32)"
  [syt & {:keys [cell-size] :or {cell-size 32}}]
  (let [rows (count syt)
        max-cols (apply max (map count syt))
        w (+ (* max-cols cell-size) 2)
        h (+ (* rows cell-size) 2)]
    (into [:svg {:width w :height h
                 :xmlns "http://www.w3.org/2000/svg"}]
          (for [r (range rows)
                c (range (count (nth syt r)))
                :let [val (nth (nth syt r) c)]]
            [:g
             [:rect {:x (+ 1 (* c cell-size))
                     :y (+ 1 (* r cell-size))
                     :width (dec cell-size)
                     :height (dec cell-size)
                     :fill "#d5e8d4"
                     :stroke "#2c3e50"
                     :stroke-width 1.5
                     :rx 2}]
             [:text {:x (+ 1 (* c cell-size) (/ cell-size 2))
                     :y (+ 1 (* r cell-size) (/ cell-size 2) 5)
                     :text-anchor "middle"
                     :font-family "monospace"
                     :font-size 14
                     :font-weight "bold"
                     :fill "#2c3e50"}
              (str val)]]))))

(defn cycle-diagram-svg
  "Render a permutation as a cycle diagram SVG.

  Draws elements around a circle with arrows showing where each maps.
  Fixed points are shown in grey.

  Options:
    :radius — circle radius in pixels (default 80)"
  [sigma & {:keys [radius] :or {radius 80}}]
  (let [n (count sigma)
        cx (+ radius 30)
        cy (+ radius 30)
        w (* 2 (+ radius 30))
        h (* 2 (+ radius 30))
        angle (fn [i] (- (* 2 Math/PI (/ i (double n))) (/ Math/PI 2)))
        px (fn [i] (+ cx (* radius (Math/cos (angle i)))))
        py (fn [i] (+ cy (* radius (Math/sin (angle i)))))
        node-r 14]
    (into
     [:svg {:width w :height h :xmlns "http://www.w3.org/2000/svg"}
      [:defs
       [:marker {:id "arrowhead" :markerWidth 8 :markerHeight 6
                 :refX 7 :refY 3 :orient "auto"}
        [:polygon {:points "0 0, 8 3, 0 6" :fill "#e74c3c"}]]]]
     (concat
      ;; Arrows for non-fixed points
      (for [i (range n)
            :when (not= i (sigma i))
            :let [j (sigma i)
                  x1 (px i) y1 (py i)
                  x2 (px j) y2 (py j)
                  dx (- x2 x1) dy (- y2 y1)
                  dist (Math/sqrt (+ (* dx dx) (* dy dy)))
                  ux (/ dx dist) uy (/ dy dist)
                  sx (+ x1 (* ux (+ node-r 2)))
                  sy (+ y1 (* uy (+ node-r 2)))
                  ex (- x2 (* ux (+ node-r 4)))
                  ey (- y2 (* uy (+ node-r 4)))]]
        [:line {:x1 sx :y1 sy :x2 ex :y2 ey
                :stroke "#e74c3c" :stroke-width 1.5
                :marker-end "url(#arrowhead)"}])
      ;; Nodes
      (for [i (range n)
            :let [fixed? (= i (sigma i))]]
        [:g
         [:circle {:cx (px i) :cy (py i) :r node-r
                   :fill (if fixed? "#bdc3c7" "#3498db")
                   :stroke "#2c3e50" :stroke-width 1.5}]
         [:text {:x (px i) :y (+ (py i) 5)
                 :text-anchor "middle"
                 :font-family "monospace" :font-size 13
                 :font-weight "bold" :fill "white"}
          (str i)]])))))

(defn cayley-table-svg
  "Render a Cayley table as an SVG grid with colored cells.

  Each group element gets a unique hue. The cell at row i, column j
  is colored by the product g_i * g_j.

  Options:
    :cell-size — pixel size of each cell (default 28)"
  [G & {:keys [cell-size] :or {cell-size 28}}]
  (let [elts (vec (p/elements G))
        n (count elts)
        elt-idx (into {} (map-indexed (fn [i e] [e i]) elts))
        colors (mapv (fn [i]
                       (let [hue (* 360.0 (/ i (double n)))]
                         (str "hsl(" (int hue) ",70%,75%)")))
                     (range n))
        header cell-size
        w (+ header (* n cell-size) 2)
        h (+ header (* n cell-size) 2)]
    (into [:svg {:width w :height h :xmlns "http://www.w3.org/2000/svg"
                 :style "font-family: monospace; font-size: 11px;"}]
          (concat
           ;; Column headers
           (for [j (range n)]
             [:text {:x (+ header (* j cell-size) (/ cell-size 2))
                     :y (- header 4)
                     :text-anchor "middle" :font-size 9 :fill "#555"}
              (str (elts j))])
           ;; Row headers
           (for [i (range n)]
             [:text {:x (- header 4)
                     :y (+ header (* i cell-size) (/ cell-size 2) 4)
                     :text-anchor "end" :font-size 9 :fill "#555"}
              (str (elts i))])
           ;; Table cells
           (for [i (range n) j (range n)
                 :let [prod (p/op G (elts i) (elts j))
                       k (elt-idx prod)]]
             [:rect {:x (+ header (* j cell-size))
                     :y (+ header (* i cell-size))
                     :width (dec cell-size) :height (dec cell-size)
                     :fill (colors k) :stroke "#fff" :stroke-width 0.5}])))))
