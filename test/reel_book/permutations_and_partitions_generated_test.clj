(ns
 reel-book.permutations-and-partitions-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def v3_l24 (reel/identity-perm 5))


(deftest t4_l26 (is (= v3_l24 [0 1 2 3 4])))


(def v5_l28 (reel/transposition 5 1 3))


(deftest t6_l30 (is (= v5_l28 [0 3 2 1 4])))


(def
 v8_l34
 (let [G (reel/symmetric-group 4)] (reel/op G [1 0 3 2] [2 3 0 1])))


(deftest t9_l37 (is (= v8_l34 [3 2 1 0])))


(def v11_l44 (reel/cycles [1 2 3 0]))


(deftest t12_l46 (is (= v11_l44 [[0 1 2 3]])))


(def v13_l48 (reel/cycles [0 3 2 1]))


(deftest t14_l50 (is (= v13_l48 [[1 3]])))


(def v15_l52 (reel/cycles [1 0 3 2]))


(deftest t16_l54 (is (= v15_l52 [[0 1] [2 3]])))


(def v18_l58 (reel/cycles [0 1 2 3]))


(deftest t19_l60 (is (= v18_l58 [])))


(def v21_l67 (reel/cycle-type [1 2 3 0]))


(deftest t22_l69 (is (= v21_l67 [4])))


(def v23_l71 (reel/cycle-type [1 0 3 2]))


(deftest t24_l73 (is (= v23_l71 [2 2])))


(def v25_l75 (reel/cycle-type [1 0 2 3]))


(deftest t26_l77 (is (= v25_l75 [2 1 1])))


(def v27_l79 (reel/cycle-type [0 1 2 3]))


(deftest t28_l81 (is (= v27_l79 [1 1 1 1])))


(def v30_l88 (reel/sign [0 1 2 3]))


(deftest t31_l90 (is (= v30_l88 1)))


(def v32_l92 (reel/sign [1 0 2 3]))


(deftest t33_l94 (is (= v32_l92 -1)))


(def v34_l96 (reel/sign [1 2 3 0]))


(deftest t35_l98 (is (= v34_l96 -1)))


(def
 v37_l104
 (let
  [G (reel/symmetric-group 5) elts (vec (reel/elements G))]
  (every?
   (fn
    [[s t]]
    (= (reel/sign (reel/op G s t)) (* (reel/sign s) (reel/sign t))))
   (for [a elts b elts] [a b]))))


(deftest t38_l111 (is (true? v37_l104)))


(def
 v40_l118
 (let
  [results
   (for
    [n (range 1 7)]
    (let
     [G (reel/symmetric-group n) e (reel/id G)]
     (every?
      (fn
       [sigma]
       (let
        [si (reel/inv G sigma)]
        (and (= (reel/op G sigma si) e) (= (reel/op G si sigma) e))))
      (reel/elements G))))]
  (every? true? results)))


(deftest t41_l129 (is (true? v40_l118)))


(def
 v43_l137
 (let
  [results
   (for
    [n (range 1 7)]
    (let
     [G
      (reel/symmetric-group n)
      id-perm
      (reel/id G)
      make-swap
      (fn [i] (let [v (vec (range n))] (assoc v i (inc i) (inc i) i)))]
     (every?
      (fn
       [sigma]
       (let
        [swaps
         (reel/adjacent-transposition-decomposition sigma)
         reconstructed
         (reduce (fn [p i] (reel/op G p (make-swap i))) id-perm swaps)]
        (= sigma reconstructed)))
      (reel/elements G))))]
  (every? true? results)))


(deftest t44_l152 (is (true? v43_l137)))


(def
 v46_l159
 (let
  [results
   (for
    [n (range 3 7)]
    (let
     [G (reel/symmetric-group n) classes (reel/conjugacy-classes G)]
     (every?
      (fn
       [cls]
       (= 1 (count (set (map reel/cycle-type (:elements cls))))))
      classes)))]
  (every? true? results)))


(deftest t47_l168 (is (true? v46_l159)))


(def
 v49_l175
 (let
  [G (reel/symmetric-group 4) elts (vec (reel/elements G))]
  (every?
   (fn
    [[a b c]]
    (= (reel/op G (reel/op G a b) c) (reel/op G a (reel/op G b c))))
   (for [a elts b elts c elts] [a b c]))))


(deftest t50_l182 (is (true? v49_l175)))


(def
 v52_l189
 (defn
  perm-order
  "Compute the order of a permutation by repeated composition."
  [G sigma]
  (let
   [e (reel/id G)]
   (loop
    [k 1 current sigma]
    (if (= current e) k (recur (inc k) (reel/op G current sigma)))))))


(def
 v54_l201
 (let
  [G (reel/symmetric-group 5)]
  (every?
   (fn
    [sigma]
    (let
     [ct
      (reel/cycle-type sigma)
      expected
      (reduce
       (fn
        [a b]
        (/ (* a b) (biginteger (.gcd (biginteger a) (biginteger b)))))
       (map biginteger ct))]
     (= (perm-order G sigma) (long expected))))
   (reel/elements G))))


(deftest t55_l209 (is (true? v54_l201)))


(def v57_l219 (reel/partitions 1))


(deftest t58_l221 (is (= v57_l219 [[1]])))


(def v59_l223 (reel/partitions 4))


(deftest t60_l225 (is (= v59_l223 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v61_l227 (reel/partitions 5))


(deftest
 t62_l229
 (is
  (= v61_l227 [[5] [4 1] [3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]])))


(def
 v64_l233
 (kind/table
  {:column-names ["$n$" "$p(n)$"],
   :row-vectors
   (mapv (fn [n] [n (count (reel/partitions n))]) (range 1 16))}))


(def
 v66_l239
 (let
  [results
   (for
    [n (range 1 13)]
    (every?
     (fn [p] (and (every? pos-int? p) (apply >= p) (= n (reduce + p))))
     (reel/partitions n)))]
  (every? true? results)))


(deftest t67_l248 (is (true? v66_l239)))


(def
 v69_l255
 (defn
  young-diagram-svg
  "Render a partition as an SVG Young diagram."
  [lambda
   &
   {:keys [cell-size fill stroke],
    :or {cell-size 28, fill "#4a90d9", stroke "#2c3e50"}}]
  (let
   [rows
    (count lambda)
    max-cols
    (if (seq lambda) (first lambda) 0)
    w
    (+ (* max-cols cell-size) 2)
    h
    (+ (* rows cell-size) 2)]
   (into
    [:svg {:width w, :height h, :xmlns "http://www.w3.org/2000/svg"}]
    (for
     [r (range rows) c (range (nth lambda r))]
     [:rect
      {:x (+ 1 (* c cell-size)),
       :y (+ 1 (* r cell-size)),
       :width (dec cell-size),
       :height (dec cell-size),
       :fill fill,
       :stroke stroke,
       :stroke-width 1.5,
       :rx 2}])))))


(def
 v71_l278
 (kind/hiccup
  (into
   [:div
    {:style
     "display: flex; flex-wrap: wrap; gap: 24px; align-items: flex-end;"}]
   (for
    [p (reel/partitions 5)]
    [:div
     {:style "text-align: center;"}
     (young-diagram-svg p)
     [:div
      {:style
       "margin-top: 4px; font-family: monospace; font-size: 13px;"}
      (str p)]]))))


(def v73_l292 (reel/partition-conjugate [4 2 1]))


(deftest t74_l294 (is (= v73_l292 [3 2 1 1])))


(def v75_l296 (reel/partition-conjugate [3 3]))


(deftest t76_l298 (is (= v75_l296 [2 2 2])))


(def v77_l300 (reel/partition-conjugate [5]))


(deftest t78_l302 (is (= v77_l300 [1 1 1 1 1])))


(def
 v80_l306
 (kind/hiccup
  (let
   [p [4 2 1] pc (reel/partition-conjugate p)]
   [:div
    {:style "display: flex; gap: 40px; align-items: flex-end;"}
    [:div
     {:style "text-align: center;"}
     (young-diagram-svg p)
     [:div
      {:style "margin-top: 4px; font-family: monospace;"}
      (str "λ = " p)]]
    [:div
     {:style
      "text-align: center; font-size: 24px; align-self: center;"}
     "↔"]
    [:div
     {:style "text-align: center;"}
     (young-diagram-svg pc :fill "#e67e22")
     [:div
      {:style "margin-top: 4px; font-family: monospace;"}
      (str "λ' = " pc)]]])))


(def
 v82_l320
 (let
  [results
   (for
    [n (range 1 11) p (reel/partitions n)]
    (= p (reel/partition-conjugate (reel/partition-conjugate p))))]
  (every? true? results)))


(deftest t83_l326 (is (true? v82_l320)))


(def
 v85_l330
 (let
  [results
   (for
    [n (range 1 11) p (reel/partitions n)]
    (= (reduce + p) (reduce + (reel/partition-conjugate p))))]
  (every? true? results)))


(deftest t86_l336 (is (true? v85_l330)))


(def
 v88_l352
 (defn
  hook-lengths
  "Compute all hook lengths for a partition."
  [lambda]
  (let
   [conj (reel/partition-conjugate lambda)]
   (for
    [i (range (count lambda)) j (range (nth lambda i))]
    (+ (- (nth lambda i) j) (- (nth conj j) i) -1)))))


(def
 v90_l364
 (defn
  young-hooks-svg
  "Render a Young diagram with hook lengths displayed in each cell."
  [lambda & {:keys [cell-size], :or {cell-size 36}}]
  (let
   [conj
    (reel/partition-conjugate lambda)
    rows
    (count lambda)
    max-cols
    (first lambda)
    w
    (+ (* max-cols cell-size) 2)
    h
    (+ (* rows cell-size) 2)]
   (into
    [:svg {:width w, :height h, :xmlns "http://www.w3.org/2000/svg"}]
    (for
     [r
      (range rows)
      c
      (range (nth lambda r))
      :let
      [hook (+ (- (nth lambda r) c) (- (nth conj c) r) -1)]]
     [:g
      [:rect
       {:x (+ 1 (* c cell-size)),
        :y (+ 1 (* r cell-size)),
        :width (dec cell-size),
        :height (dec cell-size),
        :fill "#ecf0f1",
        :stroke "#2c3e50",
        :stroke-width 1.5,
        :rx 2}]
      [:text
       {:x (+ 1 (* c cell-size) (/ cell-size 2)),
        :y (+ 1 (* r cell-size) (/ cell-size 2) 5),
        :text-anchor "middle",
        :font-family "monospace",
        :font-size 14,
        :fill "#2c3e50"}
       (str hook)]])))))


(def v92_l398 (kind/hiccup (young-hooks-svg [4 2 1])))


(def v94_l402 (kind/hiccup (young-hooks-svg [3 2 2])))


(def v96_l409 (defn factorial [n] (reduce *' (range 1 (inc n)))))


(def
 v97_l411
 (let
  [results
   (for
    [n (range 1 8) lambda (reel/partitions n)]
    (let
     [hooks
      (hook-lengths lambda)
      hook-product
      (reduce *' hooks)
      formula-dim
      (/ (factorial n) hook-product)
      enum-dim
      (count (reel/standard-young-tableaux lambda))
      lib-dim
      (reel/hook-length-dimension lambda)]
     (and (= formula-dim enum-dim) (= formula-dim lib-dim))))]
  (every? true? results)))


(deftest t98_l423 (is (true? v97_l411)))


(def
 v100_l427
 (kind/table
  {:column-names
   ["Partition λ"
    "Hook product"
    "f^λ = 5!/hooks"
    "# SYT (enumerated)"],
   :row-vectors
   (mapv
    (fn
     [lambda]
     (let
      [hooks
       (hook-lengths lambda)
       hp
       (reduce *' hooks)
       dim
       (/ (factorial 5) hp)
       enum
       (count (reel/standard-young-tableaux lambda))]
      [(str lambda) (str hp) (str dim) (str enum)]))
    (reel/partitions 5))}))


(def
 v102_l444
 (let
  [results
   (for
    [n (range 1 9)]
    (let
     [dims
      (map reel/hook-length-dimension (reel/partitions n))
      sum-sq
      (reduce
       +
       (map (fn* [p1__79611#] (* p1__79611# p1__79611#)) dims))]
     (= sum-sq (factorial n))))]
  (every? true? results)))


(deftest t103_l451 (is (true? v102_l444)))


(def v105_l460 (reel/standard-young-tableaux [2 1]))


(deftest t106_l462 (is (= v105_l460 [[[1 2] [3]] [[1 3] [2]]])))


(def v107_l464 (count (reel/standard-young-tableaux [3 2])))


(deftest t108_l466 (is (= v107_l464 5)))


(def
 v110_l470
 (defn
  syt-svg
  "Render a standard Young tableau as SVG with numbers in cells."
  [syt & {:keys [cell-size], :or {cell-size 32}}]
  (let
   [rows
    (count syt)
    max-cols
    (apply max (map count syt))
    w
    (+ (* max-cols cell-size) 2)
    h
    (+ (* rows cell-size) 2)]
   (into
    [:svg {:width w, :height h, :xmlns "http://www.w3.org/2000/svg"}]
    (for
     [r
      (range rows)
      c
      (range (count (nth syt r)))
      :let
      [val (nth (nth syt r) c)]]
     [:g
      [:rect
       {:x (+ 1 (* c cell-size)),
        :y (+ 1 (* r cell-size)),
        :width (dec cell-size),
        :height (dec cell-size),
        :fill "#d5e8d4",
        :stroke "#2c3e50",
        :stroke-width 1.5,
        :rx 2}]
      [:text
       {:x (+ 1 (* c cell-size) (/ cell-size 2)),
        :y (+ 1 (* r cell-size) (/ cell-size 2) 5),
        :text-anchor "middle",
        :font-family "monospace",
        :font-size 14,
        :font-weight "bold",
        :fill "#2c3e50"}
       (str val)]])))))


(def
 v111_l500
 (kind/hiccup
  (into
   [:div
    {:style
     "display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start;"}]
   (for
    [t (reel/standard-young-tableaux [3 2])]
    [:div {:style "text-align: center;"} (syt-svg t)]))))


(def
 v113_l514
 (defn
  class-size-formula
  "Compute conjugacy class size from the partition formula."
  [n mu]
  (let
   [freq (frequencies mu)]
   (/
    (factorial n)
    (reduce
     *'
     (map
      (fn [[k ak]] (*' (reduce *' (repeat ak k)) (factorial ak)))
      freq))))))


(def
 v115_l527
 (let
  [results
   (for
    [n (range 2 8)]
    (let
     [G (reel/symmetric-group n) classes (reel/conjugacy-classes G)]
     (and
      (every?
       (fn
        [cls]
        (let
         [ct (reel/cycle-type (:representative cls))]
         (= (:size cls) (class-size-formula n ct))))
       classes)
      (= (reduce + (map :size classes)) (factorial n)))))]
  (every? true? results)))


(deftest t116_l541 (is (true? v115_l527)))


(def
 v118_l545
 (let
  [G (reel/symmetric-group 5) classes (reel/conjugacy-classes G)]
  (kind/table
   {:column-names ["Cycle type" "Class size" "Formula"],
    :row-vectors
    (mapv
     (fn
      [cls]
      (let
       [ct (reel/cycle-type (:representative cls))]
       [(str ct) (:size cls) (class-size-formula 5 ct)]))
     classes)})))


(def
 v120_l561
 (defn
  cycle-diagram-svg
  "Render a permutation as a cycle diagram SVG."
  [sigma & {:keys [radius], :or {radius 80}}]
  (let
   [n
    (count sigma)
    cx
    (+ radius 30)
    cy
    (+ radius 30)
    w
    (* 2 (+ radius 30))
    h
    (* 2 (+ radius 30))
    angle
    (fn [i] (- (* 2 Math/PI (/ i (double n))) (/ Math/PI 2)))
    px
    (fn [i] (+ cx (* radius (Math/cos (angle i)))))
    py
    (fn [i] (+ cy (* radius (Math/sin (angle i)))))
    node-r
    14]
   (into
    [:svg
     {:width w, :height h, :xmlns "http://www.w3.org/2000/svg"}
     [:defs
      [:marker
       {:id "arrowhead",
        :markerWidth 8,
        :markerHeight 6,
        :refX 7,
        :refY 3,
        :orient "auto"}
       [:polygon {:points "0 0, 8 3, 0 6", :fill "#e74c3c"}]]]]
    (concat
     (for
      [i
       (range n)
       :when
       (not= i (sigma i))
       :let
       [j
        (sigma i)
        x1
        (px i)
        y1
        (py i)
        x2
        (px j)
        y2
        (py j)
        dx
        (- x2 x1)
        dy
        (- y2 y1)
        dist
        (Math/sqrt (+ (* dx dx) (* dy dy)))
        ux
        (/ dx dist)
        uy
        (/ dy dist)
        sx
        (+ x1 (* ux (+ node-r 2)))
        sy
        (+ y1 (* uy (+ node-r 2)))
        ex
        (- x2 (* ux (+ node-r 4)))
        ey
        (- y2 (* uy (+ node-r 4)))]]
      [:line
       {:x1 sx,
        :y1 sy,
        :x2 ex,
        :y2 ey,
        :stroke "#e74c3c",
        :stroke-width 1.5,
        :marker-end "url(#arrowhead)"}])
     (for
      [i (range n) :let [fixed? (= i (sigma i))]]
      [:g
       [:circle
        {:cx (px i),
         :cy (py i),
         :r node-r,
         :fill (if fixed? "#bdc3c7" "#3498db"),
         :stroke "#2c3e50",
         :stroke-width 1.5}]
       [:text
        {:x (px i),
         :y (+ (py i) 5),
         :text-anchor "middle",
         :font-family "monospace",
         :font-size 13,
         :font-weight "bold",
         :fill "white"}
        (str i)]]))))))


(def v122_l612 (kind/hiccup (cycle-diagram-svg [1 2 3 0])))


(def v124_l616 (kind/hiccup (cycle-diagram-svg [1 0 3 2])))


(def v126_l620 (kind/hiccup (cycle-diagram-svg [2 3 4 1 0 5])))
