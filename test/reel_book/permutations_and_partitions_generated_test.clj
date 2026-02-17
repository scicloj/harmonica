(ns
 reel-book.permutations-and-partitions-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l28
 (let
  [G (reel/symmetric-group 5) elts (vec (reel/elements G))]
  (every?
   (fn
    [[s t]]
    (= (reel/sign (reel/op G s t)) (* (reel/sign s) (reel/sign t))))
   (for [a elts b elts] [a b]))))


(deftest t4_l35 (is (true? v3_l28)))


(def
 v6_l42
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


(deftest t7_l53 (is (true? v6_l42)))


(def
 v9_l61
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


(deftest t10_l76 (is (true? v9_l61)))


(def
 v12_l83
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


(deftest t13_l92 (is (true? v12_l83)))


(def
 v15_l99
 (let
  [G (reel/symmetric-group 4) elts (vec (reel/elements G))]
  (every?
   (fn
    [[a b c]]
    (= (reel/op G (reel/op G a b) c) (reel/op G a (reel/op G b c))))
   (for [a elts b elts c elts] [a b c]))))


(deftest t16_l106 (is (true? v15_l99)))


(def
 v18_l113
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
 v20_l125
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


(deftest t21_l133 (is (true? v20_l125)))


(def v23_l143 (reel/partitions 1))


(deftest t24_l145 (is (= v23_l143 [[1]])))


(def v25_l147 (reel/partitions 4))


(deftest t26_l149 (is (= v25_l147 [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]])))


(def v27_l151 (reel/partitions 5))


(deftest
 t28_l153
 (is
  (= v27_l151 [[5] [4 1] [3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]])))


(def
 v30_l157
 (kind/table
  {:column-names ["$n$" "$p(n)$"],
   :row-vectors
   (mapv (fn [n] [n (count (reel/partitions n))]) (range 1 16))}))


(def
 v32_l163
 (let
  [results
   (for
    [n (range 1 13)]
    (every?
     (fn [p] (and (every? pos-int? p) (apply >= p) (= n (reduce + p))))
     (reel/partitions n)))]
  (every? true? results)))


(deftest t33_l172 (is (true? v32_l163)))


(def
 v35_l179
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
 v37_l202
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


(def v39_l216 (reel/partition-conjugate [4 2 1]))


(deftest t40_l218 (is (= v39_l216 [3 2 1 1])))


(def v41_l220 (reel/partition-conjugate [3 3]))


(deftest t42_l222 (is (= v41_l220 [2 2 2])))


(def v43_l224 (reel/partition-conjugate [5]))


(deftest t44_l226 (is (= v43_l224 [1 1 1 1 1])))


(def
 v46_l230
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
 v48_l244
 (let
  [results
   (for
    [n (range 1 11) p (reel/partitions n)]
    (= p (reel/partition-conjugate (reel/partition-conjugate p))))]
  (every? true? results)))


(deftest t49_l250 (is (true? v48_l244)))


(def
 v51_l254
 (let
  [results
   (for
    [n (range 1 11) p (reel/partitions n)]
    (= (reduce + p) (reduce + (reel/partition-conjugate p))))]
  (every? true? results)))


(deftest t52_l260 (is (true? v51_l254)))


(def
 v54_l276
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
 v56_l288
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


(def v58_l322 (kind/hiccup (young-hooks-svg [4 2 1])))


(def v60_l326 (kind/hiccup (young-hooks-svg [3 2 2])))


(def v62_l333 (defn factorial [n] (reduce *' (range 1 (inc n)))))


(def
 v63_l335
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


(deftest t64_l347 (is (true? v63_l335)))


(def
 v66_l351
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
 v68_l368
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
       (map (fn* [p1__71968#] (* p1__71968# p1__71968#)) dims))]
     (= sum-sq (factorial n))))]
  (every? true? results)))


(deftest t69_l375 (is (true? v68_l368)))


(def v71_l384 (reel/standard-young-tableaux [2 1]))


(deftest t72_l386 (is (= v71_l384 [[[1 2] [3]] [[1 3] [2]]])))


(def v73_l388 (count (reel/standard-young-tableaux [3 2])))


(deftest t74_l390 (is (= v73_l388 5)))


(def
 v76_l394
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
 v77_l424
 (kind/hiccup
  (into
   [:div
    {:style
     "display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-start;"}]
   (for
    [t (reel/standard-young-tableaux [3 2])]
    [:div {:style "text-align: center;"} (syt-svg t)]))))


(def
 v79_l438
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
 v81_l451
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


(deftest t82_l465 (is (true? v81_l451)))


(def
 v84_l469
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
 v86_l485
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


(def v88_l536 (kind/hiccup (cycle-diagram-svg [1 2 3 0])))


(def v90_l540 (kind/hiccup (cycle-diagram-svg [1 0 3 2])))


(def v92_l544 (kind/hiccup (cycle-diagram-svg [2 3 4 1 0 5])))
