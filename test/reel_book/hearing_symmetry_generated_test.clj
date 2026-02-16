(ns
 reel-book.hearing-symmetry-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.reel.protocols :as p]
  [scicloj.kindly.v4.kind :as kind]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [clojure.test :refer [deftest is]]))


(def
 v3_l30
 (def
  V4
  (reel/product-group (reel/cyclic-group 2) (reel/cyclic-group 2))))


(def v4_l32 (reel/order V4))


(deftest t5_l34 (is (fn v4_l32 [v] (= 4 v))))


(def v7_l38 (vec (reel/elements V4)))


(deftest t8_l40 (is (fn v7_l38 [v] (= 4 (count v)))))


(def v10_l56 (def motif [67 67 67 63]))


(def
 v11_l58
 (def
  note-names
  {65 "F",
   70 "Bb",
   62 "D",
   72 "C'",
   60 "C",
   69 "A",
   61 "C#",
   64 "E",
   66 "F#",
   68 "G#",
   67 "G",
   71 "B",
   63 "Eb"}))


(def
 v13_l66
 (defn
  apply-v4
  "Apply a Klein four-group element to a melody.\n   Inversion reflects around the first note."
  [[r i] melody]
  (let
   [pivot
    (first melody)
    inverted
    (if
     (= i 1)
     (mapv (fn* [p1__68164#] (- (* 2 pivot) p1__68164#)) melody)
     melody)
    retrograded
    (if (= r 1) (vec (reverse inverted)) inverted)]
   retrograded)))


(def
 v14_l79
 (def
  v4-labels
  {[0 0] "Original",
   [1 0] "Retrograde",
   [0 1] "Inversion",
   [1 1] "Retrograde Inversion"}))


(def
 v15_l85
 (let
  [rows
   (mapv
    (fn
     [g]
     (let
      [result (apply-v4 g motif)]
      {:transform (v4-labels g),
       :element (str g),
       :melody (str result),
       :notes
       (str
        (mapv
         (fn*
          [p1__68165#]
          (get note-names p1__68165# (str p1__68165#)))
         result))}))
    (reel/elements V4))]
  (kind/table
   {:column-names ["Transform" "Element" "MIDI" "Notes"],
    :row-vectors
    (mapv
     (fn
      [{:keys [transform element melody notes]}]
      [transform element melody notes])
     rows)})))


(def
 v17_l103
 (every?
  (fn
   [[g h]]
   (let
    [gh-melody
     (apply-v4 g (apply-v4 h motif))
     direct
     (apply-v4 (reel/op V4 g h) motif)]
    (= gh-melody direct)))
  (for [g (reel/elements V4) h (reel/elements V4)] [g h])))


(deftest t18_l109 (is (fn v17_l103 [v] (= true v))))


(def
 v20_l117
 (let
  [transforms
   (mapv
    (fn
     [g]
     (let
      [result (apply-v4 g motif)]
      {:element g, :label (v4-labels g), :melody result}))
    [[0 0] [1 0] [0 1] [1 1]])
   data
   (vec
    (mapcat
     (fn
      [{:keys [label melody]}]
      (map-indexed
       (fn [i pitch] {:time i, :pitch pitch, :transform label})
       melody))
     transforms))]
  (->
   (tc/dataset data)
   (plotly/base {:=x :time, :=y :pitch, :=color :transform})
   (plotly/layer-point {:=mark-size 12})
   (plotly/layer-line)
   (plotly/update-data
    (fn
     [d]
     (assoc
      d
      :=layout
      {:title "Klein four-group acting on a motif",
       :xaxis {:title "Time step"},
       :yaxis {:title "MIDI pitch"}})))
   plotly/plot)))


(def v22_l143 (def C12 (reel/cyclic-group 12)))


(def
 v23_l145
 (defn
  transpose-melody
  "Transpose a melody by k semitones."
  [k melody]
  (mapv (fn* [p1__68166#] (+ p1__68166# (long k))) melody)))


(def
 v25_l152
 (let
  [rows
   (mapv
    (fn
     [k]
     (let
      [transposed (transpose-melody k motif)]
      [k
       (str
        (mapv
         (fn*
          [p1__68167#]
          (get note-names (mod p1__68167# 12) (str p1__68167#)))
         (mapv
          (fn* [p1__68168#] (+ 60 (mod (- p1__68168# 60) 12)))
          transposed)))]))
    (range 12))]
  (kind/table
   {:column-names ["Semitones" "Transposed notes (mod octave)"],
    :row-vectors rows})))


(def v27_l167 (def D12 (reel/dihedral-group 12)))


(def v28_l169 (reel/order D12))


(deftest t29_l171 (is (fn v28_l169 [v] (= 24 v))))


(def
 v31_l188
 (def
  schoenberg-row
  "Schoenberg's Op. 25 row (pitch classes)."
  [4 5 7 1 6 3 8 2 11 0 9 10]))


(def
 v33_l197
 (defn
  row-forms
  "Generate the 48 forms of a tone row."
  [row]
  (let
   [prime
    row
    retrograde
    (vec (reverse row))
    pivot
    (first row)
    inversion
    (mapv (fn* [p1__68169#] (mod (- (* 2 pivot) p1__68169#) 12)) row)
    ri
    (vec (reverse inversion))
    base-forms
    {"P" prime, "R" retrograde, "I" inversion, "RI" ri}]
   (vec
    (for
     [[form-name form] base-forms k (range 12)]
     {:label (str form-name (when (pos? k) k)),
      :form-type form-name,
      :transposition k,
      :row
      (mapv (fn* [p1__68170#] (mod (+ p1__68170# k) 12)) form)})))))


(def v34_l214 (let [forms (row-forms schoenberg-row)] (count forms)))


(deftest t35_l217 (is (fn v34_l214 [v] (= 48 v))))


(def
 v37_l221
 (let
  [forms
   (row-forms schoenberg-row)
   selected
   (filterv
    (fn*
     [p1__68171#]
     (contains? #{0 6 3 9} (:transposition p1__68171#)))
    forms)
   selected
   (take 16 (sort-by (juxt :form-type :transposition) selected))]
  (kind/table
   {:column-names ["Form" "Row"],
    :row-vectors
    (mapv (fn [{:keys [label row]}] [label (str row)]) selected)})))


(def
 v39_l232
 (let
  [forms (row-forms schoenberg-row)]
  (every? (fn [{:keys [row]}] (= (set row) (set (range 12)))) forms)))


(deftest t40_l237 (is (fn v39_l232 [v] (= true v))))


(def
 v42_l244
 (let
  [forms
   (row-forms schoenberg-row)
   distinct-rows
   (set (map :row forms))]
  (count distinct-rows)))


(deftest t43_l248 (is (fn v42_l244 [v] (= 48 v))))


(def v45_l255 (def all-interval-row [0 11 7 4 2 9 3 8 10 1 5 6]))


(def
 v47_l259
 (let
  [intervals
   (mapv
    (fn
     [i]
     (mod (- (all-interval-row (inc i)) (all-interval-row i)) 12))
    (range 11))]
  (= (set intervals) (set (range 1 12)))))


(deftest t48_l266 (is (fn v47_l259 [v] (= true v))))


(def
 v50_l273
 (let
  [elts
   (vec (reel/elements V4))
   labels
   (mapv (fn [g] (get v4-labels g (str g))) elts)]
  (kind/table
   {:column-names (into ["·"] labels),
    :row-vectors
    (mapv
     (fn
      [g]
      (into
       [(get v4-labels g)]
       (mapv (fn [h] (get v4-labels (reel/op V4 g h))) elts)))
     elts)})))
