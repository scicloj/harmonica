(ns
 harmonica-book.hearing-symmetry-generated-test
 (:require
  [scicloj.harmonica.core :as hm]
  [scicloj.harmonica.protocols :as p]
  [scicloj.kindly.v4.kind :as kind]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [clojure.test :refer [deftest is]]))


(def
 v3_l30
 (def V4 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2))))


(def v4_l32 (hm/order V4))


(deftest t5_l34 (is (= v4_l32 4)))


(def v7_l38 (vec (hm/elements V4)))


(deftest t8_l40 (is ((fn [v] (= 4 (count v))) v7_l38)))


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
  "Apply a Klein four-group element to a melody.\n   Inversion reflects around a fixed pivot (the first note of the motif)."
  [pivot [r i] melody]
  (let
   [inverted
    (if
     (= i 1)
     (mapv (fn* [p1__76604#] (- (* 2 pivot) p1__76604#)) melody)
     melody)
    retrograded
    (if (= r 1) (vec (reverse inverted)) inverted)]
   retrograded)))


(def
 v14_l78
 (def
  v4-labels
  {[0 0] "Original",
   [1 0] "Retrograde",
   [0 1] "Inversion",
   [1 1] "Retrograde Inversion"}))


(def
 v15_l84
 (let
  [rows
   (mapv
    (fn
     [g]
     (let
      [result (apply-v4 (first motif) g motif)]
      {:transform (v4-labels g),
       :element (str g),
       :melody (str result),
       :notes
       (str
        (mapv
         (fn*
          [p1__76605#]
          (get note-names p1__76605# (str p1__76605#)))
         result))}))
    (hm/elements V4))]
  (kind/table
   {:column-names ["Transform" "Element" "MIDI" "Notes"],
    :row-vectors
    (mapv
     (fn
      [{:keys [transform element melody notes]}]
      [transform element melody notes])
     rows)})))


(def
 v17_l102
 (every?
  (fn
   [[g h]]
   (let
    [gh-melody
     (apply-v4 (first motif) g (apply-v4 (first motif) h motif))
     direct
     (apply-v4 (first motif) (hm/op V4 g h) motif)]
    (= gh-melody direct)))
  (for [g (hm/elements V4) h (hm/elements V4)] [g h])))


(deftest t18_l108 (is (true? v17_l102)))


(def
 v20_l116
 (let
  [transforms
   (mapv
    (fn
     [g]
     (let
      [result (apply-v4 (first motif) g motif)]
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


(def v22_l142 (def C12 (hm/cyclic-group 12)))


(def
 v23_l144
 (defn
  transpose-melody
  "Transpose a melody by k semitones."
  [k melody]
  (mapv (fn* [p1__76606#] (+ p1__76606# (long k))) melody)))


(def
 v25_l151
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
          [p1__76607#]
          (get note-names (mod p1__76607# 12) (str p1__76607#)))
         (mapv
          (fn* [p1__76608#] (+ 60 (mod (- p1__76608# 60) 12)))
          transposed)))]))
    (range 12))]
  (kind/table
   {:column-names ["Semitones" "Transposed notes (mod octave)"],
    :row-vectors rows})))


(def v27_l166 (def D12 (hm/dihedral-group 12)))


(def v28_l168 (hm/order D12))


(deftest t29_l170 (is (= v28_l168 24)))


(def
 v31_l187
 (def
  schoenberg-row
  "Schoenberg's Op. 25 row (pitch classes)."
  [4 5 7 1 6 3 8 2 11 0 9 10]))


(def
 v33_l196
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
    (mapv (fn* [p1__76609#] (mod (- (* 2 pivot) p1__76609#) 12)) row)
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
      (mapv (fn* [p1__76610#] (mod (+ p1__76610# k) 12)) form)})))))


(def v34_l213 (let [forms (row-forms schoenberg-row)] (count forms)))


(deftest t35_l216 (is (= v34_l213 48)))


(def
 v37_l220
 (let
  [forms
   (row-forms schoenberg-row)
   selected
   (filterv
    (fn*
     [p1__76611#]
     (contains? #{0 6 3 9} (:transposition p1__76611#)))
    forms)
   selected
   (take 16 (sort-by (juxt :form-type :transposition) selected))]
  (kind/table
   {:column-names ["Form" "Row"],
    :row-vectors
    (mapv (fn [{:keys [label row]}] [label (str row)]) selected)})))


(def
 v39_l231
 (let
  [forms (row-forms schoenberg-row)]
  (every? (fn [{:keys [row]}] (= (set row) (set (range 12)))) forms)))


(deftest t40_l236 (is (true? v39_l231)))


(def
 v42_l243
 (let
  [forms
   (row-forms schoenberg-row)
   distinct-rows
   (set (map :row forms))]
  (count distinct-rows)))


(deftest t43_l247 (is (= v42_l243 48)))


(def v45_l254 (def all-interval-row [0 11 7 4 2 9 3 8 10 1 5 6]))


(def
 v47_l258
 (let
  [intervals
   (mapv
    (fn
     [i]
     (mod (- (all-interval-row (inc i)) (all-interval-row i)) 12))
    (range 11))]
  (= (set intervals) (set (range 1 12)))))


(deftest t48_l265 (is (true? v47_l258)))


(def
 v50_l272
 (let
  [elts
   (vec (hm/elements V4))
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
       (mapv (fn [h] (get v4-labels (hm/op V4 g h))) elts)))
     elts)})))
