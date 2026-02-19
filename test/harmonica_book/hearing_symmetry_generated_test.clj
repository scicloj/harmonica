(ns
 harmonica-book.hearing-symmetry-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.kindly.v4.kind :as kind]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [clojure.test :refer [deftest is]]))


(def
 v3_l29
 (def V4 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2))))


(def v4_l31 (hm/order V4))


(deftest t5_l33 (is (= v4_l31 4)))


(def v7_l37 (vec (hm/elements V4)))


(deftest t8_l39 (is ((fn [v] (= 4 (count v))) v7_l37)))


(def v10_l55 (def motif [67 67 67 63]))


(def
 v11_l57
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
 v13_l65
 (defn
  apply-v4
  "Apply a Klein four-group element to a melody.\n   Inversion reflects around a fixed pivot (the first note of the motif)."
  [pivot [r i] melody]
  (let
   [inverted
    (if
     (= i 1)
     (mapv (fn* [p1__106821#] (- (* 2 pivot) p1__106821#)) melody)
     melody)
    retrograded
    (if (= r 1) (vec (reverse inverted)) inverted)]
   retrograded)))


(def
 v14_l77
 (def
  v4-labels
  {[0 0] "Original",
   [1 0] "Retrograde",
   [0 1] "Inversion",
   [1 1] "Retrograde Inversion"}))


(def
 v15_l83
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
          [p1__106822#]
          (get note-names p1__106822# (str p1__106822#)))
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
 v17_l101
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


(deftest t18_l107 (is (true? v17_l101)))


(def
 v20_l115
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


(def v22_l141 (def C12 (hm/cyclic-group 12)))


(def
 v23_l143
 (defn
  transpose-melody
  "Transpose a melody by k semitones."
  [k melody]
  (mapv (fn* [p1__106823#] (+ p1__106823# (long k))) melody)))


(def
 v25_l150
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
          [p1__106824#]
          (get note-names (mod p1__106824# 12) (str p1__106824#)))
         (mapv
          (fn* [p1__106825#] (+ 60 (mod (- p1__106825# 60) 12)))
          transposed)))]))
    (range 12))]
  (kind/table
   {:column-names ["Semitones" "Transposed notes (mod octave)"],
    :row-vectors rows})))


(def v27_l165 (def D12 (hm/dihedral-group 12)))


(def v28_l167 (hm/order D12))


(deftest t29_l169 (is (= v28_l167 24)))


(def
 v31_l186
 (def
  schoenberg-row
  "Schoenberg's Op. 25 row (pitch classes)."
  [4 5 7 1 6 3 8 2 11 0 9 10]))


(def
 v32_l190
 (let
  [pc-name
   {0 "C",
    7 "G",
    1 "C#",
    4 "E",
    6 "F#",
    3 "Eb",
    2 "D",
    11 "B",
    9 "A",
    5 "F",
    10 "Bb",
    8 "Ab"}]
  (mapv pc-name schoenberg-row)))


(def
 v34_l199
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
    (mapv (fn* [p1__106826#] (mod (- (* 2 pivot) p1__106826#) 12)) row)
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
      (mapv (fn* [p1__106827#] (mod (+ p1__106827# k) 12)) form)})))))


(def v35_l216 (let [forms (row-forms schoenberg-row)] (count forms)))


(deftest t36_l219 (is (= v35_l216 48)))


(def
 v38_l223
 (let
  [forms
   (row-forms schoenberg-row)
   selected
   (filterv
    (fn*
     [p1__106828#]
     (contains? #{0 6 3 9} (:transposition p1__106828#)))
    forms)
   selected
   (take 16 (sort-by (juxt :form-type :transposition) selected))]
  (kind/table
   {:column-names ["Form" "Row"],
    :row-vectors
    (mapv (fn [{:keys [label row]}] [label (str row)]) selected)})))


(def
 v40_l234
 (let
  [forms (row-forms schoenberg-row)]
  (every? (fn [{:keys [row]}] (= (set row) (set (range 12)))) forms)))


(deftest t41_l239 (is (true? v40_l234)))


(def
 v43_l246
 (let
  [forms
   (row-forms schoenberg-row)
   distinct-rows
   (set (map :row forms))]
  (count distinct-rows)))


(deftest t44_l250 (is (= v43_l246 48)))


(def v46_l257 (def all-interval-row [0 11 7 4 2 9 3 8 10 1 5 6]))


(def
 v48_l261
 (let
  [intervals
   (mapv
    (fn
     [i]
     (mod (- (all-interval-row (inc i)) (all-interval-row i)) 12))
    (range 11))]
  (= (set intervals) (set (range 1 12)))))


(deftest t49_l268 (is (true? v48_l261)))


(def
 v51_l275
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
