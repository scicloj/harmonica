(ns
 harmonica-book.hearing-symmetry-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.kindly.v4.kind :as kind]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [tech.v3.datatype :as dtype]
  [tech.v3.datatype.functional :as dfn]
  [clojure.test :refer [deftest is]]))


(def
 v3_l31
 (def V4 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2))))


(def v4_l33 (hm/order V4))


(deftest t5_l35 (is (= v4_l33 4)))


(def v7_l39 (vec (hm/elements V4)))


(deftest t8_l41 (is ((fn [v] (= 4 (count v))) v7_l39)))


(def v10_l56 (def sample-rate 44100.0))


(def
 v11_l58
 (defn
  midi->freq
  "Convert a MIDI note number to frequency in Hz.\n   A4 (MIDI 69) = 440 Hz."
  [midi]
  (* 440.0 (Math/pow 2.0 (/ (- midi 69.0) 12.0)))))


(def
 v12_l64
 (defn
  melody->samples
  "Render a melody (vector of MIDI note numbers) as audio samples."
  [melody note-dur]
  (let
   [n-note
    (long (* note-dur sample-rate))
    amp
    2500.0
    attack
    (long (* 0.015 sample-rate))
    sounding
    (long (* 0.85 n-note))
    release
    (long (* 0.06 sample-rate))]
   (dtype/make-reader
    :float32
    (* (count melody) n-note)
    (let
     [note-idx
      (quot idx n-note)
      t
      (rem idx n-note)
      freq
      (midi->freq (melody note-idx))
      phase
      (/ (double t) sample-rate)]
     (if
      (>= t sounding)
      (float 0.0)
      (let
       [env
        (cond
         (< t attack)
         (/ (double t) attack)
         (> t (- sounding release))
         (*
          (Math/exp (* -1.5 (/ (double (- t attack)) sounding)))
          (/ (double (- sounding t)) release))
         :else
         (Math/exp (* -1.5 (/ (double (- t attack)) sounding))))
        wave
        (+
         (* 0.65 (Math/sin (* 2.0 Math/PI freq phase)))
         (* 0.25 (Math/sin (* 2.0 Math/PI 2.0 freq phase)))
         (* 0.1 (Math/sin (* 2.0 Math/PI 3.0 freq phase))))]
       (float (* amp env wave)))))))))


(def
 v13_l92
 (defn
  play
  [melody]
  (kind/audio
   {:samples (melody->samples melody 0.35), :sample-rate sample-rate})))


(def v15_l103 (def motif [67 67 67 63]))


(def
 v16_l105
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
 v18_l113
 (defn
  apply-v4
  "Apply a Klein four-group element to a melody.\n   Inversion reflects around a fixed pivot (the first note of the motif)."
  [pivot [r i] melody]
  (let
   [inverted
    (if
     (= i 1)
     (mapv (fn* [p1__136573#] (- (* 2 pivot) p1__136573#)) melody)
     melody)
    retrograded
    (if (= r 1) (vec (reverse inverted)) inverted)]
   retrograded)))


(def
 v19_l125
 (def
  v4-labels
  {[0 0] "Original",
   [1 0] "Retrograde",
   [0 1] "Inversion",
   [1 1] "Retrograde Inversion"}))


(def
 v20_l131
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
          [p1__136574#]
          (get note-names p1__136574# (str p1__136574#)))
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


(def v22_l150 (play motif))


(def v24_l153 (play (apply-v4 (first motif) [1 0] motif)))


(def v26_l156 (play (apply-v4 (first motif) [0 1] motif)))


(def v28_l159 (play (apply-v4 (first motif) [1 1] motif)))


(def
 v30_l163
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


(deftest t31_l169 (is (true? v30_l163)))


(def
 v33_l177
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


(def v35_l203 (def C12 (hm/cyclic-group 12)))


(def
 v36_l205
 (defn
  transpose-melody
  "Transpose a melody by k semitones."
  [k melody]
  (mapv (fn* [p1__136575#] (+ p1__136575# k)) melody)))


(def
 v38_l212
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
          [p1__136576#]
          (get note-names (mod p1__136576# 12) (str p1__136576#)))
         (mapv
          (fn* [p1__136577#] (+ 60 (mod (- p1__136577# 60) 12)))
          transposed)))]))
    (range 12))]
  (kind/table
   {:column-names ["Semitones" "Transposed notes (mod octave)"],
    :row-vectors rows})))


(def v40_l224 (play motif))


(def v42_l227 (play (transpose-melody 3 motif)))


(def v44_l230 (play (transpose-melody 5 motif)))


(def v46_l233 (play (transpose-melody 7 motif)))


(def v48_l241 (def D12 (hm/dihedral-group 12)))


(def v49_l243 (hm/order D12))


(deftest t50_l245 (is (= v49_l243 24)))


(def
 v52_l262
 (def
  schoenberg-row
  "Schoenberg's Op. 25 row (pitch classes)."
  [4 5 7 1 6 3 8 2 11 0 9 10]))


(def
 v53_l266
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
 v55_l275
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
    (mapv (fn* [p1__136578#] (mod (- (* 2 pivot) p1__136578#) 12)) row)
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
      (mapv (fn* [p1__136579#] (mod (+ p1__136579# k) 12)) form)})))))


(def v56_l292 (let [forms (row-forms schoenberg-row)] (count forms)))


(deftest t57_l295 (is (= v56_l292 48)))


(def
 v59_l299
 (let
  [forms
   (row-forms schoenberg-row)
   selected
   (filterv
    (fn*
     [p1__136580#]
     (contains? #{0 6 3 9} (:transposition p1__136580#)))
    forms)
   selected
   (take 16 (sort-by (juxt :form-type :transposition) selected))]
  (kind/table
   {:column-names ["Form" "Row"],
    :row-vectors
    (mapv (fn [{:keys [label row]}] [label (str row)]) selected)})))


(def
 v61_l313
 (play (mapv (fn* [p1__136581#] (+ 60 p1__136581#)) schoenberg-row)))


(def
 v63_l316
 (play
  (mapv
   (fn* [p1__136582#] (+ 60 p1__136582#))
   (vec (reverse schoenberg-row)))))


(def
 v65_l319
 (let
  [pivot (first schoenberg-row)]
  (play
   (mapv
    (fn* [p1__136583#] (+ 60 (mod (- (* 2 pivot) p1__136583#) 12)))
    schoenberg-row))))


(def
 v67_l323
 (let
  [pivot (first schoenberg-row)]
  (play
   (mapv
    (fn* [p1__136584#] (+ 60 p1__136584#))
    (vec
     (reverse
      (mapv
       (fn* [p1__136585#] (mod (- (* 2 pivot) p1__136585#) 12))
       schoenberg-row)))))))


(def
 v69_l328
 (let
  [forms (row-forms schoenberg-row)]
  (every? (fn [{:keys [row]}] (= (set row) (set (range 12)))) forms)))


(deftest t70_l333 (is (true? v69_l328)))


(def
 v72_l340
 (let
  [forms
   (row-forms schoenberg-row)
   distinct-rows
   (set (map :row forms))]
  (count distinct-rows)))


(deftest t73_l344 (is (= v72_l340 48)))


(def v75_l351 (def all-interval-row [0 11 7 4 2 9 3 8 10 1 5 6]))


(def
 v77_l355
 (let
  [intervals
   (mapv
    (fn
     [i]
     (mod (- (all-interval-row (inc i)) (all-interval-row i)) 12))
    (range 11))]
  (= (set intervals) (set (range 1 12)))))


(deftest t78_l362 (is (true? v77_l355)))


(def
 v80_l369
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
