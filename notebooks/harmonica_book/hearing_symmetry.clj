;; # Hearing Symmetry — Permutation Music
;;
;; Apply group elements to a musical motif and hear the result. Different
;; group actions produce different musical transformations — [retrograde](https://en.wikipedia.org/wiki/Retrograde_(music)),
;; [inversion](https://en.wikipedia.org/wiki/Inversion_(music)), [transposition](https://en.wikipedia.org/wiki/Transposition_(music)) — that musicians have used for centuries.
;;
;; Bach was doing group theory. Schoenberg made it explicit.

(ns harmonica-book.hearing-symmetry
  (:require
   [scicloj.harmonica :as hm]
   [scicloj.kindly.v4.kind :as kind]
   [tablecloth.api :as tc]
   [scicloj.tableplot.v1.plotly :as plotly]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.functional :as dfn]))

;; ## The Klein Four-Group
;;
;; The simplest musical symmetries form the **Klein four-group**
;; $V_4 \cong \mathbb{Z}/2\mathbb{Z} \times \mathbb{Z}/2\mathbb{Z}$:
;;
;; - **Original** ($e$): play the melody as written
;; - **Retrograde** ($R$): play it backwards
;; - **Inversion** ($I$): flip intervals upside down
;; - **Retrograde inversion** ($RI$): both at once
;;
;; These four operations form a group because any two of them compose
;; to give one of the four, and each is its own inverse.

(def V4 (hm/product-group (hm/cyclic-group 2) (hm/cyclic-group 2)))

(hm/order V4)

(kind/test-last [= 4])

;; The group elements:

(vec (hm/elements V4))

(kind/test-last [(fn [v] (= 4 (count v)))])

;; We label them:
;;
;; - `[0 0]` = Original
;; - `[1 0]` = Retrograde
;; - `[0 1]` = Inversion
;; - `[1 1]` = Retrograde Inversion

;; ## Audio Synthesis
;;
;; To hear these transformations, we need a simple synthesizer.
;; Each MIDI note becomes a sine wave with a short envelope
;; to avoid clicks.

(def sample-rate 44100.0)

(defn midi->freq
  "Convert a MIDI note number to frequency in Hz.
   A4 (MIDI 69) = 440 Hz."
  [midi]
  (* 440.0 (Math/pow 2.0 (/ (- midi 69.0) 12.0))))

(defn melody->samples
  "Render a melody (vector of MIDI note numbers) as audio samples."
  [melody note-dur]
  (let [n-note (long (* note-dur sample-rate))
        amp 3800.0
        attack (long (* 0.01 sample-rate))
        release (long (* 0.03 sample-rate))]
    (dtype/make-reader
     :float32
     (* (count melody) n-note)
     (let [note-idx (quot idx n-note)
           t (rem idx n-note)
           freq (midi->freq (melody note-idx))
           env (cond
                 (< t attack) (/ (double t) attack)
                 (> t (- n-note release)) (/ (double (- n-note t)) release)
                 :else 1.0)]
       (float (* amp env (Math/sin (* 2.0 Math/PI freq (/ (double t) sample-rate)))))))))

(defn play [melody]
  (kind/audio {:samples (melody->samples melody 0.35)
               :sample-rate sample-rate}))

;; ## A Musical Motif
;;
;; Let's take a short motif — the opening of Beethoven's Fifth:
;; G G G E♭ (in MIDI numbers: 67 67 67 63).
;;
;; We represent a melody as a vector of MIDI pitch numbers.

(def motif [67 67 67 63])

(def note-names
  {60 "C" 61 "C#" 62 "D" 63 "Eb" 64 "E" 65 "F" 66 "F#"
   67 "G" 68 "G#" 69 "A" 70 "Bb" 71 "B" 72 "C'"})

;; ## Applying the Group
;;
;; The group acts on melodies. Each element transforms the melody:

(defn apply-v4
  "Apply a Klein four-group element to a melody.
   Inversion reflects around a fixed pivot (the first note of the motif)."
  [pivot [r i] melody]
  (let [inverted (if (= i 1)
                   (mapv #(- (* 2 pivot) %) melody)
                   melody)
        retrograded (if (= r 1)
                      (vec (reverse inverted))
                      inverted)]
    retrograded))

(def v4-labels
  {[0 0] "Original"
   [1 0] "Retrograde"
   [0 1] "Inversion"
   [1 1] "Retrograde Inversion"})

(let [rows (mapv (fn [g]
                   (let [result (apply-v4 (first motif) g motif)]
                     {:transform (v4-labels g)
                      :element (str g)
                      :melody (str result)
                      :notes (str (mapv #(get note-names % (str %)) result))}))
                 (hm/elements V4))]
  (kind/table
   {:column-names ["Transform" "Element" "MIDI" "Notes"]
    :row-vectors (mapv (fn [{:keys [transform element melody notes]}]
                         [transform element melody notes])
                       rows)}))

;; The inversion reflects all intervals: where the original goes down
;; (G→E♭, a minor third down), the inversion goes up (G→B, a major third up).
;;
;; Listen to each transformation:

;; Original:
(play motif)

;; Retrograde:
(play (apply-v4 (first motif) [1 0] motif))

;; Inversion:
(play (apply-v4 (first motif) [0 1] motif))

;; Retrograde Inversion:
(play (apply-v4 (first motif) [1 1] motif))

;; ### Verification: V₄ is closed under composition

(every? (fn [[g h]]
          (let [gh-melody (apply-v4 (first motif) g (apply-v4 (first motif) h motif))
                direct (apply-v4 (first motif) (hm/op V4 g h) motif)]
            (= gh-melody direct)))
        (for [g (hm/elements V4) h (hm/elements V4)] [g h]))

(kind/test-last [true?])

;; ## Visualizing Transformations
;;
;; A piano-roll view shows each note as a dot, with time on the x-axis
;; and pitch on the y-axis. The four transformations are geometric:
;; retrograde = horizontal flip, inversion = vertical flip.

(let [transforms (mapv (fn [g]
                         (let [result (apply-v4 (first motif) g motif)]
                           {:element g
                            :label (v4-labels g)
                            :melody result}))
                       [[0 0] [1 0] [0 1] [1 1]])
      data (vec (mapcat (fn [{:keys [label melody]}]
                          (map-indexed (fn [i pitch]
                                         {:time i :pitch pitch :transform label})
                                       melody))
                        transforms))]
  (-> (tc/dataset data)
      (plotly/base {:=x :time :=y :pitch :=color :transform})
      (plotly/layer-point {:=mark-size 12})
      (plotly/layer-line)
      (plotly/update-data
       (fn [d] (assoc d :=layout {:title "Klein four-group acting on a motif"
                                  :xaxis {:title "Time step"}
                                  :yaxis {:title "MIDI pitch"}})))
      plotly/plot))

;; ## Transposition: The Cyclic Group $C_{12}$
;;
;; **Transposition** shifts all notes by $k$ semitones. The 12 possible
;; transpositions form the cyclic group $\mathbb{Z}/12\mathbb{Z}$.

(def C12 (hm/cyclic-group 12))

(defn transpose-melody
  "Transpose a melody by k semitones."
  [k melody]
  (mapv #(+ % k) melody))

;; All 12 transpositions of our motif:

(let [rows (mapv (fn [k]
                   (let [transposed (transpose-melody k motif)]
                     [k (str (mapv #(get note-names (mod % 12) (str %))
                                   (mapv #(+ 60 (mod (- % 60) 12)) transposed)))]))
                 (range 12))]
  (kind/table
   {:column-names ["Semitones" "Transposed notes (mod octave)"]
    :row-vectors rows}))

;; Listen to a few transpositions — the motif shifts up:

;; Original (G G G Eb):
(play motif)

;; Up 3 semitones (Bb Bb Bb Gb):
(play (transpose-melody 3 motif))

;; Up 5 semitones (C C C Ab):
(play (transpose-melody 5 motif))

;; Up 7 semitones (D D D Bb):
(play (transpose-melody 7 motif))

;; ## The Full Group: $D_{12}$ on Pitch Classes
;;
;; Combining transposition ($C_{12}$) with inversion gives the dihedral
;; group $D_{12}$ of order 24. This is the standard **TI-group** of
;; pitch-class set theory.

(def D12 (hm/dihedral-group 12))

(hm/order D12)

(kind/test-last [= 24])

;; ## Twelve-Tone Rows
;;
;; In Schoenberg's [twelve-tone technique](https://en.wikipedia.org/wiki/Twelve-tone_technique), a **[tone row](https://en.wikipedia.org/wiki/Tone_row)** is an ordering
;; of all 12 pitch classes. The composer then derives all material from
;; the row and its transformations.
;;
;; A tone row is a permutation of {0, 1, ..., 11}. The group acts on it:
;;
;; - Transposition: add $k$ to each pitch class (mod 12)
;; - Inversion: negate each pitch class (mod 12)
;; - Retrograde: reverse the order
;; - Retrograde inversion: both
;;
;; With 12 transpositions × 4 forms = **48 transformations** of each row.

(def schoenberg-row
  "Schoenberg's Op. 25 row (pitch classes)."
  [4 5 7 1 6 3 8 2 11 0 9 10])

(let [pc-name {0 "C" 1 "C#" 2 "D" 3 "Eb" 4 "E" 5 "F" 6 "F#"
               7 "G" 8 "Ab" 9 "A" 10 "Bb" 11 "B"}]
  (mapv pc-name schoenberg-row))

;; The 48 forms come from combining:
;;
;; - 12 transpositions (add k mod 12)
;; - 4 forms: P (prime), R (retrograde), I (inversion), RI

(defn row-forms
  "Generate the 48 forms of a tone row."
  [row]
  (let [prime row
        retrograde (vec (reverse row))
        ;; Inversion: I_0 maps pitch x to (- first-note x) mod 12
        pivot (first row)
        inversion (mapv #(mod (- (* 2 pivot) %) 12) row)
        ri (vec (reverse inversion))
        base-forms {"P" prime "R" retrograde "I" inversion "RI" ri}]
    (vec (for [[form-name form] base-forms
               k (range 12)]
           {:label (str form-name (when (pos? k) k))
            :form-type form-name
            :transposition k
            :row (mapv #(mod (+ % k) 12) form)}))))

(let [forms (row-forms schoenberg-row)]
  (count forms))

(kind/test-last [= 48])

;; Let's see a few of the 48 forms:

(let [forms (row-forms schoenberg-row)
      selected (filterv #(contains? #{0 3 6 9} (:transposition %)) forms)
      selected (take 16 (sort-by (juxt :form-type :transposition) selected))]
  (kind/table
   {:column-names ["Form" "Row"]
    :row-vectors (mapv (fn [{:keys [label row]}]
                         [label (str row)])
                       selected)}))

;; Listen to the prime row and its three transformations.
;; Each note is a pitch class, so we place them in the octave
;; starting at middle C (MIDI 60):

;; Prime:
(play (mapv #(+ 60 %) schoenberg-row))

;; Retrograde:
(play (mapv #(+ 60 %) (vec (reverse schoenberg-row))))

;; Inversion:
(let [pivot (first schoenberg-row)]
  (play (mapv #(+ 60 (mod (- (* 2 pivot) %) 12)) schoenberg-row)))

;; Retrograde Inversion:
(let [pivot (first schoenberg-row)]
  (play (mapv #(+ 60 %) (vec (reverse (mapv #(mod (- (* 2 pivot) %) 12) schoenberg-row))))))

;; ### All 48 rows use all 12 pitch classes

(let [forms (row-forms schoenberg-row)]
  (every? (fn [{:keys [row]}]
            (= (set row) (set (range 12))))
          forms))

(kind/test-last [true?])

;; ## Orbit Analysis
;;
;; The 48 forms of a row form the orbit of the row under the combined action
;; of transposition, inversion, and retrograde. Are all 48 actually distinct?

(let [forms (row-forms schoenberg-row)
      distinct-rows (set (map :row forms))]
  (count distinct-rows))

(kind/test-last [= 48])

;; Yes — Schoenberg's row has no internal symmetries, so its orbit has
;; the maximum size of 48.

;; Some rows are more symmetric. The **all-interval row** has special properties:

(def all-interval-row [0 11 7 4 2 9 3 8 10 1 5 6])

;; Let's check: are all 11 interval classes present?

(let [intervals (mapv (fn [i]
                        (mod (- (all-interval-row (inc i))
                                (all-interval-row i))
                             12))
                      (range 11))]
  (= (set intervals) (set (range 1 12))))

(kind/test-last [true?])

;; ## The Group Table
;;
;; The Klein four-group $V_4$ has a simple multiplication table.
;; Every element is its own inverse:

(let [elts (vec (hm/elements V4))
      labels (mapv (fn [g] (get v4-labels g (str g))) elts)]
  (kind/table
   {:column-names (into ["·"] labels)
    :row-vectors (mapv (fn [g]
                         (into [(get v4-labels g)]
                               (mapv (fn [h]
                                       (get v4-labels (hm/op V4 g h)))
                                     elts)))
                       elts)}))

;; Every element squares to the identity — this is the defining property
;; of the Klein four-group. It's the simplest non-cyclic group.

;; ## Summary
;;
;; This notebook demonstrated:
;;
;; - **Klein four-group $V_4$**: retrograde, inversion, retrograde-inversion
;; - **Transposition as $C_{12}$ action**: shifting all pitches mod 12
;; - **The TI-group $D_{12}$**: combining transposition and inversion
;; - **Twelve-tone rows**: 48 forms as the orbit under $V_4 \times C_{12}$
;; - **Group theory in music**: composers use symmetry as a compositional tool

;; For a complementary perspective on music and group theory — classifying
;; chords via group actions — see [Chord Geometry](chord_geometry.html).

;; $V_4$ is a product group ($\mathbb{Z}/2\mathbb{Z} \times \mathbb{Z}/2\mathbb{Z}$) —
;; for the Fourier transform on product groups, see
;; [Product Group DFT](product_group_dft.html).

;; For the deeper theory of non-abelian groups — permutations, partitions,
;; and Fourier analysis on $S_n$ — continue to
;; [Symmetric Groups](symmetric_groups.html).

