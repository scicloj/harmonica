(ns scicloj.reel.characters
  "Character tables for finite groups.

   A character table is a value (map) containing:
     :group        - the group
     :classes      - vector of conjugacy class representatives
     :class-sizes  - vector of class sizes
     :irrep-labels - vector of labels for each irreducible representation
     :table        - vector of vectors of complex values (Vec2)
                     table[i][j] = chi_i(class_j)
     :table-re     - vector of double arrays (real parts of each row)
     :table-im     - vector of double arrays (imaginary parts of each row)

   For cyclic groups Z/nZ, the character table is exactly the DFT matrix:
   table[j][k] = omega^(jk) where omega = e^(2*pi*i/n).

   For symmetric groups S_n, the character table is computed via the
   Murnaghan-Nakayama rule. All entries are real integers."
  (:require [scicloj.reel.protocols :as p]
            [scicloj.reel.impl.murnaghan-nakayama :as mn]
            [scicloj.reel.impl.partition :as part]
            [fastmath.complex :as c]
            [fastmath.vector :as v]
            [tech.v3.datatype.functional :as dfn])
  (:import [fastmath.vector Vec2]))

;; ---------------------------------------------------------------------------
;; Character table construction (multimethod on group type)
;; ---------------------------------------------------------------------------

(defmulti character-table
  "Compute the character table of a finite group.
   Returns a map with :group, :classes, :class-sizes, :irrep-labels, :table."
  p/group-type)

(defmethod character-table :cyclic
  [G]
  (let [n (p/order G)
        ;; omega = e^(2*pi*i/n), the primitive n-th root of unity
        angle (/ (* 2.0 Math/PI) n)
        ;; Precompute cos and sin arrays for k = 0..n-1
        cos-vals (double-array (map #(Math/cos (* (double %) angle)) (range n)))
        sin-vals (double-array (map #(Math/sin (* (double %) angle)) (range n)))
        ;; Build table: table[j][k] = omega^(j*k)
        ;; Also build split real/imaginary double arrays for fast numerical access
        rows (mapv (fn [j]
                     (let [re-row (double-array n)
                           im-row (double-array n)
                           vec-row (object-array n)]
                       (dotimes [k n]
                         (let [idx (mod (* (long j) (long k)) n)]
                           (aset re-row k (aget cos-vals idx))
                           (aset im-row k (aget sin-vals idx))
                           (aset vec-row k (c/complex (aget cos-vals idx)
                                                      (aget sin-vals idx)))))
                       {:vec-row (vec vec-row)
                        :re-row re-row
                        :im-row im-row}))
                   (range n))]
    {:group G
     :classes (vec (range n))
     :class-sizes (vec (repeat n 1))
     :irrep-labels (vec (range n))
     :table (mapv :vec-row rows)
     :table-re (mapv :re-row rows)
     :table-im (mapv :im-row rows)}))

(defmethod character-table :symmetric
  [G]
  (let [n (.-n G)
        parts (part/partitions n)
        ;; Classes ordered by reverse lex (identity [1^n] last in partitions,
        ;; so we reverse to get identity first)
        classes (vec (reverse parts))
        num-classes (count classes)
        class-sizes (mapv #(part/partition-class-size n %) classes)
        ;; Compute character table via Murnaghan-Nakayama
        rows (mapv (fn [lam]
                     (let [re-row (double-array num-classes)
                           im-row (double-array num-classes)
                           vec-row (object-array num-classes)]
                       (dotimes [j num-classes]
                         (let [val (double (mn/chi lam (classes j)))]
                           (aset re-row j val)
                           (aset im-row j 0.0)
                           (aset vec-row j (c/complex val 0.0))))
                       {:vec-row (vec vec-row)
                        :re-row re-row
                        :im-row im-row}))
                   parts)]
    {:group G
     :classes classes
     :class-sizes class-sizes
     :irrep-labels parts
     :table (mapv :vec-row rows)
     :table-re (mapv :re-row rows)
     :table-im (mapv :im-row rows)}))

(defmethod character-table :dihedral
  [G]
  (let [n (.-n G)
        classes (p/conjugacy-classes G)
        num-classes (count classes)
        class-sizes (mapv :size classes)
        class-reps (mapv :representative classes)]
    (if (odd? n)
      ;; n odd: 2 one-dim irreps + (n-1)/2 two-dim irreps
      ;; Classes: {e}, {r^k, r^{-k}} for k=1..(n-1)/2, {all reflections}
      (let [num-2d (quot (dec n) 2)
            angle (/ (* 2.0 Math/PI) n)
            rows (vec
                  (concat
                   ;; χ₁: trivial — all 1s
                   [(let [re-row (double-array num-classes)
                          im-row (double-array num-classes)
                          vec-row (object-array num-classes)]
                      (dotimes [j num-classes]
                        (aset re-row j 1.0)
                        (aset im-row j 0.0)
                        (aset vec-row j (c/complex 1.0 0.0)))
                      {:vec-row (vec vec-row) :re-row re-row :im-row im-row})]
                   ;; χ₂: +1 on rotations, -1 on reflections
                   [(let [re-row (double-array num-classes)
                          im-row (double-array num-classes)
                          vec-row (object-array num-classes)]
                      (dotimes [j num-classes]
                        (let [rep (class-reps j)
                              val (if (= :s (first rep)) -1.0 1.0)]
                          (aset re-row j val)
                          (aset im-row j 0.0)
                          (aset vec-row j (c/complex val 0.0))))
                      {:vec-row (vec vec-row) :re-row re-row :im-row im-row})]
                   ;; χ_m for m=1..(n-1)/2: dimension 2
                   ;; χ_m(e)=2, χ_m(r^k)=2cos(2πmk/n), χ_m(s)=0
                   (mapv (fn [m]
                           (let [re-row (double-array num-classes)
                                 im-row (double-array num-classes)
                                 vec-row (object-array num-classes)]
                             (dotimes [j num-classes]
                               (let [rep (class-reps j)
                                     val (cond
                                           ;; identity
                                           (= rep [:r 0]) 2.0
                                           ;; rotation class {r^k, r^{-k}}
                                           (= :r (first rep))
                                           (* 2.0 (Math/cos (* angle m (double (second rep)))))
                                           ;; reflection class
                                           :else 0.0)]
                                 (aset re-row j val)
                                 (aset im-row j 0.0)
                                 (aset vec-row j (c/complex val 0.0))))
                             {:vec-row (vec vec-row) :re-row re-row :im-row im-row}))
                         (range 1 (inc num-2d)))))]
        {:group G
         :classes class-reps
         :class-sizes class-sizes
         :irrep-labels (vec (concat [:trivial :sign]
                                    (mapv (fn [m] [:dim2 m]) (range 1 (inc num-2d)))))
         :table (mapv :vec-row rows)
         :table-re (mapv :re-row rows)
         :table-im (mapv :im-row rows)})
      ;; n even: 4 one-dim irreps + (n/2-1) two-dim irreps
      ;; Classes: {e}, {r^{n/2}}, {r^k,r^{-k}} for k=1..n/2-1, {even refl}, {odd refl}
      (let [half (quot n 2)
            num-2d (dec half)
            angle (/ (* 2.0 Math/PI) n)
            ;; Helper: value of 1-dim irrep on class representative
            ;; χ₁: all 1
            ;; χ₂: +1 on rotations, -1 on all reflections
            ;; χ₃: (-1)^k on r^k, +1 on even-refl, -1 on odd-refl
            ;; χ₄: (-1)^k on r^k, -1 on even-refl, +1 on odd-refl
            one-dim-row (fn [rot-fn even-refl-val odd-refl-val]
                          (let [re-row (double-array num-classes)
                                im-row (double-array num-classes)
                                vec-row (object-array num-classes)]
                            (dotimes [j num-classes]
                              (let [rep (class-reps j)
                                    val (cond
                                          (= :r (first rep)) (rot-fn (second rep))
                                          (= rep [:s 0]) (double even-refl-val)
                                          :else (double odd-refl-val))]
                                (aset re-row j (double val))
                                (aset im-row j 0.0)
                                (aset vec-row j (c/complex val 0.0))))
                            {:vec-row (vec vec-row) :re-row re-row :im-row im-row}))
            rows (vec
                  (concat
                   [(one-dim-row (constantly 1.0) 1.0 1.0) ;; χ₁
                    (one-dim-row (constantly 1.0) -1.0 -1.0) ;; χ₂
                    (one-dim-row (fn [k] (Math/pow -1.0 (double k))) 1.0 -1.0) ;; χ₃
                    (one-dim-row (fn [k] (Math/pow -1.0 (double k))) -1.0 1.0)] ;; χ₄
                   ;; 2-dim irreps χ_m for m=1..(n/2-1)
                   (mapv (fn [m]
                           (let [re-row (double-array num-classes)
                                 im-row (double-array num-classes)
                                 vec-row (object-array num-classes)]
                             (dotimes [j num-classes]
                               (let [rep (class-reps j)
                                     val (cond
                                           (= :r (first rep))
                                           (* 2.0 (Math/cos (* angle m (double (second rep)))))
                                           :else 0.0)]
                                 (aset re-row j val)
                                 (aset im-row j 0.0)
                                 (aset vec-row j (c/complex val 0.0))))
                             {:vec-row (vec vec-row) :re-row re-row :im-row im-row}))
                         (range 1 (inc num-2d)))))]
        {:group G
         :classes class-reps
         :class-sizes class-sizes
         :irrep-labels (vec (concat [:trivial :sign :sign-rot :sign-both]
                                    (mapv (fn [m] [:dim2 m]) (range 1 (inc num-2d)))))
         :table (mapv :vec-row rows)
         :table-re (mapv :re-row rows)
         :table-im (mapv :im-row rows)}))))

;; ---------------------------------------------------------------------------
;; Character inner product
;; ---------------------------------------------------------------------------

(defn character-inner-product
  "Compute the inner product of two characters (or class functions).

   <chi, psi> = (1/|G|) * sum_{classes} |C_mu| * chi(mu) * conj(psi(mu))

   chi-vals and psi-vals are vectors of complex values (Vec2), one per class.
   class-sizes is a vector of class sizes.
   group-order is |G|.

   Uses split real/imaginary arrays and dfn for vectorized computation."
  [chi-vals psi-vals class-sizes group-order]
  (let [n (count chi-vals)
        chi-re (double-array n)
        chi-im (double-array n)
        psi-re (double-array n)
        psi-im (double-array n)
        sz (double-array n)]
    (dotimes [i n]
      (let [^Vec2 chi (chi-vals i)
            ^Vec2 psi (psi-vals i)]
        (aset chi-re i (.-x chi))
        (aset chi-im i (.-y chi))
        (aset psi-re i (.-x psi))
        (aset psi-im i (.-y psi))
        (aset sz i (double (class-sizes i)))))
    ;; chi * conj(psi) = (a+bi)(c-di) = (ac+bd) + (bc-ad)i
    ;; weighted by class sizes, scaled by 1/|G|
    (let [inv-order (/ 1.0 (double group-order))]
      (c/complex (* inv-order (dfn/sum (dfn/* sz (dfn/+ (dfn/* chi-re psi-re)
                                                        (dfn/* chi-im psi-im)))))
                 (* inv-order (dfn/sum (dfn/* sz (dfn/- (dfn/* chi-im psi-re)
                                                        (dfn/* chi-re psi-im)))))))))
