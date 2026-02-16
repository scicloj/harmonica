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
