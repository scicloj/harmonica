(ns scicloj.harmonica.analysis.characters
  "Character tables for finite groups.

   A character table is a value (map) containing:
     :group        - the group
     :classes      - vector of conjugacy class representatives
     :class-sizes  - vector of class sizes
     :irrep-labels - vector of labels for each irreducible representation
     :table        - ComplexTensor matrix [n_irreps × n_classes]

   For cyclic groups Z/nZ, the character table is exactly the DFT matrix:
   table[j][k] = omega^(jk) where omega = e^(2*pi*i/n).

   For symmetric groups S_n, the character table is computed via the
   Murnaghan-Nakayama rule. All entries are real integers."
  (:require [scicloj.harmonica.protocols :as p]
            [scicloj.harmonica.combinatorics.murnaghan-nakayama :as mn]
            [scicloj.harmonica.combinatorics.partition :as part]
            [scicloj.harmonica.linalg.complex :as cx]
            [tech.v3.datatype.functional :as dfn]))

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
        ;; Build re and im rows for the full table
        re-rows (mapv (fn [j]
                        (let [row (double-array n)]
                          (dotimes [k n]
                            (let [idx (mod (* (long j) (long k)) n)]
                              (aset row k (aget cos-vals idx))))
                          row))
                      (range n))
        im-rows (mapv (fn [j]
                        (let [row (double-array n)]
                          (dotimes [k n]
                            (let [idx (mod (* (long j) (long k)) n)]
                              (aset row k (aget sin-vals idx))))
                          row))
                      (range n))]
    {:group G
     :classes (vec (range n))
     :class-sizes (vec (repeat n 1))
     :irrep-labels (vec (range n))
     :table (cx/complex-tensor re-rows im-rows)}))

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
        re-rows (mapv (fn [lam]
                        (let [row (double-array num-classes)]
                          (dotimes [j num-classes]
                            (aset row j (double (mn/chi lam (classes j)))))
                          row))
                      parts)
        im-rows (mapv (fn [_] (double-array num-classes)) parts)]
    {:group G
     :classes classes
     :class-sizes class-sizes
     :irrep-labels parts
     :table (cx/complex-tensor re-rows im-rows)}))

(defmethod character-table :dihedral
  [G]
  (let [n (.-n G)
        classes (p/conjugacy-classes G)
        num-classes (count classes)
        class-sizes (mapv :size classes)
        class-reps (mapv :representative classes)]
    (if (odd? n)
      ;; n odd: 2 one-dim irreps + (n-1)/2 two-dim irreps
      (let [num-2d (quot (dec n) 2)
            num-irreps (+ 2 num-2d)
            angle (/ (* 2.0 Math/PI) n)
            re-rows (vec (repeatedly num-irreps #(double-array num-classes)))
            im-rows (vec (repeatedly num-irreps #(double-array num-classes)))]
        ;; Row 0: trivial — all 1s
        (dotimes [j num-classes]
          (aset ^doubles (re-rows 0) j 1.0))
        ;; Row 1: +1 on rotations, -1 on reflections
        (dotimes [j num-classes]
          (aset ^doubles (re-rows 1) j
                (if (= :s (first (class-reps j))) -1.0 1.0)))
        ;; Rows 2..(1+num-2d): 2-dim irreps
        (dotimes [mi num-2d]
          (let [m (inc mi)
                row-idx (+ 2 mi)]
            (dotimes [j num-classes]
              (let [rep (class-reps j)
                    val (cond
                          (= rep [:r 0]) 2.0
                          (= :r (first rep))
                          (* 2.0 (Math/cos (* angle m (double (second rep)))))
                          :else 0.0)]
                (aset ^doubles (re-rows row-idx) j val)))))
        {:group G
         :classes class-reps
         :class-sizes class-sizes
         :irrep-labels (vec (concat [:trivial :sign]
                                    (mapv (fn [m] [:dim2 m]) (range 1 (inc num-2d)))))
         :table (cx/complex-tensor re-rows im-rows)})
      ;; n even: 4 one-dim irreps + (n/2-1) two-dim irreps
      (let [half (quot n 2)
            num-2d (dec half)
            num-irreps (+ 4 num-2d)
            angle (/ (* 2.0 Math/PI) n)
            re-rows (vec (repeatedly num-irreps #(double-array num-classes)))
            im-rows (vec (repeatedly num-irreps #(double-array num-classes)))
            ;; Helper: fill a 1-dim irrep row
            fill-1d-row! (fn [row-idx rot-fn even-refl-val odd-refl-val]
                           (dotimes [j num-classes]
                             (let [rep (class-reps j)
                                   val (cond
                                         (= :r (first rep)) (double (rot-fn (second rep)))
                                         (= rep [:s 0]) (double even-refl-val)
                                         :else (double odd-refl-val))]
                               (aset ^doubles (re-rows row-idx) j val))))]
        ;; χ₁: all 1
        (fill-1d-row! 0 (constantly 1.0) 1.0 1.0)
        ;; χ₂: +1 on rotations, -1 on all reflections
        (fill-1d-row! 1 (constantly 1.0) -1.0 -1.0)
        ;; χ₃: (-1)^k on r^k, +1 on even-refl, -1 on odd-refl
        (fill-1d-row! 2 (fn [k] (Math/pow -1.0 (double k))) 1.0 -1.0)
        ;; χ₄: (-1)^k on r^k, -1 on even-refl, +1 on odd-refl
        (fill-1d-row! 3 (fn [k] (Math/pow -1.0 (double k))) -1.0 1.0)
        ;; 2-dim irreps χ_m for m=1..(n/2-1)
        (dotimes [mi num-2d]
          (let [m (inc mi)
                row-idx (+ 4 mi)]
            (dotimes [j num-classes]
              (let [rep (class-reps j)
                    val (cond
                          (= :r (first rep))
                          (* 2.0 (Math/cos (* angle m (double (second rep)))))
                          :else 0.0)]
                (aset ^doubles (re-rows row-idx) j val)))))
        {:group G
         :classes class-reps
         :class-sizes class-sizes
         :irrep-labels (vec (concat [:trivial :sign :sign-rot :sign-both]
                                    (mapv (fn [m] [:dim2 m]) (range 1 (inc num-2d)))))
         :table (cx/complex-tensor re-rows im-rows)}))))

(defmethod character-table :product
  [G]
  (let [g1 (:G1 G)
        g2 (:G2 G)
        ct1 (character-table g1)
        ct2 (character-table g2)
        t1 (:table ct1) t2 (:table ct2)
        n1 (count (:irrep-labels ct1))
        n2 (count (:irrep-labels ct2))
        c1 (count (:classes ct1))
        c2 (count (:classes ct2))
        num-irreps (* n1 n2)
        num-classes (* c1 c2)
        ;; Build rows via Kronecker product of character values
        ;; chi_{(i,j)}((a,b)) = chi_i(a) * chi_j(b) (complex multiply)
        re-rows (mapv (fn [ij]
                        (let [i (quot ij n2)
                              j (rem ij n2)
                              row (double-array num-classes)]
                          (dotimes [ab num-classes]
                            (let [a (quot ab c2)
                                  b (rem ab c2)
                                  ;; chi_i(a)
                                  r1 (cx/re ((t1 i) a))
                                  m1 (cx/im ((t1 i) a))
                                  ;; chi_j(b)
                                  r2 (cx/re ((t2 j) b))
                                  m2 (cx/im ((t2 j) b))
                                  ;; complex product: (r1+m1*i)(r2+m2*i)
                                  re (- (* r1 r2) (* m1 m2))]
                              (aset row ab re)))
                          row))
                      (range num-irreps))
        im-rows (mapv (fn [ij]
                        (let [i (quot ij n2)
                              j (rem ij n2)
                              row (double-array num-classes)]
                          (dotimes [ab num-classes]
                            (let [a (quot ab c2)
                                  b (rem ab c2)
                                  r1 (cx/re ((t1 i) a))
                                  m1 (cx/im ((t1 i) a))
                                  r2 (cx/re ((t2 j) b))
                                  m2 (cx/im ((t2 j) b))
                                  im (+ (* r1 m2) (* m1 r2))]
                              (aset row ab im)))
                          row))
                      (range num-irreps))]
    {:group G
     :classes (vec (for [a (:classes ct1) b (:classes ct2)] [a b]))
     :class-sizes (vec (for [s1 (:class-sizes ct1) s2 (:class-sizes ct2)] (* s1 s2)))
     :irrep-labels (vec (for [l1 (:irrep-labels ct1) l2 (:irrep-labels ct2)] [l1 l2]))
     :table (cx/complex-tensor re-rows im-rows)}))

;; ---------------------------------------------------------------------------
;; Character inner product
;; ---------------------------------------------------------------------------

(defn character-inner-product
  "Compute the inner product of two characters (or class functions).

   <chi, psi> = (1/|G|) * sum_{classes} |C_mu| * chi(mu) * conj(psi(mu))

   chi-vals and psi-vals are ComplexTensor vectors, one entry per class.
   class-sizes is a vector of class sizes.
   group-order is |G|.

   Returns a scalar ComplexTensor."
  [chi-vals psi-vals class-sizes group-order]
  (let [chi-re (cx/re chi-vals)
        chi-im (cx/im chi-vals)
        psi-re (cx/re psi-vals)
        psi-im (cx/im psi-vals)
        sz (double-array (map double class-sizes))
        ;; chi * conj(psi) = (a+bi)(c-di) = (ac+bd) + (bc-ad)i
        ;; weighted by class sizes, scaled by 1/|G|
        inv-order (/ 1.0 (double group-order))]
    (cx/complex (* inv-order (dfn/sum (dfn/* sz (dfn/+ (dfn/* chi-re psi-re)
                                                       (dfn/* chi-im psi-im)))))
                (* inv-order (dfn/sum (dfn/* sz (dfn/- (dfn/* chi-im psi-re)
                                                       (dfn/* chi-re psi-im))))))))
