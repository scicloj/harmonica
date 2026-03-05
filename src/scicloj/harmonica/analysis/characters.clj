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
            [scicloj.lalinea.tensor :as t]
            [scicloj.lalinea.elementwise :as el]
            [scicloj.lalinea.linalg :as la]
            [scicloj.kindly.v4.kind :as kind]))

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
        angle (/ (* 2.0 Math/PI) n)
        cos-vals (t/make-reader :float64 n (Math/cos (* (double idx) angle)))
        sin-vals (t/make-reader :float64 n (Math/sin (* (double idx) angle)))
        re-table (t/compute-tensor [n n] (fn [j k]
                                           (cos-vals (mod (* (long j) (long k)) n))) :float64)
        im-table (t/compute-tensor [n n] (fn [j k]
                                           (sin-vals (mod (* (long j) (long k)) n))) :float64)]
    {:group G
     :classes (vec (range n))
     :class-sizes (vec (repeat n 1))
     :irrep-labels (vec (range n))
     :table (t/complex-tensor re-table im-table)}))

(defmethod character-table :symmetric
  [G]
  (let [n (.-n G)
        parts (part/partitions n)
        classes (vec (reverse parts))
        num-classes (count classes)
        class-sizes (mapv #(part/partition-class-size n %) classes)
        re-table (t/compute-tensor
                  [(count parts) num-classes]
                  (fn [i j] (double (mn/chi (parts i) (classes j))))
                  :float64)]
    {:group G
     :classes classes
     :class-sizes class-sizes
     :irrep-labels parts
     :table (t/complex-tensor-real re-table)}))

(defmethod character-table :dihedral
  [G]
  (let [n (.-n G)
        classes (p/conjugacy-classes G)
        num-classes (count classes)
        class-sizes (mapv :size classes)
        class-reps (mapv :representative classes)]
    (if (odd? n)
      (let [num-2d (quot (dec n) 2)
            num-irreps (+ 2 num-2d)
            angle (/ (* 2.0 Math/PI) n)
            re-table (t/compute-tensor
                      [num-irreps num-classes]
                      (fn [row-idx j]
                        (let [rep (class-reps j)]
                          (cond
                            (= row-idx 0) 1.0
                            (= row-idx 1) (if (= :s (first rep)) -1.0 1.0)
                            :else
                            (let [m (- row-idx 1)]
                              (cond
                                (= rep [:r 0]) 2.0
                                (= :r (first rep))
                                (* 2.0 (Math/cos (* angle m (double (second rep)))))
                                :else 0.0)))))
                      :float64)]
        {:group G
         :classes class-reps
         :class-sizes class-sizes
         :irrep-labels (vec (concat [:trivial :sign]
                                    (mapv (fn [m] [:dim2 m]) (range 1 (inc num-2d)))))
         :table (t/complex-tensor-real re-table)})
      (let [half (quot n 2)
            num-2d (dec half)
            num-irreps (+ 4 num-2d)
            angle (/ (* 2.0 Math/PI) n)
            re-table (t/compute-tensor
                      [num-irreps num-classes]
                      (fn [row-idx j]
                        (let [rep (class-reps j)]
                          (cond
                            (= row-idx 0) 1.0
                            (= row-idx 1) (if (= :r (first rep)) 1.0 -1.0)
                            (= row-idx 2) (cond
                                            (= :r (first rep)) (if (even? (second rep)) 1.0 -1.0)
                                            (= rep [:s 0]) 1.0
                                            :else -1.0)
                            (= row-idx 3) (cond
                                            (= :r (first rep)) (if (even? (second rep)) 1.0 -1.0)
                                            (= rep [:s 0]) -1.0
                                            :else 1.0)
                            :else
                            (let [m (- row-idx 3)]
                              (if (= :r (first rep))
                                (* 2.0 (Math/cos (* angle m (double (second rep)))))
                                0.0)))))
                      :float64)]
        {:group G
         :classes class-reps
         :class-sizes class-sizes
         :irrep-labels (vec (concat [:trivial :sign :sign-rot :sign-both]
                                    (mapv (fn [m] [:dim2 m]) (range 1 (inc num-2d)))))
         :table (t/complex-tensor-real re-table)}))))

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
        ;; chi_{(i,j)}((a,b)) = chi_i(a) * chi_j(b) (complex multiply via el/*)
        re-table (t/compute-tensor
                  [num-irreps num-classes]
                  (fn [ij ab]
                    (let [i (quot ij n2) j (rem ij n2)
                          a (quot ab c2) b (rem ab c2)
                          prod (el/* ((t1 i) a) ((t2 j) b))]
                      (double (el/re prod))))
                  :float64)
        im-table (t/compute-tensor
                  [num-irreps num-classes]
                  (fn [ij ab]
                    (let [i (quot ij n2) j (rem ij n2)
                          a (quot ab c2) b (rem ab c2)
                          prod (el/* ((t1 i) a) ((t2 j) b))]
                      (double (el/im prod))))
                  :float64)]
    {:group G
     :classes (vec (for [a (:classes ct1) b (:classes ct2)] [a b]))
     :class-sizes (vec (for [s1 (:class-sizes ct1) s2 (:class-sizes ct2)] (* s1 s2)))
     :irrep-labels (vec (for [l1 (:irrep-labels ct1) l2 (:irrep-labels ct2)] [l1 l2]))
     :table (t/complex-tensor re-table im-table)}))

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
  (let [sz (t/complex-tensor-real class-sizes)
        weighted (el/* sz chi-vals)
        ip (la/dot weighted psi-vals)]
    (el/scale ip (/ 1.0 (double group-order)))))

(defn format-cx
  "Format a complex character value for display.
   Integers stay as integers, pure imaginary values display as i/-i/ni,
   and general complex values display as re+imi."
  [v]
  (let [re (el/re v) im (el/im v)
        near-int? (fn [x] (< (Math/abs (- x (Math/round x))) 1e-10))
        fmt (fn [x] (if (near-int? x) (long (Math/round x)) (format "%.3f" x)))]
    (cond
      (< (Math/abs im) 1e-10) (fmt re)
      (< (Math/abs re) 1e-10) (let [i (fmt im)]
                                (cond
                                  (= i 1) "i"
                                  (= i -1) "-i"
                                  :else (str i "i")))
      :else (let [r (fmt re) i (fmt im)]
              (if (neg? (if (number? im) im (Double/parseDouble (str im))))
                (str r (fmt im) "i")
                (str r "+" (fmt im) "i"))))))

(defn show-character-table
  "Display a character table as a kind/table with labeled rows and columns.
   Rows are irreducible representations, columns are conjugacy classes.
   Complex values are formatted for readability."
  [ct]
  (let [{:keys [table classes irrep-labels]} ct]
    (kind/table
     {:column-names (into [""] (mapv str classes))
      :row-vectors (mapv (fn [label row]
                           (into [(str label)]
                                 (mapv format-cx row)))
                         irrep-labels table)})))
