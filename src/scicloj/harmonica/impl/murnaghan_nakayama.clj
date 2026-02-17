(ns scicloj.harmonica.impl.murnaghan-nakayama
  "Murnaghan-Nakayama rule for computing characters of symmetric groups.

   Computes χ_λ(μ) — the character of the irreducible representation
   labeled by partition λ, evaluated at the conjugacy class with cycle
   type μ.

   Uses the partition sequence (BitVector) encoding from:
   Dan Bernstein, 'The computational complexity of rules for the
   character table of Sn', J. Symbolic Computation 37(6), 2004.

   A partition is encoded as a boolean vector by tracing the lower
   contour of its Young diagram from left to right: true for each
   horizontal step, false for each vertical step. Rim hooks of length k
   at position i exist when seq[i]=true and seq[i+k]=false. Removing a
   rim hook is a swap of those two entries.")

(defn partition-seq
  "Encode a partition as a boolean vector (partition sequence).
   Traces the lower contour of the Young diagram: true for horizontal
   steps, false for vertical steps. Always starts with true and ends
   with false."
  [lambda]
  (if (empty? lambda)
    (boolean-array 0)
    (let [n (count lambda)
          max-part (first lambda)
          len (+ max-part n)
          seq (boolean-array len)]
      ;; Fill with true initially
      (java.util.Arrays/fill seq true)
      ;; Place false entries at vertical steps.
      ;; Working from bottom row up (reversed partition):
      ;; row widths give horizontal steps, then one vertical step.
      (let [rev-lambda (vec (reverse lambda))]
        (loop [pos 0
               i 0]
          (when (< i n)
            (let [width (if (zero? i)
                          (rev-lambda i)
                          (- (rev-lambda i) (rev-lambda (dec i))))]
              (aset seq (int (+ pos width)) false)
              (recur (+ pos width 1) (inc i))))))
      seq)))

(defn- essential-seq
  "Extract the essential part of a partition sequence: from first true
   to last false."
  [^booleans seq]
  (let [len (alength seq)]
    (if (zero? len)
      seq
      (let [first-true (loop [i 0]
                         (cond
                           (>= i len) -1
                           (aget seq i) i
                           :else (recur (inc i))))
            last-false (loop [i (dec len)]
                         (cond
                           (< i 0) -1
                           (not (aget seq i)) i
                           :else (recur (dec i))))]
        (if (or (neg? first-true) (neg? last-false) (> first-true last-false))
          (boolean-array 0)
          (java.util.Arrays/copyOfRange seq (int first-true) (int (inc last-false))))))))

(defn- rim-hook?
  "Check if a rim hook of length len exists at position idx in the
   partition sequence R."
  [^booleans R idx len]
  (let [end (+ idx len)]
    (and (< end (alength R))
         (aget R idx)
         (not (aget R end)))))

(defn chi
  "Compute χ_λ(μ) — character of irrep λ at conjugacy class μ.
   Both lambda and mu are partitions (descending vectors of positive ints).
   Returns a long integer."
  [lambda mu]
  (let [cache (atom {})]
    (letfn [(mn-inner [^booleans R mu-parts t]
              (if (>= t (count mu-parts))
                1
                (let [part-len (mu-parts t)
                      rlen (alength R)]
                  (if (> part-len rlen)
                    0
                    (let [result (volatile! 0)
                          sgn (volatile! false)]
                      ;; Count false entries in R[0..part-len-2] for initial sign
                      (dotimes [j (dec part-len)]
                        (when (not (aget R j))
                          (vswap! sgn not)))
                      ;; Scan for rim hooks
                      (dotimes [i (- rlen part-len)]
                        ;; Update sign: R[i] != R[i+part-len-1] flips sign
                        (when (not= (aget R i) (aget R (+ i part-len -1)))
                          (vswap! sgn not))
                        (when (rim-hook? R i part-len)
                          ;; Swap to remove rim hook
                          (aset R i false)
                          (aset R (+ i part-len) true)
                          ;; Compute key for cache
                          (let [ess (essential-seq R)
                                remaining (subvec mu-parts (inc t))
                                cache-key [(vec ess) remaining]
                                sub-val (if-let [cached (get @cache cache-key)]
                                          cached
                                          (let [v (mn-inner ess remaining 0)]
                                            (swap! cache assoc cache-key v)
                                            v))]
                            (if @sgn
                              (vswap! result - sub-val)
                              (vswap! result + sub-val)))
                          ;; Restore R
                          (aset R i true)
                          (aset R (+ i part-len) false)))
                      @result)))))]
      (if (and (empty? lambda) (empty? mu))
        1
        (if (or (empty? lambda) (empty? mu))
          0
          (if (not= (reduce + lambda) (reduce + mu))
            0
            (mn-inner (partition-seq lambda) (vec mu) 0)))))))
