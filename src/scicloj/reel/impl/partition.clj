(ns scicloj.reel.impl.partition
  "Integer partitions.

   A partition of n is a vector of positive integers in descending order
   that sum to n. For example, [3 2 1] is a partition of 6.

   Partitions index the conjugacy classes of the symmetric group S_n:
   two permutations are conjugate iff they have the same cycle type,
   and cycle types are partitions.")

(defn partitions
  "All partitions of n, as descending vectors.
   Returns them in reverse lexicographic order."
  [n]
  (if (zero? n)
    [[]]
    (letfn [(gen [remaining max-part]
              (if (zero? remaining)
                [[]]
                (for [part (range (min remaining max-part) 0 -1)
                      rest (gen (- remaining part) part)]
                  (into [part] rest))))]
      (vec (gen n n)))))

(defn partition?
  "True if p is a valid partition: a non-empty vector of positive integers
   in descending order."
  [p]
  (and (vector? p)
       (pos? (count p))
       (every? pos-int? p)
       (apply >= p)))

(defn conjugate
  "The conjugate (transpose) of a partition.
   If the partition is drawn as a Young diagram, this reflects it
   along the main diagonal."
  [p]
  (if (empty? p)
    []
    (let [max-part (first p)]
      (mapv (fn [j]
              (count (filter #(> % j) p)))
            (range max-part)))))

(defn partition-class-size
  "Size of the conjugacy class in S_n with cycle type given by partition p.

   Formula: n! / (1^{a_1} * a_1! * 2^{a_2} * a_2! * ... * n^{a_n} * a_n!)

   where a_k is the number of parts equal to k."
  [n p]
  (let [factorial (fn [m] (reduce *' (range 1 (inc m))))
        freq (frequencies p)]
    (/ (factorial n)
       (reduce *' (map (fn [[k ak]]
                         (*' (reduce *' (repeat ak k))
                             (factorial ak)))
                       freq)))))
