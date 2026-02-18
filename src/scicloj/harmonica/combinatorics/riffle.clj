(ns scicloj.harmonica.combinatorics.riffle
  "Gilbert-Shannon-Reeds (GSR) riffle shuffle distribution.

   The GSR model: cut the deck binomially, then interleave the two packets
   preserving relative order within each packet.

   After k riffle shuffles of an n-card deck, the probability of
   permutation σ is:

     Q_a(σ) = C(a + n - r(σ), n) / a^n

   where a = 2^k and r(σ) = 1 + number of descents in σ^{-1}.

   References:
   - Bayer & Diaconis (1992), 'Trailing the dovetail shuffle to its lair'
   - Diaconis (1988), 'Group representations in probability and statistics'"
  (:require [scicloj.harmonica.combinatorics.permutation :as perm]
            [tech.v3.datatype :as dtype]))

(defn descents
  "Number of descents of a permutation σ.
   A descent at position i means σ(i) > σ(i+1) (0-indexed)."
  [sigma]
  (let [n (count sigma)]
    (loop [i 0 cnt 0]
      (if (>= i (dec n))
        cnt
        (recur (inc i)
               (if (> (long (sigma i)) (long (sigma (inc i))))
                 (inc cnt)
                 cnt))))))

(defn rising-sequences
  "Number of rising sequences of σ.
   r(σ) = 1 + descents(σ⁻¹)."
  [sigma]
  (inc (descents (perm/inverse sigma))))

(defn- binomial-coeff
  "Binomial coefficient C(n, k) using exact integer arithmetic."
  [n k]
  (cond
    (neg? k) 0
    (> k n) 0
    :else
    (let [k (min k (- n k))]
      (loop [i 0 result 1N]
        (if (>= i k)
          result
          (recur (inc i)
                 (/ (* result (- n i))
                    (inc i))))))))

(defn gsr-probability
  "Probability of permutation σ after k riffle shuffles of an n-card deck.

   Q_a(σ) = C(a + n - r(σ), n) / a^n

   where a = 2^k, r(σ) = number of rising sequences."
  [sigma k]
  (let [n (count sigma)
        a (bit-shift-left 1 k) ;; 2^k
        r (rising-sequences sigma)]
    (if (> r a)
      0.0 ;; impossible: at most a rising sequences after a-shuffle
      (double (/ (binomial-coeff (+ a n (- r)) n)
                 (reduce *' (repeat n (long a))))))))

(defn gsr-distribution-vec
  "Compute GSR probabilities for a given vector of permutations.
   Returns a dtype buffer of probabilities in the same order."
  [perms k]
  (dtype/emap (fn [p] (gsr-probability p k)) :float64 perms))
