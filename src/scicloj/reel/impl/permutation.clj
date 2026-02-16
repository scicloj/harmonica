(ns scicloj.reel.impl.permutation
  "Permutation utilities using 0-indexed one-line notation.

   A permutation of n elements is a vector [σ(0) σ(1) ... σ(n-1)]
   where each σ(i) is an element of {0, 1, ..., n-1}.

   Composition follows standard mathematical convention (right-to-left):
   (compose sigma tau) = σ∘τ where (σ∘τ)(i) = σ(τ(i)).")

(defn identity-perm
  "The identity permutation of {0, ..., n-1}."
  [n]
  (vec (range n)))

(defn compose
  "Compose two permutations: (σ∘τ)(i) = σ(τ(i)).
   Both must have the same length."
  [sigma tau]
  (mapv sigma tau))

(defn inverse
  "Inverse permutation: if σ(i) = j, then σ⁻¹(j) = i."
  [sigma]
  (let [n (count sigma)
        result (int-array n)]
    (dotimes [i n]
      (aset result (int (sigma i)) i))
    (vec result)))

(defn cycles
  "Cycle decomposition of a permutation.
   Returns a vector of cycles, each a vector of elements.
   Fixed points (1-cycles) are omitted."
  [sigma]
  (let [n (count sigma)
        visited (boolean-array n)]
    (loop [i 0
           result (transient [])]
      (if (< i n)
        (if (aget visited i)
          (recur (inc i) result)
          (let [cycle (loop [j i
                             c (transient [])]
                        (aset visited j true)
                        (let [c (conj! c j)
                              next-j (int (sigma j))]
                          (if (= next-j i)
                            (persistent! c)
                            (recur next-j c))))]
            (recur (inc i)
                   (if (> (count cycle) 1)
                     (conj! result cycle)
                     result))))
        (persistent! result)))))

(defn from-cycles
  "Construct a permutation from cycle notation.
   n is the degree (size of the set), cycles is a collection of cycles
   (each a sequence of elements)."
  [n cycles]
  (let [result (vec (range n))]
    (reduce (fn [perm cycle]
              (let [cycle (vec cycle)
                    len (count cycle)]
                (reduce (fn [p i]
                          (assoc p (cycle i) (cycle (mod (inc i) len))))
                        perm
                        (range len))))
            result
            cycles)))

(defn cycle-type
  "The cycle type of a permutation as a partition (descending sorted).
   Includes 1-cycles (fixed points)."
  [sigma]
  (let [n (count sigma)
        cs (cycles sigma)
        explicit-lengths (mapv count cs)
        fixed-point-count (- n (reduce + 0 explicit-lengths))]
    (vec (sort > (into explicit-lengths (repeat fixed-point-count 1))))))

(defn sign
  "The sign (parity) of a permutation: +1 for even, -1 for odd.
   A permutation is even iff it has an even number of even-length cycles."
  [sigma]
  (let [cs (cycles sigma)
        n-transpositions (reduce + (map #(dec (count %)) cs))]
    (if (even? n-transpositions) 1 -1)))

(defn transposition
  "The transposition that swaps i and j in {0, ..., n-1}."
  [n i j]
  (-> (identity-perm n)
      (assoc i j)
      (assoc j i)))
