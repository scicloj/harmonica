(ns scicloj.harmonica.combinatorics.young-tableaux
  "Standard Young Tableaux (SYT) for integer partitions.

   A Standard Young Tableau of shape λ is a filling of the Young diagram
   of λ with the numbers 1..n (each used once) such that entries increase
   along each row (left to right) and each column (top to bottom).

   SYTs index the basis vectors of the irreducible representations of S_n
   in Young's orthogonal form.

   Includes:
   - standard-young-tableaux: enumerate all SYTs of a given shape
   - hook-length-dimension: count SYTs using the hook-length formula
   - hook-lengths: compute the hook length at each cell"
  (:require [scicloj.harmonica.combinatorics.partition :as part]))

(defn addable-cells
  "Return the cells (as [row, col] pairs, 0-indexed) where a new entry
   can be added to a partial SYT of shape described by row-lengths.
   A cell is addable if placing an entry there still gives increasing
   rows and columns."
  [row-lengths num-rows]
  (let [result (transient [])]
    ;; Can extend an existing row i if row i is shorter than row i-1
    (dotimes [i num-rows]
      (let [len-i (row-lengths i)
            len-prev (if (zero? i) Long/MAX_VALUE (row-lengths (dec i)))]
        (when (< len-i len-prev)
          (conj! result [i len-i]))))
    ;; Can start a new row if num-rows < number of parts
    ;; Actually, we always allow adding a new row as long as it would be valid
    ;; (i.e., current last row has length > 0, and we haven't exceeded partition shape)
    ;; But we parameterize by the target partition shape, so this is handled by the caller.
    (persistent! result)))

(defn standard-young-tableaux
  "Enumerate all Standard Young Tableaux of shape λ (a partition).
   Returns a vector of SYTs, where each SYT is a vector of row vectors.

   Example: (standard-young-tableaux [2 1])
   => [[[1 2] [3]] [[1 3] [2]]]"
  [lam]
  (let [n (reduce + 0 lam)
        num-parts (count lam)]
    (if (zero? n)
      [[]]
      (let [results (java.util.ArrayList.)]
        (letfn [(generate [val ;; next value to place (1-indexed)
                           rows ;; current state: vector of row vectors
                           row-lengths] ;; int array of current row lengths
                  (if (> val n)
                    (.add results (mapv vec rows))
                    ;; Try each row where we can place val
                    (dotimes [i num-parts]
                      (let [cur-len (aget ^ints row-lengths i)
                            max-len (long (lam i))
                            prev-len (if (zero? i)
                                       Long/MAX_VALUE
                                       (aget ^ints row-lengths (dec i)))]
                        ;; Can place in row i if:
                        ;; 1. Row i is not full (cur-len < lam[i])
                        ;; 2. Row i is shorter than row i-1 (ensures column-increasing)
                        (when (and (< cur-len max-len)
                                   (< cur-len prev-len))
                          (aset ^ints row-lengths i (unchecked-inc-int cur-len))
                          (let [row-i (rows i)]
                            (generate (inc val)
                                      (assoc rows i (conj row-i val))
                                      row-lengths))
                          (aset ^ints row-lengths i cur-len))))))]
          (let [initial-rows (vec (repeat num-parts []))
                initial-lengths (int-array num-parts)]
            (generate 1 initial-rows initial-lengths)))
        (vec results)))))

(defn position-of
  "Given a SYT (vector of row vectors) and a value v (1-indexed),
   return [row, col] (0-indexed) where v appears."
  [syt v]
  (let [num-rows (count syt)]
    (loop [r 0]
      (when (< r num-rows)
        (let [row (syt r)
              c (.indexOf ^clojure.lang.PersistentVector row v)]
          (if (>= c 0)
            [r c]
            (recur (inc r))))))))

(defn content
  "The content of cell (row, col) in a Young diagram: col - row.
   This is the key quantity for Young's orthogonal form."
  [row col]
  (- col row))

(defn axial-distance
  "The axial distance between values i and j in SYT T.
   This equals content(pos(j)) - content(pos(i))
   = (col_j - row_j) - (col_i - row_i).

   The sign convention: positive when j is to the lower-right of i."
  [syt i j]
  (let [[ri ci] (position-of syt i)
        [rj cj] (position-of syt j)]
    (- (content rj cj) (content ri ci))))

(defn hook-length
  "The hook length at cell (i,j) in partition lambda (0-indexed).
   h(i,j) = (lambda_i - j) + (lambda'_j - i) - 1
   where lambda' is the conjugate partition.
   (The -1 accounts for 0-based indexing.)"
  [lam conj-lam i j]
  (+ (- (long (lam i)) j)
     (- (long (conj-lam j)) i)
     -1))

(defn hook-length-dimension
  "Dimension of the irrep of S_n corresponding to partition lambda,
   computed via the hook-length formula: d_lambda = n! / product of hook lengths."
  [lam]
  (let [n (reduce + 0 lam)
        conj-lam (part/conjugate lam)
        factorial (fn [m] (reduce *' (range 1 (inc m))))
        hook-product (reduce *'
                             (for [i (range (count lam))
                                   j (range (lam i))]
                               (hook-length lam conj-lam i j)))]
    (/ (factorial n) hook-product)))

(defn syt-count
  "Number of standard Young tableaux of shape lambda.
   This equals the dimension of the corresponding irrep of S_n."
  [lam]
  (hook-length-dimension lam))
