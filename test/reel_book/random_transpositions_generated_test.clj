(ns
 reel-book.random-transpositions-generated-test
 (:require
  [scicloj.reel.core :as reel]
  [scicloj.reel.impl.partition :as part]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l31
 (let
  [ct (reel/character-table (reel/symmetric-group 5))]
  (kind/table
   {:column-names (into ["Irrep λ"] (map str (:classes ct))),
    :row-vectors
    (mapv
     (fn
      [label row]
      (into
       [(str label)]
       (map (fn* [p1__63701#] (long (.-x p1__63701#))) row)))
     (:irrep-labels ct)
     (:table ct))})))


(def
 v5_l49
 (let
  [ct
   (reel/character-table (reel/symmetric-group 5))
   table
   (:table ct)
   sizes
   (:class-sizes ct)
   order
   120]
  (kind/table
   {:column-names (into [""] (map str (:irrep-labels ct))),
    :row-vectors
    (mapv
     (fn
      [i]
      (into
       [(str ((:irrep-labels ct) i))]
       (map
        (fn
         [j]
         (let
          [v
           (reel/character-inner-product
            (table i)
            (table j)
            sizes
            order)]
          (format "%.0f" (.-x v))))
        (range (count table)))))
     (range (count table)))})))


(def
 v7_l102
 (defn
  n-stat
  "n(lambda) = sum_i C(lambda_i, 2) = sum_i lambda_i*(lambda_i-1)/2."
  [lambda]
  (reduce + (map (fn [p] (/ (* p (dec p)) 2)) lambda))))


(def
 v8_l107
 (defn
  eigenvalue
  "Eigenvalue of the random transposition operator on irrep lambda of S_n."
  [n lambda]
  (let
   [M (inc (/ (* n (dec n)) 2))]
   (/
    (double (+ 1 (n-stat lambda) (- (n-stat (part/conjugate lambda)))))
    (double M)))))


(def
 v9_l114
 (defn
  hook-length-dim
  "Dimension of irrep lambda via the hook-length formula: n! / prod h(i,j)."
  [lambda]
  (if
   (empty? lambda)
   1
   (let
    [n (reduce + lambda) conj (part/conjugate lambda)]
    (/
     (reduce *' (range 1 (inc n)))
     (reduce
      *'
      (for
       [i (range (count lambda)) j (range (lambda i))]
       (+ (- (lambda i) j) (- (conj j) i) -1))))))))


(def
 v11_l131
 (let
  [n
   5
   ct
   (reel/character-table (reel/symmetric-group n))
   table
   (:table ct)
   M
   (inc (/ (* n (dec n)) 2))
   trans-idx
   (.indexOf (:classes ct) (into [2] (repeat (- n 2) 1)))]
  (kind/table
   {:column-names ["lambda" "d" "beta (table)" "beta (closed)"],
    :row-vectors
    (mapv
     (fn
      [i]
      (let
       [lam
        ((:irrep-labels ct) i)
        d
        (long (.-x ((table i) 0)))
        chi-t
        (long (.-x ((table i) trans-idx)))
        from-table
        (/ (+ 1.0 (* (/ (* n (dec n)) 2) (/ (double chi-t) d))) M)]
       [(str lam)
        d
        (format "%.4f" from-table)
        (format "%.4f" (eigenvalue n lam))]))
     (range (count table)))})))


(deftest
 t12_l148
 (is
  ((fn
    [_]
    (let
     [n
      5
      ct
      (reel/character-table (reel/symmetric-group n))
      table
      (:table ct)
      M
      (inc (/ (* n (dec n)) 2))
      trans-idx
      (.indexOf (:classes ct) (into [2] (repeat (- n 2) 1)))]
     (every?
      (fn
       [i]
       (let
        [lam
         ((:irrep-labels ct) i)
         d
         (.-x ((table i) 0))
         chi-t
         (.-x ((table i) trans-idx))
         from-table
         (/ (+ 1.0 (* (/ (* n (dec n)) 2) (/ chi-t d))) M)]
        (< (Math/abs (- from-table (eigenvalue n lam))) 1.0E-10)))
      (range (count table)))))
   v11_l131)))


(def
 v14_l178
 (defn
  tv-upper-bound
  "Upper bound on ||Q^{*k} - U||_TV via Diaconis's Upper Bound Lemma."
  [partitions-data k]
  (let
   [ub
    (reduce
     +
     (map
      (fn
       [{:keys [dim eigenvalue]}]
       (* dim dim (Math/pow (Math/abs (double eigenvalue)) (* 2 k))))
      partitions-data))]
   (min 1.0 (* 0.5 (Math/sqrt ub))))))


(def
 v16_l191
 (let
  [ns-to-plot
   [10 20 30 40]
   rows
   (vec
    (for
     [n
      ns-to-plot
      :let
      [parts
       (reel/partitions n)
       data
       (mapv
        (fn
         [p]
         {:dim (double (hook-length-dim p)),
          :eigenvalue (eigenvalue n p)})
        (rest parts))
       cutoff
       (* 0.5 n (Math/log n))
       k-max
       (long (* 3 cutoff))]
      k
      (range 0 (inc k-max) (max 1 (long (/ k-max 100))))]
     {:n (str "n=" n), :k k, :tv-bound (tv-upper-bound data k)}))]
  (->
   (tc/dataset rows)
   (plotly/base {:=x :k, :=y :tv-bound, :=color :n})
   (plotly/layer-line)
   (plotly/update-data
    (fn
     [d]
     (assoc
      d
      :=layout
      {:title "Random Transpositions: TV Distance Upper Bound",
       :xaxis {:title "Steps (k)"},
       :yaxis {:title "TV upper bound", :range [0 1.05]}})))
   plotly/plot)))


(def
 v18_l219
 (let
  [ns-to-plot
   [10 20 30 40]
   rows
   (vec
    (for
     [n
      ns-to-plot
      :let
      [parts
       (reel/partitions n)
       data
       (mapv
        (fn
         [p]
         {:dim (double (hook-length-dim p)),
          :eigenvalue (eigenvalue n p)})
        (rest parts))
       cutoff
       (* 0.5 n (Math/log n))
       k-max
       (long (* 3 cutoff))]
      k
      (range 0 (inc k-max) (max 1 (long (/ k-max 100))))]
     {:n (str "n=" n),
      :k-normalized (/ (double k) cutoff),
      :tv-bound (tv-upper-bound data k)}))]
  (->
   (tc/dataset rows)
   (plotly/base {:=x :k-normalized, :=y :tv-bound, :=color :n})
   (plotly/layer-line)
   (plotly/update-data
    (fn
     [d]
     (assoc
      d
      :=layout
      {:title "Cutoff at (1/2)*n*ln(n)",
       :xaxis {:title "k / ((1/2)*n*ln(n))"},
       :yaxis {:title "TV upper bound", :range [0 1.05]}})))
   plotly/plot)))


(def
 v20_l244
 (kind/table
  {:column-names ["n" "|S_n|" "(1/2)*n*ln(n)" "# partitions"],
   :row-vectors
   (mapv
    (fn
     [n]
     [n
      (str (reduce *' (range 1 (inc n))))
      (format "%.1f" (* 0.5 n (Math/log n)))
      (count (reel/partitions n))])
    [5 10 15 20 30 40 52])}))
