(ns
 harmonica-book.random-transpositions-generated-test
 (:require
  [scicloj.harmonica :as hm]
  [scicloj.harmonica.linalg.complex :as cx]
  [harmonica-book.book-helpers :refer [allclose?]]
  [tablecloth.api :as tc]
  [scicloj.tableplot.v1.plotly :as plotly]
  [scicloj.kindly.v4.kind :as kind]
  [clojure.test :refer [deftest is]]))


(def
 v3_l32
 (let
  [ct (hm/character-table (hm/symmetric-group 5))]
  (kind/table
   {:column-names
    (into
     ["Irrep $\\lambda$"]
     (map (fn* [p1__117684#] (str p1__117684#)) (:classes ct))),
    :row-vectors
    (mapv
     (fn
      [label row]
      (into
       [(str label)]
       (map (fn* [p1__117685#] (long (cx/re p1__117685#))) row)))
     (:irrep-labels ct)
     (:table ct))})))


(def
 v5_l44
 (count (:irrep-labels (hm/character-table (hm/symmetric-group 5)))))


(deftest t6_l46 (is (= v5_l44 7)))


(def
 v8_l56
 (let
  [ct
   (hm/character-table (hm/symmetric-group 5))
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
           (hm/character-inner-product
            (table i)
            (table j)
            sizes
            order)]
          (format "%.0f" (cx/re v))))
        (range (count table)))))
     (range (count table)))})))


(def
 v10_l73
 (let
  [ct
   (hm/character-table (hm/symmetric-group 5))
   table
   (:table ct)
   sizes
   (:class-sizes ct)
   k
   (count (:irrep-labels ct))
   errors
   (double-array
    (for
     [i (range k) j (range k)]
     (-
      (cx/re
       (hm/character-inner-product (table i) (table j) sizes 120))
      (if (= i j) 1.0 0.0))))]
  (allclose? errors 0.0)))


(deftest t11_l84 (is (true? v10_l73)))


(def
 v13_l123
 (defn
  n-stat
  "n(lambda) = sum_i C(lambda_i, 2) = sum_i lambda_i*(lambda_i-1)/2."
  [lambda]
  (reduce + (map (fn [p] (/ (* p (dec p)) 2)) lambda))))


(def
 v14_l128
 (defn
  eigenvalue
  "Eigenvalue of the random transposition operator on irrep lambda of S_n."
  [n lambda]
  (let
   [M (inc (/ (* n (dec n)) 2))]
   (/
    (double
     (+
      1
      (n-stat lambda)
      (- (n-stat (hm/partition-conjugate lambda)))))
    (double M)))))


(def
 v15_l135
 (defn
  hook-length-dim
  "Dimension of irrep lambda via the hook-length formula: n! / prod h(i,j)."
  [lambda]
  (if
   (empty? lambda)
   1
   (let
    [n (reduce + lambda) conj (hm/partition-conjugate lambda)]
    (/
     (reduce *' (range 1 (inc n)))
     (reduce
      *'
      (for
       [i (range (count lambda)) j (range (lambda i))]
       (+ (- (lambda i) j) (- (conj j) i) -1))))))))


(def
 v17_l152
 (let
  [n
   5
   ct
   (hm/character-table (hm/symmetric-group n))
   table
   (:table ct)
   M
   (inc (/ (* n (dec n)) 2))
   trans-idx
   (.indexOf (:classes ct) (into [2] (repeat (- n 2) 1)))]
  (kind/table
   {:column-names
    ["$\\lambda$"
     "$d_\\lambda$"
     "$\\beta$ (table)"
     "$\\beta$ (closed)"],
    :row-vectors
    (mapv
     (fn
      [i]
      (let
       [lam
        ((:irrep-labels ct) i)
        d
        (long (cx/re ((table i) 0)))
        chi-t
        (long (cx/re ((table i) trans-idx)))
        from-table
        (/ (+ 1.0 (* (/ (* n (dec n)) 2) (/ (double chi-t) d))) M)]
       [(str lam)
        d
        (format "%.4f" from-table)
        (format "%.4f" (eigenvalue n lam))]))
     (range (count table)))})))


(deftest
 t18_l169
 (is
  ((fn
    [_]
    (let
     [n
      5
      ct
      (hm/character-table (hm/symmetric-group n))
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
         (cx/re ((table i) 0))
         chi-t
         (cx/re ((table i) trans-idx))
         from-table
         (/ (+ 1.0 (* (/ (* n (dec n)) 2) (/ chi-t d))) M)]
        (< (Math/abs (- from-table (eigenvalue n lam))) 1.0E-10)))
      (range (count table)))))
   v17_l152)))


(def v20_l191 (eigenvalue 5 [5]))


(deftest t21_l193 (is (= v20_l191 1.0)))


(def v23_l197 (eigenvalue 5 [1 1 1 1 1]))


(deftest
 t24_l199
 (is ((fn [v] (< (Math/abs (- v (/ -9.0 11.0))) 1.0E-10)) v23_l197)))


(def
 v26_l213
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
 v28_l226
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
       (hm/partitions n)
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
 v30_l251
 (let
  [n
   10
   parts
   (hm/partitions n)
   data
   (mapv
    (fn
     [p]
     {:dim (double (hook-length-dim p)), :eigenvalue (eigenvalue n p)})
    (rest parts))]
  (> (tv-upper-bound data 1) 0.5)))


(deftest t31_l258 (is (true? v30_l251)))


(def
 v33_l262
 (let
  [n
   10
   parts
   (hm/partitions n)
   data
   (mapv
    (fn
     [p]
     {:dim (double (hook-length-dim p)), :eigenvalue (eigenvalue n p)})
    (rest parts))]
  (< (tv-upper-bound data 100) 0.01)))


(deftest t34_l269 (is (true? v33_l262)))


(def
 v36_l276
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
       (hm/partitions n)
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
      {:title "Cutoff at k = (1/2) n ln(n)",
       :xaxis {:title "k / ((1/2) n ln n)"},
       :yaxis {:title "TV upper bound", :range [0 1.05]}})))
   plotly/plot)))


(def
 v38_l302
 (kind/table
  {:column-names
   ["$n$" "$|S_n|$" "$\\tfrac{1}{2}n\\ln n$" "# partitions"],
   :row-vectors
   (mapv
    (fn
     [n]
     [n
      (str (reduce *' (range 1 (inc n))))
      (format "%.1f" (* 0.5 n (Math/log n)))
      (count (hm/partitions n))])
    [5 10 15 20 30 40 52])}))
