(ns scicloj.reel.characters-test
  (:require [clojure.test :refer [deftest testing is are]]
            [scicloj.reel.core :as reel]
            [scicloj.reel.impl.murnaghan-nakayama :as mn]
            [scicloj.reel.impl.partition :as part]
            [fastmath.complex :as c])
  (:import [fastmath.vector Vec2]))

;; ---------------------------------------------------------------------------
;; Murnaghan-Nakayama rule: known character tables
;; ---------------------------------------------------------------------------

(deftest mn-s3-character-table
  (testing "S_3 character table matches known values"
    (let [parts [[3] [2 1] [1 1 1]]
          classes [[1 1 1] [2 1] [3]]
          ;; Known table:
          ;; [3]:       1  1  1
          ;; [2,1]:     2  0 -1
          ;; [1,1,1]:   1 -1  1
          expected [[1 1 1] [2 0 -1] [1 -1 1]]]
      (doseq [i (range 3)
              j (range 3)]
        (is (= (get-in expected [i j]) (mn/chi (parts i) (classes j)))
            (str "chi_" (parts i) "(" (classes j) ")"))))))

(deftest mn-s4-character-table
  (testing "S_4 character table matches known values"
    (let [parts [[4] [3 1] [2 2] [2 1 1] [1 1 1 1]]
          classes [[1 1 1 1] [2 1 1] [2 2] [3 1] [4]]
          expected [[1  1  1  1  1]
                    [3  1 -1  0 -1]
                    [2  0  2 -1  0]
                    [3 -1 -1  0  1]
                    [1 -1  1  1 -1]]]
      (doseq [i (range 5)
              j (range 5)]
        (is (= (get-in expected [i j]) (mn/chi (parts i) (classes j)))
            (str "chi_" (parts i) "(" (classes j) ")"))))))

(deftest mn-s5-character-table
  (testing "S_5 character table matches known values"
    (let [parts [[5] [4 1] [3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]]
          classes [[1 1 1 1 1] [2 1 1 1] [2 2 1] [3 1 1] [3 2] [4 1] [5]]
          expected [[1  1  1  1  1  1  1]
                    [4  2  0  1 -1  0 -1]
                    [5  1  1 -1  1 -1  0]
                    [6  0 -2  0  0  0  1]
                    [5 -1  1 -1 -1  1  0]
                    [4 -2  0  1  1  0 -1]
                    [1 -1  1  1 -1 -1  1]]]
      (doseq [i (range 7)
              j (range 7)]
        (is (= (get-in expected [i j]) (mn/chi (parts i) (classes j)))
            (str "chi_" (parts i) "(" (classes j) ")"))))))

;; ---------------------------------------------------------------------------
;; Character table structure via public API
;; ---------------------------------------------------------------------------

(deftest symmetric-character-table-structure
  (doseq [n (range 2 7)]
    (let [G (reel/symmetric-group n)
          ct (reel/character-table G)
          num-classes (count (reel/partitions n))]
      (testing (str "S_" n " character table is square")
        (is (= num-classes (count (:irrep-labels ct))))
        (is (= num-classes (count (:classes ct))))
        (is (= num-classes (count (:table ct))))
        (doseq [row (:table ct)]
          (is (= num-classes (count row))))))))

(deftest symmetric-trivial-character
  (testing "trivial character (partition [n]) is all 1s"
    (doseq [n (range 2 7)]
      (let [G (reel/symmetric-group n)
            ct (reel/character-table G)
            trivial-row (first (:table ct))]
        (doseq [v trivial-row]
          (is (< (Math/abs (- (.-x ^Vec2 v) 1.0)) 1e-10)
              (str "trivial character entry should be 1 for S_" n)))))))

(deftest symmetric-sign-character
  (testing "sign character (partition [1^n]) alternates by parity"
    (doseq [n (range 2 7)]
      (let [G (reel/symmetric-group n)
            ct (reel/character-table G)
            sign-row (last (:table ct))
            classes (:classes ct)]
        (doseq [[v cls] (map vector sign-row classes)]
          (let [expected (if (even? (- n (count cls))) 1.0 -1.0)]
            (is (< (Math/abs (- (.-x ^Vec2 v) expected)) 1e-10)
                (str "sign character at " cls " for S_" n))))))))

(deftest symmetric-dimensions
  (testing "first column (identity class) gives irrep dimensions"
    (doseq [n (range 2 7)]
      (let [G (reel/symmetric-group n)
            ct (reel/character-table G)
            dims (mapv (fn [row] (long (.-x ^Vec2 (first row)))) (:table ct))]
        (doseq [d dims]
          (is (pos? d) (str "dimension should be positive for S_" n)))
        (is (= (reel/order G)
               (reduce + (map #(* % %) dims)))
            (str "sum d_i^2 = " n "!"))))))

(deftest symmetric-row-orthogonality
  (testing "row orthogonality: <chi_i, chi_j> = delta_ij"
    (doseq [n [3 4 5]]
      (let [G (reel/symmetric-group n)
            ct (reel/character-table G)
            table (:table ct)
            sizes (:class-sizes ct)
            order (reel/order G)
            k (count table)]
        (doseq [i (range k)
                j (range i (min (+ i 3) k))]
          (let [ip (c/abs (reel/character-inner-product
                           (table i) (table j) sizes order))]
            (if (= i j)
              (is (< (Math/abs (- ip 1.0)) 1e-8)
                  (str "norm of chi_" i " should be 1 in S_" n))
              (is (< ip 1e-8)
                  (str "chi_" i " and chi_" j " should be orthogonal in S_" n)))))))))

(deftest symmetric-column-orthogonality
  (testing "column orthogonality: sum_lambda chi_lambda(mu) chi_lambda(nu) = |G|/|C_mu| delta_{mu,nu}"
    (doseq [n [3 4 5]]
      (let [G (reel/symmetric-group n)
            ct (reel/character-table G)
            table (:table ct)
            sizes (:class-sizes ct)
            order (reel/order G)
            k (count table)]
        (doseq [mu-idx (range k)
                nu-idx (range mu-idx (min (+ mu-idx 3) k))]
          (let [col-prod (reduce + (map (fn [row]
                                          (let [^Vec2 a (row mu-idx)
                                                ^Vec2 b (row nu-idx)]
                                            (* (.-x a) (.-x b))))
                                        table))]
            (if (= mu-idx nu-idx)
              (is (< (Math/abs (- col-prod (/ (double order) (double (sizes mu-idx))))) 1e-8)
                  (str "column orthogonality diagonal for class " mu-idx " in S_" n))
              (is (< (Math/abs col-prod) 1e-8)
                  (str "column orthogonality off-diagonal " mu-idx "," nu-idx " in S_" n)))))))))

(deftest symmetric-class-sizes-sum
  (testing "class sizes in character table sum to n!"
    (doseq [n (range 2 7)]
      (let [G (reel/symmetric-group n)
            ct (reel/character-table G)]
        (is (= (reel/order G)
               (reduce + (:class-sizes ct)))
            (str "class sizes sum to " n "!"))))))
