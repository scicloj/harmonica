(ns scicloj.harmonica.symmetric-test
  (:require [clojure.test :refer [deftest testing is are]]
            [scicloj.harmonica.core :as hm]
            [scicloj.harmonica.impl.permutation :as perm]
            [scicloj.harmonica.impl.partition :as part]))

;; ---------------------------------------------------------------------------
;; Permutation utilities
;; ---------------------------------------------------------------------------

(deftest permutation-composition
  (testing "compose is right-to-left: (σ∘τ)(i) = σ(τ(i))"
    ;; τ = (0 1 2), σ = (0 1) in S_3
    ;; τ: 0→1, 1→2, 2→0 so τ = [1 2 0]
    ;; σ: 0→1, 1→0 so σ = [1 0 2]
    ;; σ∘τ: 0→σ(1)=0, 1→σ(2)=2, 2→σ(0)=1 → [0 2 1] = (1 2)
    (is (= [0 2 1] (perm/compose [1 0 2] [1 2 0])))))

(deftest permutation-inverse
  (testing "σ∘σ⁻¹ = id"
    (doseq [sigma [[1 2 0] [1 0 2] [2 0 1 3] [3 2 1 0]]]
      (is (= (perm/identity-perm (count sigma))
             (perm/compose sigma (perm/inverse sigma)))))))

(deftest permutation-cycles
  (testing "cycle decomposition"
    (is (= [] (perm/cycles [0 1 2])))
    (is (= [[0 1]] (perm/cycles [1 0 2])))
    (is (= [[0 1 2]] (perm/cycles [1 2 0]))))

  (testing "from-cycles round-trip"
    (doseq [sigma [[1 2 0] [1 0 2] [2 0 3 1] [0 1 2 3]]]
      (is (= sigma
             (perm/from-cycles (count sigma) (perm/cycles sigma)))))))

(deftest permutation-cycle-type
  (testing "cycle type is a partition"
    (is (= [3] (perm/cycle-type [1 2 0])))
    (is (= [2 1] (perm/cycle-type [1 0 2])))
    (is (= [1 1 1] (perm/cycle-type [0 1 2])))))

(deftest permutation-sign
  (testing "identity has sign +1"
    (doseq [n [1 2 3 4]]
      (is (= 1 (perm/sign (perm/identity-perm n))))))

  (testing "transpositions have sign -1"
    (doseq [[n i j] [[2 0 1] [3 0 1] [3 1 2] [4 0 3]]]
      (is (= -1 (perm/sign (perm/transposition n i j))))))

  (testing "sign is multiplicative"
    (let [S3 (hm/symmetric-group 3)
          elts (vec (hm/elements S3))]
      (doseq [a elts b elts]
        (is (= (perm/sign (hm/op S3 a b))
               (* (perm/sign a) (perm/sign b))))))))

;; ---------------------------------------------------------------------------
;; Partitions
;; ---------------------------------------------------------------------------

(deftest partition-enumeration
  (testing "known partition counts"
    ;; p(0)=1, p(1)=1, p(2)=2, p(3)=3, p(4)=5, p(5)=7, p(6)=11, p(7)=15
    (is (= [1 1 2 3 5 7 11 15]
           (mapv #(count (hm/partitions %)) (range 8)))))

  (testing "all partitions are valid"
    (doseq [n (range 1 8)
            p (hm/partitions n)]
      (is (part/partition? p))
      (is (= n (reduce + p))))))

(deftest partition-conjugate
  (testing "conjugate of conjugate is identity"
    (doseq [n (range 1 7)
            p (hm/partitions n)]
      (is (= p (part/conjugate (part/conjugate p)))))))

;; ---------------------------------------------------------------------------
;; Symmetric group axioms
;; ---------------------------------------------------------------------------

(deftest symmetric-group-axioms
  (doseq [n [1 2 3 4]]
    (let [G (hm/symmetric-group n)
          elts (vec (hm/elements G))
          id (hm/id G)]
      (testing (str "S_" n " identity")
        (doseq [g elts]
          (is (= g (hm/op G g id)))
          (is (= g (hm/op G id g)))))

      (testing (str "S_" n " inverse")
        (doseq [g elts]
          (is (= id (hm/op G g (hm/inv G g))))
          (is (= id (hm/op G (hm/inv G g) g)))))

      (testing (str "S_" n " associativity (spot-check)")
        (let [sample (take 6 elts)]
          (doseq [a sample b sample c sample]
            (is (= (hm/op G (hm/op G a b) c)
                   (hm/op G a (hm/op G b c))))))))))

(deftest symmetric-group-order
  (testing "order = n!"
    ;; 1!=1, 2!=2, 3!=6, 4!=24, 5!=120
    (doseq [[n expected] [[1 1] [2 2] [3 6] [4 24] [5 120]]]
      (let [G (hm/symmetric-group n)]
        (is (= expected (hm/order G)))
        (when (<= n 4)
          (is (= expected (count (hm/elements G)))))))))

;; ---------------------------------------------------------------------------
;; Conjugacy classes
;; ---------------------------------------------------------------------------

(deftest conjugacy-class-count
  (testing "number of conjugacy classes = number of partitions"
    (doseq [n [1 2 3 4 5]]
      (let [G (hm/symmetric-group n)]
        (is (= (count (hm/partitions n))
               (count (hm/conjugacy-classes G))))))))

(deftest conjugacy-class-sizes
  (testing "class sizes sum to |G|"
    (doseq [n [1 2 3 4 5]]
      (let [G (hm/symmetric-group n)]
        (is (= (hm/order G)
               (reduce + (map :size (hm/conjugacy-classes G))))))))

  (testing "S_3 class sizes"
    (let [G (hm/symmetric-group 3)
          sizes (sort (map :size (hm/conjugacy-classes G)))]
      (is (= [1 2 3] sizes))))

  (testing "S_4 class sizes"
    (let [G (hm/symmetric-group 4)
          sizes (sort (map :size (hm/conjugacy-classes G)))]
      (is (= [1 3 6 6 8] sizes)))))

(deftest conjugacy-class-cycle-types
  (testing "cycle types match partitions of n"
    (doseq [n [2 3 4 5]]
      (let [G (hm/symmetric-group n)
            types (set (map :cycle-type (hm/conjugacy-classes G)))
            parts (set (hm/partitions n))]
        (is (= parts types))))))
