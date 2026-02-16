(ns scicloj.reel.protocols-test
  (:require [clojure.test :refer [deftest testing is are]]
            [scicloj.reel.core :as reel]
            [fastmath.complex :as c]))

;; ---------------------------------------------------------------------------
;; Group axioms for cyclic groups
;; ---------------------------------------------------------------------------

(deftest cyclic-group-axioms
  (let [G (reel/cyclic-group 8)]
    (testing "identity element"
      (doseq [g (reel/elements G)]
        (is (= g (reel/op G g (reel/id G))))
        (is (= g (reel/op G (reel/id G) g)))))

    (testing "inverse"
      (doseq [g (reel/elements G)]
        (is (= (reel/id G) (reel/op G g (reel/inv G g))))
        (is (= (reel/id G) (reel/op G (reel/inv G g) g)))))

    (testing "associativity (spot-check)"
      (doseq [a [0 1 3 7]
              b [0 2 5 6]
              c [0 4 7 1]]
        (is (= (reel/op G (reel/op G a b) c)
               (reel/op G a (reel/op G b c))))))))

(deftest cyclic-group-order
  (doseq [n [1 2 3 5 8 12 100]]
    (let [G (reel/cyclic-group n)]
      (is (= n (reel/order G)))
      (is (= n (count (reel/elements G)))))))

;; ---------------------------------------------------------------------------
;; Character table properties
;; ---------------------------------------------------------------------------

(deftest character-orthogonality
  (doseq [n [3 5 8 12]]
    (let [G (reel/cyclic-group n)
          ct (reel/character-table G)
          table (:table ct)
          sizes (:class-sizes ct)]
      (testing (str "row orthogonality for Z/" n "Z")
        (doseq [j (range n)
                k (range n)]
          (let [ip (c/abs (reel/character-inner-product
                           (table j) (table k) sizes n))]
            (if (= j k)
              (is (< (Math/abs (- ip 1.0)) 1e-8)
                  (str "chi_" j " should have norm 1"))
              (is (< ip 1e-8)
                  (str "chi_" j " and chi_" k " should be orthogonal")))))))))

(deftest dimension-sum-formula
  ;; Sum of squared dimensions of irreps = |G|.
  ;; For abelian groups, all irreps are 1-dimensional, so this is just n = n.
  (doseq [n [3 5 8]]
    (let [G (reel/cyclic-group n)
          ct (reel/character-table G)]
      (is (= n (count (:irrep-labels ct)))))))

;; ---------------------------------------------------------------------------
;; Fourier transform properties
;; ---------------------------------------------------------------------------

(deftest fourier-round-trip
  (doseq [n [4 8 16]]
    (let [G (reel/cyclic-group n)
          ct (reel/character-table G)
          signal (mapv (fn [_] (c/complex (double (rand-int 100)))) (range n))
          f-hat (reel/fourier-transform ct signal)
          recovered (reel/inverse-fourier-transform ct f-hat)]
      (testing (str "round-trip for Z/" n "Z")
        (doseq [i (range n)]
          (is (< (c/abs (c/sub (signal i) (recovered i))) 1e-8)))))))

(deftest convolution-theorem
  (let [G (reel/cyclic-group 8)
        ct (reel/character-table G)
        f (mapv #(c/complex (double %)) [1 2 0 0 0 0 0 3])
        h (mapv #(c/complex (double %)) [0 1 1 0 0 0 0 0])
        ;; Convolution via library
        conv (reel/convolve ct f h)
        ;; Convolution via pointwise product in Fourier domain
        f-hat (reel/fourier-transform ct f)
        h-hat (reel/fourier-transform ct h)
        product (mapv c/mult f-hat h-hat)
        conv-from-fourier (reel/inverse-fourier-transform ct product)]
    (testing "convolution matches pointwise Fourier product"
      (doseq [i (range 8)]
        (is (< (c/abs (c/sub (conv i) (conv-from-fourier i))) 1e-8))))))

(deftest parseval-theorem
  (let [G (reel/cyclic-group 8)
        ct (reel/character-table G)
        signal (mapv #(c/complex (double %)) [20 22 25 23 21 19 18 20])
        f-hat (reel/fourier-transform ct signal)
        energy-time (reduce + (map #(let [m (c/abs %)] (* m m)) signal))
        energy-freq (/ (reduce + (map #(let [m (c/abs %)] (* m m)) f-hat))
                       (double (reel/order G)))]
    (is (< (Math/abs (- energy-time energy-freq)) 1e-8))))
