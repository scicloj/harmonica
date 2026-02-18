(ns scicloj.harmonica.protocols-test
  (:require [clojure.test :refer [deftest testing is are]]
            [scicloj.harmonica :as hm]
            [scicloj.harmonica.linalg.complex :as cx]))

;; ---------------------------------------------------------------------------
;; Group axioms for cyclic groups
;; ---------------------------------------------------------------------------

(deftest cyclic-group-axioms
  (let [G (hm/cyclic-group 8)]
    (testing "identity element"
      (doseq [g (hm/elements G)]
        (is (= g (hm/op G g (hm/id G))))
        (is (= g (hm/op G (hm/id G) g)))))

    (testing "inverse"
      (doseq [g (hm/elements G)]
        (is (= (hm/id G) (hm/op G g (hm/inv G g))))
        (is (= (hm/id G) (hm/op G (hm/inv G g) g)))))

    (testing "associativity (spot-check)"
      (doseq [a [0 1 3 7]
              b [0 2 5 6]
              c [0 4 7 1]]
        (is (= (hm/op G (hm/op G a b) c)
               (hm/op G a (hm/op G b c))))))))

(deftest cyclic-group-order
  (doseq [n [1 2 3 5 8 12 100]]
    (let [G (hm/cyclic-group n)]
      (is (= n (hm/order G)))
      (is (= n (count (hm/elements G)))))))

;; ---------------------------------------------------------------------------
;; Character table properties
;; ---------------------------------------------------------------------------

(deftest character-orthogonality
  (doseq [n [3 5 8 12]]
    (let [G (hm/cyclic-group n)
          ct (hm/character-table G)
          table (:table ct)
          sizes (:class-sizes ct)]
      (testing (str "row orthogonality for Z/" n "Z")
        (doseq [j (range n)
                k (range n)]
          (let [ip (cx/cabs (hm/character-inner-product
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
    (let [G (hm/cyclic-group n)
          ct (hm/character-table G)]
      (is (= n (count (:irrep-labels ct)))))))

;; ---------------------------------------------------------------------------
;; Fourier transform properties
;; ---------------------------------------------------------------------------

(deftest fourier-round-trip
  (doseq [n [4 8 16]]
    (let [G (hm/cyclic-group n)
          ct (hm/character-table G)
          signal (cx/complex-tensor-real (mapv (fn [_] (double (rand-int 100))) (range n)))
          f-hat (hm/fourier-transform ct signal)
          recovered (hm/inverse-fourier-transform ct f-hat)
          max-err (apply max (vec (cx/cabs (cx/csub recovered signal))))]
      (testing (str "round-trip for Z/" n "Z")
        (is (< max-err 1e-8))))))

(deftest convolution-theorem
  (let [G (hm/cyclic-group 8)
        ct (hm/character-table G)
        f (cx/complex-tensor-real [1 2 0 0 0 0 0 3])
        h (cx/complex-tensor-real [0 1 1 0 0 0 0 0])
        ;; Convolution via library
        conv (hm/convolve ct f h)
        ;; Convolution via pointwise product in Fourier domain
        f-hat (hm/fourier-transform ct f)
        h-hat (hm/fourier-transform ct h)
        product (cx/cmul f-hat h-hat)
        conv-from-fourier (hm/inverse-fourier-transform ct product)
        max-err (apply max (vec (cx/cabs (cx/csub conv conv-from-fourier))))]
    (testing "convolution matches pointwise Fourier product"
      (is (< max-err 1e-8)))))

(deftest parseval-theorem
  (let [G (hm/cyclic-group 8)
        ct (hm/character-table G)
        signal (cx/complex-tensor-real [20 22 25 23 21 19 18 20])
        f-hat (hm/fourier-transform ct signal)
        mag-s (cx/cabs signal)
        mag-f (cx/cabs f-hat)
        energy-time (apply + (map #(* % %) (vec mag-s)))
        energy-freq (/ (apply + (map #(* % %) (vec mag-f)))
                       (double (hm/order G)))]
    (is (< (Math/abs (- energy-time energy-freq)) 1e-8))))
