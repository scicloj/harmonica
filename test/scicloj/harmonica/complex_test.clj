(ns scicloj.harmonica.complex-test
  (:require [clojure.test :refer [deftest is testing]]
            [scicloj.harmonica.linalg.complex :as cx]
            [tech.v3.tensor :as tensor]
            [tech.v3.datatype :as dtype]
            [tech.v3.datatype.functional :as dfn]))

;; ---------------------------------------------------------------------------
;; Construction
;; ---------------------------------------------------------------------------

(deftest construction-from-re-im
  (let [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
    (is (= [3] (cx/complex-shape ct)))
    (is (= 3 (count ct)))
    (is (not (cx/scalar? ct)))
    (is (= [1.0 2.0 3.0] (vec (cx/re ct))))
    (is (= [4.0 5.0 6.0] (vec (cx/im ct))))))

(deftest construction-from-tensor
  (let [t (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
        ct (cx/complex-tensor t)]
    (is (= [2] (cx/complex-shape ct)))
    (is (= 2 (count ct)))
    (is (= [1.0 3.0] (vec (cx/re ct))))
    (is (= [2.0 4.0] (vec (cx/im ct))))))

(deftest construction-real-only
  (let [ct (cx/complex-tensor-real [5.0 6.0 7.0])]
    (is (= [5.0 6.0 7.0] (vec (cx/re ct))))
    (is (= [0.0 0.0 0.0] (vec (cx/im ct))))))

(deftest construction-matrix
  (let [ct (cx/complex-tensor [[1.0 2.0] [3.0 4.0]]
                              [[5.0 6.0] [7.0 8.0]])]
    (is (= [2 2] (cx/complex-shape ct)))
    (is (= 2 (count ct)))))

;; ---------------------------------------------------------------------------
;; Element access
;; ---------------------------------------------------------------------------

(deftest element-access-vector
  (let [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
    ;; nth
    (is (cx/scalar? (nth ct 0)))
    (is (= 1.0 (cx/re (nth ct 0))))
    (is (= 4.0 (cx/im (nth ct 0))))
    ;; IFn
    (is (= 2.0 (cx/re (ct 1))))
    (is (= 5.0 (cx/im (ct 1))))
    ;; nth with not-found
    (is (= :nope (nth ct 99 :nope)))))

(deftest element-access-matrix
  (let [ct (cx/complex-tensor [[1.0 2.0] [3.0 4.0]]
                              [[5.0 6.0] [7.0 8.0]])]
    ;; Row access returns a complex vector
    (let [row0 (ct 0)]
      (is (= [2] (cx/complex-shape row0)))
      (is (= [1.0 2.0] (vec (cx/re row0))))
      (is (= [5.0 6.0] (vec (cx/im row0)))))
    ;; Nested access
    (is (= 4.0 (cx/re ((ct 1) 1))))
    (is (= 8.0 (cx/im ((ct 1) 1))))))

(deftest seq-support
  (let [ct (cx/complex-tensor [1.0 2.0] [3.0 4.0])]
    (is (= 2 (count (seq ct))))
    (is (every? cx/scalar? (seq ct)))
    (is (= [1.0 2.0] (mapv cx/re (seq ct))))
    (is (= [3.0 4.0] (mapv cx/im (seq ct))))))

(deftest scalar-complex
  (let [s (cx/complex-tensor (tensor/->tensor [3.0 4.0]))]
    (is (cx/scalar? s))
    (is (= 0 (count s)))
    (is (nil? (seq s)))
    (is (= 3.0 (cx/re s)))
    (is (= 4.0 (cx/im s)))))

;; ---------------------------------------------------------------------------
;; Arithmetic
;; ---------------------------------------------------------------------------

(deftest cmul-test
  (let [a (cx/complex-tensor [1.0 2.0] [3.0 4.0])
        b (cx/complex-tensor [5.0 6.0] [7.0 8.0])
        c (cx/cmul a b)]
    ;; (1+3i)(5+7i) = (5-21) + (7+15)i = -16+22i
    ;; (2+4i)(6+8i) = (12-32) + (16+24)i = -20+40i
    (is (= [-16.0 -20.0] (vec (cx/re c))))
    (is (= [22.0 40.0] (vec (cx/im c))))))

(deftest cconj-test
  (let [a (cx/complex-tensor [1.0 2.0] [3.0 -4.0])
        c (cx/cconj a)]
    (is (= [1.0 2.0] (vec (cx/re c))))
    (is (= [-3.0 4.0] (vec (cx/im c))))))

(deftest cscale-test
  (let [a (cx/complex-tensor [1.0 2.0] [3.0 4.0])
        c (cx/cscale a 2.0)]
    (is (= [2.0 4.0] (vec (cx/re c))))
    (is (= [6.0 8.0] (vec (cx/im c))))))

(deftest cabs-test
  (let [a (cx/complex-tensor [3.0 0.0] [4.0 1.0])
        m (cx/cabs a)]
    (is (< (Math/abs (- 5.0 (double (m 0)))) 1e-10))
    (is (< (Math/abs (- 1.0 (double (m 1)))) 1e-10))))

(deftest cdot-test
  (let [a (cx/complex-tensor [1.0 0.0] [0.0 1.0]) ;; [1, i]
        b (cx/complex-tensor [0.0 1.0] [1.0 0.0])] ;; [i, 1]
    ;; 1*i + i*1 = 2i
    (let [[re im] (cx/cdot a b)]
      (is (< (Math/abs re) 1e-10))
      (is (< (Math/abs (- im 2.0)) 1e-10)))))

(deftest cdot-conj-test
  (let [a (cx/complex-tensor [1.0 0.0] [0.0 1.0]) ;; [1, i]
        b (cx/complex-tensor [0.0 1.0] [1.0 0.0])] ;; [i, 1]
    ;; 1*conj(i) + i*conj(1) = -i + i = 0
    (let [[re im] (cx/cdot-conj a b)]
      (is (< (Math/abs re) 1e-10))
      (is (< (Math/abs im) 1e-10)))))

(deftest cdot-conj-norm
  ;; <a, a> = ||a||^2 for Hermitian inner product
  (let [a (cx/complex-tensor [3.0 1.0] [4.0 2.0])
        [re im] (cx/cdot-conj a a)]
    ;; |3+4i|^2 + |1+2i|^2 = 25 + 5 = 30
    (is (< (Math/abs (- re 30.0)) 1e-10))
    (is (< (Math/abs im) 1e-10))))

(deftest complex-scalar-test
  (let [s (cx/complex 3.0 4.0)]
    (is (cx/scalar? s))
    (is (= 3.0 (cx/re s)))
    (is (= 4.0 (cx/im s)))))

(deftest cadd-test
  (testing "vector addition"
    (let [a (cx/complex-tensor [1.0 2.0] [3.0 4.0])
          b (cx/complex-tensor [5.0 6.0] [7.0 8.0])
          c (cx/cadd a b)]
      (is (= [6.0 8.0] (vec (cx/re c))))
      (is (= [10.0 12.0] (vec (cx/im c))))))
  (testing "scalar addition"
    (let [a (cx/complex 1.0 2.0)
          b (cx/complex 3.0 4.0)
          c (cx/cadd a b)]
      (is (= 4.0 (cx/re c)))
      (is (= 6.0 (cx/im c))))))

(deftest csub-test
  (testing "vector subtraction"
    (let [a (cx/complex-tensor [5.0 8.0] [3.0 4.0])
          b (cx/complex-tensor [1.0 2.0] [1.0 1.0])
          c (cx/csub a b)]
      (is (= [4.0 6.0] (vec (cx/re c))))
      (is (= [2.0 3.0] (vec (cx/im c))))))
  (testing "scalar subtraction"
    (let [a (cx/complex 5.0 3.0)
          b (cx/complex 2.0 1.0)
          c (cx/csub a b)]
      (is (= 3.0 (cx/re c)))
      (is (= 2.0 (cx/im c))))))

(deftest cmul-scalar-test
  (let [a (cx/complex 1.0 3.0)
        b (cx/complex 5.0 7.0)
        c (cx/cmul a b)]
    ;; (1+3i)(5+7i) = (5-21) + (7+15)i = -16+22i
    (is (= -16.0 (cx/re c)))
    (is (= 22.0 (cx/im c)))))

;; ---------------------------------------------------------------------------
;; Zero-copy
;; ---------------------------------------------------------------------------

(deftest zero-copy-double-array
  (let [ct (cx/complex-tensor [1.0 2.0] [3.0 4.0])
        arr1 (cx/->double-array ct)
        arr2 (cx/->double-array ct)]
    (is (identical? arr1 arr2))
    (is (= [1.0 3.0 2.0 4.0] (vec arr1)))))

;; ---------------------------------------------------------------------------
;; Print
;; ---------------------------------------------------------------------------

(deftest print-format
  (is (= "#ComplexTensor [2]\n[1.0+2.0i, 3.0+4.0i]"
         (str (cx/complex-tensor [1.0 3.0] [2.0 4.0]))))
  (is (= "#ComplexTensor [2]\n[1.0, 2.0]"
         (str (cx/complex-tensor-real [1.0 2.0]))))
  (is (= "#ComplexTensor [1]\n[3.0-4.0i]"
         (str (cx/complex-tensor [3.0] [-4.0]))))
  (is (= "#ComplexTensor []\n3.0+4.0i"
         (str (cx/complex-tensor (tensor/->tensor [3.0 4.0])))))
  (is (= "#ComplexTensor [2 2]\n[[1.0+5.0i, 2.0+6.0i]\n [3.0+7.0i, 4.0+8.0i]]"
         (str (cx/complex-tensor [[1.0 2.0] [3.0 4.0]]
                                 [[5.0 6.0] [7.0 8.0]])))))

;; ---------------------------------------------------------------------------
;; dtype-next protocol integration
;; ---------------------------------------------------------------------------

(deftest dfn-addition
  (let [a (cx/complex-tensor [1.0 2.0] [3.0 4.0])
        b (cx/complex-tensor [5.0 6.0] [7.0 8.0])
        result (cx/complex-tensor (dfn/+ a b))]
    (is (= [6.0 8.0] (vec (cx/re result))))
    (is (= [10.0 12.0] (vec (cx/im result))))))

(deftest dfn-subtraction
  (let [a (cx/complex-tensor [5.0 8.0] [3.0 4.0])
        b (cx/complex-tensor [1.0 2.0] [1.0 1.0])
        result (cx/complex-tensor (dfn/- a b))]
    (is (= [4.0 6.0] (vec (cx/re result))))
    (is (= [2.0 3.0] (vec (cx/im result))))))

(deftest dfn-real-scaling
  (let [a (cx/complex-tensor [1.0 2.0] [3.0 4.0])
        result (cx/complex-tensor (dfn/* a 2.0))]
    (is (= [2.0 4.0] (vec (cx/re result))))
    (is (= [6.0 8.0] (vec (cx/im result))))))

(deftest csum-test
  (let [a (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])
        s (cx/csum a)]
    (is (cx/scalar? s))
    (is (= 6.0 (cx/re s)))
    (is (= 15.0 (cx/im s)))))

(deftest dtype-ecount-test
  (let [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
    (is (= 6 (dtype/ecount ct)))))

(deftest dtype-shape-test
  (let [ct (cx/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
    (is (= [3 2] (vec (dtype/shape ct))))))
