(ns scicloj.harmonica.complex-test
  (:require [clojure.test :refer [deftest is testing]]
            [scicloj.lalinea.tensor :as t]
            [scicloj.lalinea.elementwise :as el]
            [scicloj.lalinea.linalg :as la]
            [tech.v3.tensor :as tensor]
            [tech.v3.datatype :as dtype]
            [tech.v3.datatype.functional :as dfn]))

;; ---------------------------------------------------------------------------
;; Construction
;; ---------------------------------------------------------------------------

(deftest construction-from-re-im
  (let [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
    (is (= [3] (t/complex-shape ct)))
    (is (= 3 (count ct)))
    (is (not (t/scalar? ct)))
    (is (= [1.0 2.0 3.0] (vec (el/re ct))))
    (is (= [4.0 5.0 6.0] (vec (el/im ct))))))

(deftest construction-from-tensor
  (let [t (tensor/->tensor [[1.0 2.0] [3.0 4.0]])
        ct (t/complex-tensor t)]
    (is (= [2] (t/complex-shape ct)))
    (is (= 2 (count ct)))
    (is (= [1.0 3.0] (vec (el/re ct))))
    (is (= [2.0 4.0] (vec (el/im ct))))))

(deftest construction-real-only
  (let [ct (t/complex-tensor-real [5.0 6.0 7.0])]
    (is (= [5.0 6.0 7.0] (vec (el/re ct))))
    (is (= [0.0 0.0 0.0] (vec (el/im ct))))))

(deftest construction-matrix
  (let [ct (t/complex-tensor [[1.0 2.0] [3.0 4.0]]
                             [[5.0 6.0] [7.0 8.0]])]
    (is (= [2 2] (t/complex-shape ct)))
    (is (= 2 (count ct)))))

;; ---------------------------------------------------------------------------
;; Element access
;; ---------------------------------------------------------------------------

(deftest element-access-vector
  (let [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
    ;; nth
    (is (t/scalar? (nth ct 0)))
    (is (= 1.0 (el/re (nth ct 0))))
    (is (= 4.0 (el/im (nth ct 0))))
    ;; IFn
    (is (= 2.0 (el/re (ct 1))))
    (is (= 5.0 (el/im (ct 1))))
    ;; nth with not-found
    (is (= :nope (nth ct 99 :nope)))))

(deftest element-access-matrix
  (let [ct (t/complex-tensor [[1.0 2.0] [3.0 4.0]]
                             [[5.0 6.0] [7.0 8.0]])]
    ;; Row access returns a complex vector
    (let [row0 (ct 0)]
      (is (= [2] (t/complex-shape row0)))
      (is (= [1.0 2.0] (vec (el/re row0))))
      (is (= [5.0 6.0] (vec (el/im row0)))))
    ;; Nested access
    (is (= 4.0 (el/re ((ct 1) 1))))
    (is (= 8.0 (el/im ((ct 1) 1))))))

(deftest seq-support
  (let [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])]
    (is (= 2 (count (seq ct))))
    (is (every? t/scalar? (seq ct)))
    (is (= [1.0 2.0] (mapv el/re (seq ct))))
    (is (= [3.0 4.0] (mapv el/im (seq ct))))))

(deftest scalar-complex
  (let [s (t/complex-tensor (tensor/->tensor [3.0 4.0]))]
    (is (t/scalar? s))
    (is (= 0 (count s)))
    (is (nil? (seq s)))
    (is (= 3.0 (el/re s)))
    (is (= 4.0 (el/im s)))))

;; ---------------------------------------------------------------------------
;; Arithmetic
;; ---------------------------------------------------------------------------

(deftest cmul-test
  (let [a (t/complex-tensor [1.0 2.0] [3.0 4.0])
        b (t/complex-tensor [5.0 6.0] [7.0 8.0])
        c (el/* a b)]
    ;; (1+3i)(5+7i) = (5-21) + (7+15)i = -16+22i
    ;; (2+4i)(6+8i) = (12-32) + (16+24)i = -20+40i
    (is (= [-16.0 -20.0] (vec (el/re c))))
    (is (= [22.0 40.0] (vec (el/im c))))))

(deftest cconj-test
  (let [a (t/complex-tensor [1.0 2.0] [3.0 -4.0])
        c (el/conj a)]
    (is (= [1.0 2.0] (vec (el/re c))))
    (is (= [-3.0 4.0] (vec (el/im c))))))

(deftest cscale-test
  (let [a (t/complex-tensor [1.0 2.0] [3.0 4.0])
        c (el/scale a 2.0)]
    (is (= [2.0 4.0] (vec (el/re c))))
    (is (= [6.0 8.0] (vec (el/im c))))))

(deftest cabs-test
  (let [a (t/complex-tensor [3.0 0.0] [4.0 1.0])
        m (el/abs a)]
    (is (< (Math/abs (- 5.0 (double (m 0)))) 1e-10))
    (is (< (Math/abs (- 1.0 (double (m 1)))) 1e-10))))

(deftest cdot-test
  ;; la/dot is Hermitian: la/dot(a,a) = ||a||^2
  (let [a (t/complex-tensor [3.0 1.0] [4.0 2.0])
        result (la/dot a a)
        re (double (el/re result))
        im (double (el/im result))]
    ;; |3+i|^2 + |4+2i|^2 = 10 + 20 = 30
    (is (< (Math/abs (- re 30.0)) 1e-10))
    (is (< (Math/abs im) 1e-10)))
  ;; Bilinear form: el/sum(el/* a b) — no conjugation
  (let [a (t/complex-tensor [1.0 0.0] [0.0 1.0]) ;; [1, i]
        b (t/complex-tensor [0.0 1.0] [1.0 0.0]) ;; [i, 1]
        result (el/sum (el/* a b))
        re (double (el/re result))
        im (double (el/im result))]
    ;; 1*i + i*1 = 2i
    (is (< (Math/abs re) 1e-10))
    (is (< (Math/abs (- im 2.0)) 1e-10))))

(deftest cdot-conj-test
  (let [a (t/complex-tensor [1.0 0.0] [0.0 1.0]) ;; [1, i]
        b (t/complex-tensor [0.0 1.0] [1.0 0.0]) ;; [i, 1]
        ;; 1*conj(i) + i*conj(1) = -i + i = 0
        result (la/dot-conj a b)
        re (el/re result)
        im (el/im result)]
    (is (< (Math/abs (double re)) 1e-10))
    (is (< (Math/abs (double im)) 1e-10))))

(deftest cdot-conj-norm
  ;; <a, a> = ||a||^2 for Hermitian inner product
  (let [a (t/complex-tensor [3.0 1.0] [4.0 2.0])
        result (la/dot-conj a a)
        re (el/re result)
        im (el/im result)]
    ;; |3+i|^2 + |4+2i|^2 = 10 + 20 = 30
    (is (< (Math/abs (- (double re) 30.0)) 1e-10))
    (is (< (Math/abs (double im)) 1e-10))))

(deftest complex-scalar-test
  (let [s (t/complex 3.0 4.0)]
    (is (t/scalar? s))
    (is (= 3.0 (el/re s)))
    (is (= 4.0 (el/im s)))))

(deftest cadd-test
  (testing "vector addition"
    (let [a (t/complex-tensor [1.0 2.0] [3.0 4.0])
          b (t/complex-tensor [5.0 6.0] [7.0 8.0])
          c (el/+ a b)]
      (is (= [6.0 8.0] (vec (el/re c))))
      (is (= [10.0 12.0] (vec (el/im c))))))
  (testing "scalar addition"
    (let [a (t/complex 1.0 2.0)
          b (t/complex 3.0 4.0)
          c (el/+ a b)]
      (is (= 4.0 (el/re c)))
      (is (= 6.0 (el/im c))))))

(deftest csub-test
  (testing "vector subtraction"
    (let [a (t/complex-tensor [5.0 8.0] [3.0 4.0])
          b (t/complex-tensor [1.0 2.0] [1.0 1.0])
          c (el/- a b)]
      (is (= [4.0 6.0] (vec (el/re c))))
      (is (= [2.0 3.0] (vec (el/im c))))))
  (testing "scalar subtraction"
    (let [a (t/complex 5.0 3.0)
          b (t/complex 2.0 1.0)
          c (el/- a b)]
      (is (= 3.0 (el/re c)))
      (is (= 2.0 (el/im c))))))

(deftest cmul-scalar-test
  (let [a (t/complex 1.0 3.0)
        b (t/complex 5.0 7.0)
        c (el/* a b)]
    ;; (1+3i)(5+7i) = (5-21) + (7+15)i = -16+22i
    (is (= -16.0 (el/re c)))
    (is (= 22.0 (el/im c)))))

;; ---------------------------------------------------------------------------
;; Zero-copy
;; ---------------------------------------------------------------------------

(deftest double-array-roundtrip
  (let [ct (t/complex-tensor [1.0 2.0] [3.0 4.0])
        arr (t/->double-array ct)]
    (is (= [1.0 3.0 2.0 4.0] (vec arr)))))

;; ---------------------------------------------------------------------------
;; Print
;; ---------------------------------------------------------------------------

(deftest print-format
  (is (= "ComplexTensor<float64>[2]"
         (str (t/complex-tensor [1.0 3.0] [2.0 4.0]))))
  (is (= "ComplexTensor<float64>[2]"
         (str (t/complex-tensor-real [1.0 2.0]))))
  (is (= "ComplexTensor<float64>[1]"
         (str (t/complex-tensor [3.0] [-4.0]))))
  (is (= "ComplexTensor<float64>[]"
         (str (t/complex-tensor (tensor/->tensor [3.0 4.0])))))
  (is (= "ComplexTensor<float64>[2 2]"
         (str (t/complex-tensor [[1.0 2.0] [3.0 4.0]]
                                [[5.0 6.0] [7.0 8.0]])))))

;; ---------------------------------------------------------------------------
;; dtype-next protocol integration
;; ---------------------------------------------------------------------------

(deftest dfn-addition
  (let [a (t/complex-tensor [1.0 2.0] [3.0 4.0])
        b (t/complex-tensor [5.0 6.0] [7.0 8.0])
        result (t/complex-tensor (dfn/+ a b))]
    (is (= [6.0 8.0] (vec (el/re result))))
    (is (= [10.0 12.0] (vec (el/im result))))))

(deftest dfn-subtraction
  (let [a (t/complex-tensor [5.0 8.0] [3.0 4.0])
        b (t/complex-tensor [1.0 2.0] [1.0 1.0])
        result (t/complex-tensor (dfn/- a b))]
    (is (= [4.0 6.0] (vec (el/re result))))
    (is (= [2.0 3.0] (vec (el/im result))))))

(deftest dfn-real-scaling
  (let [a (t/complex-tensor [1.0 2.0] [3.0 4.0])
        result (t/complex-tensor (dfn/* a 2.0))]
    (is (= [2.0 4.0] (vec (el/re result))))
    (is (= [6.0 8.0] (vec (el/im result))))))

(deftest csum-test
  (let [a (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])
        s (el/sum a)]
    (is (t/scalar? s))
    (is (= 6.0 (el/re s)))
    (is (= 15.0 (el/im s)))))

(deftest dtype-ecount-test
  (let [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
    (is (= 6 (dtype/ecount ct)))))

(deftest dtype-shape-test
  (let [ct (t/complex-tensor [1.0 2.0 3.0] [4.0 5.0 6.0])]
    (is (= [3 2] (vec (dtype/shape ct))))))
