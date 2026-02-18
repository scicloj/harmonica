(ns scicloj.harmonica.action-test
  (:require [clojure.test :refer [deftest testing is are]]
            [scicloj.harmonica :as hm]
            [scicloj.harmonica.protocols :as p]
            [scicloj.harmonica.group.dihedral :as dih]
            [scicloj.harmonica.group.product :as prod]
            [scicloj.harmonica.group.cyclic :as cyc]
            [scicloj.harmonica.analysis.characters :as ch]
            [scicloj.harmonica.action :as action]
            [scicloj.harmonica.linalg.complex :as cx]))

;; ---------------------------------------------------------------------------
;; DihedralGroup axioms
;; ---------------------------------------------------------------------------

(deftest dihedral-group-axioms
  (doseq [n [3 4 5 6 8]]
    (let [G (dih/dihedral-group n)
          elts (vec (p/elements G))
          e (p/id G)]
      (testing (str "D_" n " identity")
        (doseq [g elts]
          (is (= g (p/op G g e)))
          (is (= g (p/op G e g)))))
      (testing (str "D_" n " inverse")
        (doseq [g elts]
          (is (= e (p/op G g (p/inv G g))))
          (is (= e (p/op G (p/inv G g) g)))))
      (testing (str "D_" n " associativity (spot-check)")
        ;; Test with a sample of triples
        (let [sample (take 30 (for [a elts b elts c elts] [a b c]))]
          (doseq [[a b c] sample]
            (is (= (p/op G (p/op G a b) c)
                   (p/op G a (p/op G b c))))))))))

(deftest dihedral-group-order
  (doseq [n [1 2 3 4 5 6 10]]
    (let [G (dih/dihedral-group n)]
      (is (= (* 2 n) (p/order G))
          (str "D_" n " should have order " (* 2 n)))
      (is (= (* 2 n) (count (p/elements G)))))))

(deftest dihedral-conjugacy-class-count
  (testing "n odd: (n+3)/2 classes"
    (doseq [n [3 5 7]]
      (is (= (/ (+ n 3) 2)
             (count (p/conjugacy-classes (dih/dihedral-group n))))
          (str "D_" n))))
  (testing "n even: n/2+3 classes"
    (doseq [n [4 6 8]]
      (is (= (+ (/ n 2) 3)
             (count (p/conjugacy-classes (dih/dihedral-group n))))
          (str "D_" n)))))

(deftest dihedral-class-sizes-sum-to-order
  (doseq [n [3 4 5 6 8]]
    (let [G (dih/dihedral-group n)
          sizes (map :size (p/conjugacy-classes G))]
      (is (= (* 2 n) (reduce + sizes))
          (str "class sizes must sum to 2n for D_" n)))))

;; ---------------------------------------------------------------------------
;; ProductGroup axioms
;; ---------------------------------------------------------------------------

(deftest product-group-axioms
  (let [G (prod/product-group (cyc/cyclic-group 2) (cyc/cyclic-group 3))
        elts (vec (p/elements G))
        e (p/id G)]
    (testing "identity"
      (doseq [g elts]
        (is (= g (p/op G g e)))
        (is (= g (p/op G e g)))))
    (testing "inverse"
      (doseq [g elts]
        (is (= e (p/op G g (p/inv G g))))
        (is (= e (p/op G (p/inv G g) g)))))
    (testing "associativity"
      (doseq [a elts b elts c elts]
        (is (= (p/op G (p/op G a b) c)
               (p/op G a (p/op G b c))))))))

(deftest product-group-order
  (is (= 6 (p/order (prod/product-group (cyc/cyclic-group 2) (cyc/cyclic-group 3)))))
  (is (= 12 (p/order (prod/product-group (cyc/cyclic-group 3) (cyc/cyclic-group 4))))))

(deftest product-group-class-count
  ;; Z/2Z × Z/3Z is abelian with 6 elements, so 6 conjugacy classes
  (is (= 6 (count (p/conjugacy-classes
                   (prod/product-group (cyc/cyclic-group 2) (cyc/cyclic-group 3)))))))

;; ---------------------------------------------------------------------------
;; Dihedral character table
;; ---------------------------------------------------------------------------

(deftest dihedral-character-orthogonality
  (doseq [n [3 4 5 6 8]]
    (let [G (dih/dihedral-group n)
          ct (ch/character-table G)
          table (:table ct)
          sizes (:class-sizes ct)
          order (p/order G)
          num-irreps (count table)]
      (testing (str "row orthogonality for D_" n)
        (doseq [i (range num-irreps)
                j (range num-irreps)]
          (let [ip (cx/cabs (ch/character-inner-product (table i) (table j) sizes order))]
            (if (= i j)
              (is (< (Math/abs (- ip 1.0)) 1e-8)
                  (str "chi_" i " should have norm 1 in D_" n))
              (is (< ip 1e-8)
                  (str "chi_" i " and chi_" j " should be orthogonal in D_" n)))))))))

(deftest dihedral-dimension-sum
  (testing "Σ d² = |G| for dihedral groups"
    (doseq [n [3 4 5 6 8]]
      (let [G (dih/dihedral-group n)
            ct (ch/character-table G)
            table (:table ct)
            order (p/order G)
            sum-sq (reduce + (map (fn [row]
                                    (let [d (long (Math/round (cx/re (row 0))))]
                                      (* d d)))
                                  table))]
        (is (= order sum-sq)
            (str "Σ d² ≠ 2n for D_" n))))))

;; ---------------------------------------------------------------------------
;; Group actions: orbits and Burnside
;; ---------------------------------------------------------------------------

(defn- rotation-act
  "Action of cyclic group element g on position x mod n."
  [g x]
  (mod (+ (long x) (long g)) (count (range 12)))) ;; placeholder, use closure

(defn- make-cyclic-act [n]
  (fn [g x] (mod (+ (long x) (long g)) n)))

(defn- make-dihedral-act [n]
  (fn [[t k] x]
    (case t
      :r (mod (+ (long x) (long k)) n)
      :s (mod (- (long k) (long x)) n))))

(defn- all-colorings
  "All k-colorings of n positions as vectors."
  [n k]
  (if (zero? n)
    [[]]
    (for [rest-coloring (all-colorings (dec n) k)
          c (range k)]
      (conj rest-coloring c))))

(deftest necklace-counts
  (testing "binary necklaces (OEIS A000031)"
    ;; A000031: 1, 2, 3, 4, 6, 8, 14, 20, 36, ...
    (let [expected {3 4, 4 6, 5 8, 6 14}]
      (doseq [[n cnt] expected]
        (let [G (cyc/cyclic-group n)
              domain (all-colorings n 2)
              act (fn [g coloring]
                    (let [len (count coloring)]
                      (mapv #(coloring (mod (+ % (long g)) len)) (range len))))]
          (is (= cnt (action/burnside-count G act domain))
              (str "binary necklaces for n=" n)))))))

(deftest bracelet-counts
  (testing "binary bracelets (OEIS A000029)"
    ;; A000029: 1, 2, 3, 4, 6, 8, 13, 18, 30, ...
    (let [expected {3 4, 4 6, 5 8, 6 13}]
      (doseq [[n cnt] expected]
        (let [G (dih/dihedral-group n)
              domain (all-colorings n 2)
              act (fn [[t k] coloring]
                    (let [len (count coloring)]
                      (case t
                        :r (mapv #(coloring (mod (+ % (long k)) len)) (range len))
                        :s (mapv #(coloring (mod (- (long k) %) len)) (range len)))))]
          (is (= cnt (action/burnside-count G act domain))
              (str "binary bracelets for n=" n)))))))

(deftest orbits-match-burnside
  (testing "orbits count equals Burnside count"
    (doseq [n [3 4 5]]
      (let [G (cyc/cyclic-group n)
            domain (all-colorings n 2)
            act (fn [g coloring]
                  (let [len (count coloring)]
                    (mapv #(coloring (mod (+ % (long g)) len)) (range len))))]
        (is (= (count (action/orbits G act domain))
               (action/burnside-count G act domain))
            (str "n=" n))))))

(deftest polya-matches-burnside
  (testing "Pólya count matches Burnside for small cases"
    (doseq [n [3 4 5 6]
            k [2 3]]
      (let [G (cyc/cyclic-group n)
            domain (range n)
            act (make-cyclic-act n)
            ci (action/cycle-index G act domain)
            polya (action/polya-count ci k)
            ;; Also compute via Burnside on colorings
            all-cols (all-colorings n k)
            act-col (fn [g coloring]
                      (let [len (count coloring)]
                        (mapv #(coloring (mod (+ % (long g)) len)) (range len))))
            burnside (action/burnside-count G act-col all-cols)]
        (is (= burnside polya)
            (str "n=" n " k=" k))))))

;; ---------------------------------------------------------------------------
;; Chord classification
;; ---------------------------------------------------------------------------

(deftest trichord-classification
  (testing "220 trichords → 19 types under C₁₂ → 12 under D₁₂"
    (let [domain (range 12)
          act-c (fn [g x] (mod (+ (long x) (long g)) 12))
          act-d (fn [[t k] x]
                  (case t
                    :r (mod (+ (long x) (long k)) 12)
                    :s (mod (- (long k) (long x)) 12)))
          ;; Trichords under C_12
          sub-c (action/subset-action act-c domain 3)
          types-c (action/orbits (cyc/cyclic-group 12) (:act sub-c) (:domain sub-c))
          ;; Trichords under D_12
          sub-d (action/subset-action act-d domain 3)
          types-d (action/orbits (dih/dihedral-group 12) (:act sub-d) (:domain sub-d))]
      (is (= 220 (count (:domain sub-c))))
      (is (= 19 (count types-c)))
      (is (= 12 (count types-d))))))

(deftest stabilizer-orbit-theorem
  (testing "|orbit| × |stabilizer| = |G| for several points"
    (let [n 5
          G (cyc/cyclic-group n)
          domain (range n)
          act (make-cyclic-act n)]
      (doseq [x domain]
        (is (= (long (p/order G))
               (* (count (action/orbit G act x))
                  (count (action/stabilizer G act x)))))))))

(deftest coloring-action-test
  (testing "coloring-action generates correct domain and action"
    (let [point-act (fn [g x] (mod (+ (long x) (long g)) 4))
          {:keys [domain act]} (action/coloring-action point-act 4 2)]
      (is (= 16 (count domain)) "2^4 = 16 colorings")
      (is (= [0 1 1 0] (act 1 [0 0 1 1])) "rotation by 1")))
  (testing "coloring-action gives correct Burnside counts"
    (doseq [[n k expected] [[4 2 6] [5 2 8] [6 2 14] [3 3 11]]]
      (let [G (cyc/cyclic-group n)
            point-act (fn [g x] (mod (+ (long x) (long g)) n))
            {:keys [domain act]} (action/coloring-action point-act n k)]
        (is (= expected (action/burnside-count G act domain))
            (str "n=" n " k=" k)))))
  (testing "coloring-action with dihedral group gives bracelet counts"
    (doseq [[n expected] [[3 4] [4 6] [5 8] [6 13]]]
      (let [G (dih/dihedral-group n)
            point-act (fn [[t k] x]
                        (case t
                          :r (mod (+ (long x) (long k)) n)
                          :s (mod (- (long k) (long x)) n)))
            {:keys [domain act]} (action/coloring-action point-act n 2)]
        (is (= expected (action/burnside-count G act domain))
            (str "D_" n " bracelets"))))))
