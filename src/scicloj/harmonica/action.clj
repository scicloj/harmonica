(ns scicloj.harmonica.action
  "Group actions on finite sets.

   A group action is represented as a plain function (act g x) → x'
   where g is a group element and x is a point in the domain.

   Core functions:
     orbit          - orbit of a point under the group action
     orbits         - partition a domain into orbits
     fixed-points   - points fixed by a given group element
     stabilizer     - group elements that fix a given point
     burnside-count - number of orbits via Burnside's lemma
     cycle-index    - cycle index of a permutation action
     polya-count    - number of colorings via Pólya enumeration
     subset-action  - induced action on k-element subsets
     coloring-action - induced action on k-colorings of n positions"
  (:require [scicloj.harmonica.protocols :as p]))

(defn orbit
  "The orbit of point x under the action of group G.
   act is a function (act g x) → x'.
   Returns a set of all points reachable from x."
  [G act x]
  (set (map #(act % x) (p/elements G))))

(defn orbits
  "Partition domain into orbits under the action of group G.
   Returns a vector of sets."
  [G act domain]
  (loop [remaining (set domain)
         result []]
    (if (empty? remaining)
      result
      (let [x (first remaining)
            orb (orbit G act x)]
        (recur (reduce disj remaining orb)
               (conj result orb))))))

(defn fixed-points
  "The set of points in domain fixed by group element g.
   {x ∈ domain : act(g, x) = x}"
  [act g domain]
  (set (filter #(= (act g %) %) domain)))

(defn stabilizer
  "The stabilizer of point x: the set of group elements that fix x.
   {g ∈ G : act(g, x) = x}"
  [G act x]
  (set (filter #(= (act % x) x) (p/elements G))))

(defn burnside-count
  "Number of orbits via Burnside's lemma:
   |orbits| = (1/|G|) Σ_{g∈G} |Fix(g)|
   Returns a long (always a non-negative integer)."
  [G act domain]
  (let [domain-set (set domain)
        total (reduce + (map #(count (fixed-points act % domain-set))
                             (p/elements G)))]
    (long (/ total (p/order G)))))

(defn- action-cycle-type
  "Compute the cycle type of group element g acting on domain.
   Returns a partition (descending sorted vector of cycle lengths)."
  [act g domain]
  (->> (reduce (fn [[cycles seen] x]
                 (if (seen x)
                   [cycles seen]
                   (let [cycle (->> (iterate #(act g %) x)
                                    (rest)
                                    (take-while #(not= % x))
                                    (into [x]))]
                     [(conj cycles (count cycle))
                      (into seen cycle)])))
               [[] #{}]
               domain)
       (first)
       (sort >)
       (vec)))

(defn cycle-index
  "The cycle index of a group action.
   Returns a map from cycle-type partition to its coefficient (a rational).

   Z(G) = (1/|G|) Σ_{g∈G} p_{λ(g)}

   The coefficient for partition λ is (count of g with that cycle type) / |G|."
  [G act domain]
  (let [n (p/order G)
        freqs (frequencies (map #(action-cycle-type act % domain)
                                (p/elements G)))]
    (into {} (map (fn [[ct cnt]] [ct (/ cnt n)]) freqs))))

(defn polya-count
  "Number of distinct colorings with k colors, via Pólya enumeration.
   Evaluates the cycle index by substituting p_i = k for all i.

   |colorings| = Σ_{λ} coeff(λ) · k^{number of cycles in λ}"
  [cycle-idx k]
  (reduce + (map (fn [[partition coeff]]
                   (* coeff (reduce *' (repeat (count partition) k))))
                 cycle-idx)))

(defn subset-action
  "Induced action of a group on k-element subsets of a domain.
   Given an action (act g x) on individual elements, returns:
     {:act    - function (act g subset) acting on sorted-vector subsets
      :domain - all k-element subsets of domain (as sorted vectors)}"
  [act domain k]
  (let [domain-vec (vec (sort domain))
        subsets (letfn [(combos [items k]
                          (cond
                            (zero? k) [[]]
                            (empty? items) []
                            :else (let [f (first items)
                                        r (rest items)]
                                    (concat
                                     (map #(into [f] %) (combos r (dec k)))
                                     (combos r k)))))]
                  (vec (combos domain-vec k)))
        act-on-subset (fn [g subset]
                        (vec (sort (map #(act g %) subset))))]
    {:act act-on-subset
     :domain subsets}))

(defn coloring-action
  "Induced action of a group on k-colorings of n positions.
   Given a point action (act g i) → i' on positions {0, ..., n-1},
   returns:
     {:act    - function (act g coloring) acting on coloring vectors
      :domain - all k^n colorings as vectors of length n with entries in {0, ..., k-1}}"
  [point-act n k]
  (let [domain (loop [i 0 d [[]]]
                 (if (= i n) d
                     (recur (inc i)
                            (vec (for [prev d c (range k)]
                                   (conj prev c))))))
        act-on-coloring (fn [g coloring]
                          (mapv #(coloring (point-act g %)) (range n)))]
    {:act act-on-coloring
     :domain domain}))
