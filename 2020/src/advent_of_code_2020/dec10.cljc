(ns advent-of-code-2020.dec10
  (:require [ysera.test :refer [is=]]))

(defn add-missing-joltages
  [adapters]
  (conj adapters
        0
        (+ (apply max adapters)
           3)))

(defn increasing-pairwise
  [adapters]
  (map list
       adapters
       (rest adapters)))

(defn jolt-outlets
  {:test (fn []
           (is= (jolt-outlets [16
                               10
                               15
                               5
                               1
                               11
                               7
                               19
                               6
                               12
                               4])
                35))}
  [adapters]
  (->> adapters
       (add-missing-joltages)
       (sort)
       (increasing-pairwise)
       (map (partial apply -))
       (map (partial * -1))
       (group-by identity)
       (filter (comp #{1 3} first))
       (map (comp count second))
       (apply *)))

(defn base-adapter-arrangements
  {:test (fn []
           (is= (base-adapter-arrangements base-adapter-arrangements [0 3])
                1)
           (is= (base-adapter-arrangements base-adapter-arrangements [0 1 3])
                2)
           (is= (base-adapter-arrangements base-adapter-arrangements [0 1 3 6])
                2)
           (is= (base-adapter-arrangements base-adapter-arrangements [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22])
                8))}
  [recur-fn adapters]
  (if (= (count adapters) 2)
    1
    (let [[head & tail] adapters
          next (first (rest tail))
          base-result (recur-fn recur-fn tail)]
      (+ (or (and (> (count tail) 1)
                  (<= (- next head)
                      3)
                  (recur-fn recur-fn
                            (conj (rest tail)
                                  head)))
             0)
         base-result))))

(defn adapter-arrangements
  [adapters]
  (let [rec-fn (memoize base-adapter-arrangements)]
    (->> adapters
         (add-missing-joltages)
         (sort)
         (rec-fn rec-fn))))