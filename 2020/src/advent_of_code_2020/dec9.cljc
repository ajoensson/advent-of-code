(ns advent-of-code-2020.dec9
  (:require [advent-of-code-2020.cartesian-products :refer [p-tuples-of-elements]]
            [ysera.test :refer [is=]]))

(def example-input [35
                    20
                    15
                    25
                    47
                    40
                    62
                    55
                    65
                    95
                    102
                    117
                    150
                    182
                    127
                    219
                    299
                    277
                    309
                    576])

(defn check-n-previous
  {:test (fn []
           (is= (check-n-previous example-input 5)
                127))}
  [items n]
  (->> (map (partial #(drop % items)) (range))
       (map (fn [items]
              (let [[prev upcoming] (split-at n items)
                    val (first upcoming)]
                {:val    val
                 :exists (->> (map (partial apply +) (p-tuples-of-elements prev 2))
                              (filter #{val})
                              (first))})))
       (filter (comp nil? :exists))
       (first)
       (:val)))

(defn smallest-contiguous-list
  [items searched-value]
  (->> (map (partial #(drop % items)) (range (count items)))
       (map (fn [rest-items]
              (->> (map #(take (inc %) rest-items) (range))
                   (take-while #(<= (apply + %) searched-value))
                   (filter #(= (apply + %) searched-value)))))
       (filter not-empty)
       (first)
       (first)))

(defn smallest-contiguous-sum
  {:test (fn []
           (is= (smallest-contiguous-sum example-input 127)
                62))}
  [items searched-value]
  (let [found-list (smallest-contiguous-list items searched-value)]
    (+ (apply min found-list)
       (apply max found-list))))