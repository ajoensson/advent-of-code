(ns advent-of-code-2020.dec15
  (:require [ysera.test :refer [is=]]))

(defn say
  [m num idx]
  (assoc m num idx))

(defn memory-game
  {:test (fn []
           (is= (memory-game "0,3,6" 4)
                0)
           (is= (memory-game "0,3,6" 5)
                3)
           (is= (memory-game "0,3,6" 7)
                1)
           (is= (memory-game "0,3,6" 6)
                3)
           (is= (memory-game "0,3,6" 8)
                0)
           (is= (memory-game "0,3,6" 9)
                4)
           (is= (memory-game "0,3,6" 10)
                0)
           (is= (memory-game "0,3,6" 2020)
                436)
           (is= (memory-game "1,3,2" 2020)
                1)
           (is= (memory-game "2,1,3" 2020)
                10)
           (is= (memory-game "1,2,3" 2020)
                27)
           (is= (memory-game "2,3,1" 2020)
                78)
           (is= (memory-game "3,2,1" 2020)
                438)
           (is= (memory-game "3,1,2" 2020)
                1836))}
  [str x]
  (let [nums (map read-string (clojure.string/split str #","))
        n (count nums)
        new-map (->> (map vector nums (range))
                     (into {}))
        old-map (dissoc new-map (last nums))]
    (-> (reduce (fn [[oldm val newm] idx]
                  (let [last-spoken (get oldm val)
                        new-v (if (nil? last-spoken)
                                0
                                (- (dec idx) last-spoken))]
                    [newm new-v (assoc newm new-v idx)]))
                [old-map (last nums) new-map]
                (map (partial + n)
                     (range (- x n))))
        (second))))
