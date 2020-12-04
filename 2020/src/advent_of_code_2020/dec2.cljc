(ns advent-of-code-2020.dec2
  (:require [clojure.string :refer [split]]))

(defn extract-policy [str]
  (let [[_ minN maxN letter] (re-matches #"^(\d+)-(\d+) (\D+)" str)]
    {:min  (read-string minN)
     :max  (read-string maxN)
     :char (first letter)}))

(defn checker [n policy [char & rest]]
  (if (nil? char)
    (<= (:min policy)
        n
        (:max policy))
    (checker (if (= char (:char policy))
               (inc n)
               n)
             policy rest)))

(defn check-policy [policy str]
  (checker 0 policy str))

(defn check-index-policy [policy str]
  (->> [:min :max]
       (map (comp (partial = (:char policy))
                  (partial get str)
                  dec
                  policy))
       (apply distinct?)))

(defn password-philosophy [strs]
  (let [[policies strings] (->> strs
                                (map #(split % #": " 2))
                                (apply map list))]
    (->> (map check-policy
              (map extract-policy policies)
              strings)
         (filter true?)
         (count))))

(defn password-philosophy-2 [strs]
  (let [[policies strings] (->> strs
                                (map #(split % #": " 2))
                                (apply map list))]
    (->> (map check-index-policy
              (map extract-policy policies)
              strings)
         (filter true?)
         (count))))
