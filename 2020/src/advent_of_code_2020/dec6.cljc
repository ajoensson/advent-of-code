(ns advent-of-code-2020.dec6
  (:require [advent-of-code-2020.helpers :refer [split-by-empty-lines]]
            [clojure.set :refer [intersection]]))

(defn custom-customs
  [lines]
  (->> (split-by-empty-lines lines "")
       (map (partial into #{}))
       (map count)
       (apply +)))

(defn actual-custom-customs
  [lines]
  (->> (split-by-empty-lines lines \newline)
       (map (fn [persons]
              (->> persons
                   (partition-by #{\newline})
                   (filter (comp not #{[\newline]}))
                   (map (partial into #{}))
                   (apply intersection))))
       (map count)
       (apply +)))