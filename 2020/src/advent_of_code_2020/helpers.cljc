(ns advent-of-code-2020.helpers
  (:require [clojure.string :refer [join]]))

(defn split-by-empty-lines
  ([lines] (split-by-empty-lines lines \space))
  ([lines join-with]
   (->> lines
        (partition-by empty?)
        (map (partial join join-with))
        (filter (comp not empty?)))))
