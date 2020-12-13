(ns advent-of-code-2020.dec13
  (:require [ysera.test :refer [is=]]
            [clojure.string :refer [split]]
            [advent-of-code-2020.cartesian-products :refer [cartesian-prod]]))

(defn parse-simple
  [[timestamp-str ids-row]]
  {:timestamp (read-string timestamp-str)
   :ids       (->> (split ids-row #",")
                   (filter (comp not #{"x"}))
                   (map read-string))})

(defn wait-time
  {:test (fn []
           (is= (wait-time 100 11)
                10))}
  [timestamp id]
  (- id (mod timestamp id)))

(defn shuttle-search
  {:test (fn []
           (is= (shuttle-search ["939" "7,13,x,x,59,x,31,19"])
                295))}
  [strings]
  (let [{timestamp :timestamp
         ids       :ids} (parse-simple strings)]
    (->> ids
         (map #(vector % (wait-time timestamp %)))
         (sort-by second)
         (first)
         (apply *))))
