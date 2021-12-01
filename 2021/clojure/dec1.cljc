(defn advent-1 [input]
  (->> (map < input (rest input))
       (apply +)))

(defn advent-1-extra [input]
  (->> (map + input (rest input) (rest (rest input)))
       (advent-1)))
