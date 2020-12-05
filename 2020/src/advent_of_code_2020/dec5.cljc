(ns advent-of-code-2020.dec5)

(defn seat-id
  [str]
  (->> str
       (map #{\B \R})
       (map #(if % 1 0))
       (reduce (fn [agg v]
                 (+ v (* 2 agg))))))

(defn binary-boarding
  [strings]
  (->> strings
       (map seat-id)
       (apply max)))

(defn fasten-seatbelt
  [strings]
  (let [ids (sort (map seat-id strings))]
    (->> (map list ids (rest ids))
         (filter (fn [[a b]]
                   (= (+ a 2) b)))
         (first)
         (first)
         (inc))))
