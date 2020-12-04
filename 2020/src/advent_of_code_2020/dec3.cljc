(ns advent-of-code-2020.dec3)

(defn trajectory
  [rightSpeed downSpeed]
  (map (fn [x y]
         [(* rightSpeed x)
          (* downSpeed y)])
       (range)
       (range)))

(defn collissions
  [environment]
  (->> environment
       (map (fn [i row]
              (map vector
                   (range)
                   (repeat i)
                   row))
            (range))
       (reduce into)
       (map #(update % 2 (partial = \#)))
       (filter #(get % 2))
       (map (partial take 2))))

(defn toboggan-trajectory
  [environment rightSpeed downSpeed]
  (let [rows (count environment)
        width (count (first environment))
        collissionCoordinate (apply hash-set (collissions environment))]
    (->> (trajectory rightSpeed downSpeed)
         (take-while #(< (get % 1) rows))
         (map #(update % 0 (fn [d] (mod d width))))
         (filter collissionCoordinate)
         (count))))