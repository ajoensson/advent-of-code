(ns advent-of-code-2020.cartesian-products)


(defn cartesian-prod [n p]
  (let [pow (fn [num exponent] (apply * (repeat exponent num)))
        factors (map (partial pow n) (range p))]
    (->> (range (pow n p))
         (map (fn [i]
                (apply vector (map #(mod (int (/ i %)) n)
                                   factors)))))))

(defn unique-p-tuples [n p]
  (filter (partial apply <)
          (cartesian-prod n p)))

(defn p-tuples-of-elements [list p]
  (let [items (into (vector) list)
        tuples (unique-p-tuples (count items) p)]
    (map
      (fn [indexes]
        (apply vector
               (map (partial get items)
                    indexes)))
      tuples)))
