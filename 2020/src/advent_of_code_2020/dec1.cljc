(ns advent-of-code-2020.dec1)

(defn cart-prod
  [[head & tail]]
  (if (zero? (count tail))
    []
    (into
      (mapv (partial conj [head]) tail)
      (cart-prod tail))))

(defn cart-prod2
  [nums n]
  (if (empty? nums)
    []
    (if (= 1 n)
      (map list nums)
      (let [[head & tail] nums]
        (concat
          (map (partial cons head)
               (cart-prod2 tail (dec n)))
          (cart-prod2 tail n))))))

(defn report-repair-generic [tuples year]
  (->> tuples
       (map (fn [tuple] {:key tuple :sum (apply + tuple)}))
       (filter (comp (partial = year) :sum))
       (map :key)
       (first)
       (apply *)))

(defn report-repair [ns year]
  (-> ns
      (cart-prod)
      (report-repair-generic year)))

(defn report-repair-elves [ns year]
  (-> ns
      (cart-prod2 3)
      (report-repair-generic year)))