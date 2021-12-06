(require '[clojure.string :as str])

(defn parse-input [input]
  (as-> input $
    (str/split $ #"\n")
    (map #(str/split % #"") $)
    (map (partial map #(case %
                         "0" 0
                         "1" 1)) $)))

(defn double-then-add [old-v to-add]
  (-> old-v
      (* 2)
      (+ to-add)))

(defn advent-3 [input]
  (let [parsed (parse-input input)
        n (count parsed)]
    (->> parsed
         (apply map list)
         (map (partial apply +))
         (reduce (fn [m v]
                   (let [is-smaller (if (<= v (/ n 2)) 1 0)]
                     (-> m
                         (update :gamma double-then-add (- 1 is-smaller))
                         (update :epsilon double-then-add is-smaller))))
                 {:gamma 0 :epsilon 0})
         (vals)
         (apply *))))

(defn filter-rows [larger? input idx]
  (if (= 1 (count input))
    input
    (let [n (count input)
          ones (apply + (map #(nth % idx) input))
          most-common (if (>= ones (/ n 2)) 1 0)]
      (filter #(= larger? (= most-common (nth % idx))) input))))

(defn advent-3-extra [input]
  (let [parsed (parse-input input)
        n (count parsed)]
    (->> [true false]
         (map #(reduce (partial filter-rows %) parsed (range n)))
         (map first)
         (map #(reduce double-then-add %))
         (apply *))))

(def input "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")
