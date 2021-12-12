(require '[clojure.string :as str])

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #""))
       (map (partial map read-string))))

(defn to-index-map [rows]
  (->> rows
       (mapcat #(map vector (range) (repeat %1) %2)
               (range))
       (mapcat #(vector (vec (take 2 %)) (last %)))
       (apply hash-map)))

(to-index-map (parse-input "34
50"))

(defn get-neighbors [steps max-val x y]
  (let [the-range (map inc (range steps))]
    (->> the-range
         (mapcat #(vector [x (+ y %)]
                          [x (- y %)]
                          [(+ x %) y]
                          [(- x %) y]
                          [(+ x %) (- y %)]
                          [(- x %) (- y %)]
                          [(+ x %) (+ y %)]
                          [(- x %) (+ y %)]))
         (remove (partial some #(or (neg? %)
                                    (>= % max-val)))))))

(defn reset-to-zero [m k v]
  (assoc m k (if (>= v 10)
               0 v)))

(defn do-step [get-neighbors [in-energies in-flashes]]
  (loop [energies in-energies
         [pos & tail] (keys in-energies)
         flashes in-flashes]
    (if (nil? pos)
      [(reduce-kv reset-to-zero (hash-map) energies) flashes]
      (let [n-energies (update energies pos inc)]
        (if (= 10 (n-energies pos))
          (recur
           n-energies
           (concat tail (apply get-neighbors pos))
           (inc flashes))
          (recur n-energies tail flashes))))))

(defn dec11 [input]
  (let [rows (parse-input input)
        max-val (count rows)
        energies (to-index-map rows)
        get-neighbors (partial get-neighbors 1 max-val)]
    (-> (iterate (partial do-step get-neighbors) [energies 0])
        (nth 100)
        (second))))

(dec11 "11111
19991
19191
19991
11111")

(dec11 input-full)

(defn dec11-extra [input]
  (let [rows (parse-input input)
        max-val (count rows)
        energies (to-index-map rows)
        get-neighbors (partial get-neighbors 1 max-val)
        all-steps (iterate (partial do-step get-neighbors) [energies 0])]
    (->> (map vector (range)
              all-steps
              (rest all-steps))
         (filter (fn [[_ [_ flashes-a] [_ flashes-b]]]
                   ; The input is square, so if there were 10*10 flashes, all flashed
                   (= (* max-val max-val)
                      (- flashes-b flashes-a))))
         (ffirst)
         (inc))))

(dec11-extra input-full)

(def input "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def input-full "4438624262
6263251864
2618812434
2134264565
1815131247
2612457325
8585767584
7217134556
2825456563
8248473584")
