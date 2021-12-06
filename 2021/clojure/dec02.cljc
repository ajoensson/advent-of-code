(require '[clojure.string :refer [split]])

(defn parse-input [strs]
  (->> strs
       (map #(split % #" "))
       (map (fn [[command num-str]]
              (let [num (read-string num-str)]
                (case command
                  "forward" {:dim :horizontal
                             :diff num}
                  "down"    {:dim :depth
                             :diff num}
                  "up"      {:dim :depth
                             :diff (- num)}))))))

(defn dive-eval-position [input]
  (->> (reduce (fn [m v] (update m (:dim v) + (:diff v)))
               {:horizontal 0
                :depth 0}
               input)
       (vals)
       (apply *)))

(defn advent-2 []
  (-> ["forward 5"
       "down 5"
       "forward 8"
       "up 3"
       "down 8"
       "forward 2"]
      (parse-input)
      (dive-eval-position)))

(defn dive-eval-position-extra [input]
  (as-> input $
    (reduce (fn [m {action :dim
                    diff :diff}]
              (case action
                :depth      (update m :aim + diff)
                :horizontal (-> m
                                (update :horizontal + diff)
                                (update :depth #(+ % (* (:aim m) diff))))))
            {:horizontal 0 :aim 0 :depth 0}
            $)
    (select-keys $ [:horizontal :depth])
    (vals $)
    (apply * $)))

(defn advent-2-extra []
  (-> ["forward 5"
       "down 5"
       "forward 8"
       "up 3"
       "down 8"
       "forward 2"]
      (parse-input)
      (dive-eval-position-extra)))