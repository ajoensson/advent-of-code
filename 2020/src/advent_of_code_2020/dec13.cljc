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

(defn parse-extended
  {:test (fn []
           (is= (parse-extended ["939" "7,13,x,x,59,x,31,19"])
                [{:num  [7 13]
                  :diff 1}
                 {:num  [13 59]
                  :diff 3}
                 {:num  [59 31]
                  :diff 2}
                 {:num  [31 19]
                  :diff 1}])
           (is= (parse-extended ["" "67,7,59,61"])
                [{:num  [67 7]
                  :diff 1}
                 {:num  [7 59]
                  :diff 1}
                 {:num  [59 61]
                  :diff 1}]))}
  [[_ id-strs]]
  (->> (split id-strs #",")
       (reduce (fn [[agg prev] str]
                 (if (= "x" str)
                   [agg (update prev :diff inc)]
                   [(conj agg {:num  [(:v prev) (read-string str)]
                               :diff (:diff prev)})
                    {:diff 1 :v (read-string str)}]))
               [[] {:diff 1 :v -1}])
       (first)
       (rest)))

(defn div-ceil
  {:test (fn []
           (is= (div-ceil 33 12)
                3)
           (is= (div-ceil 100 13)
                8)
           (is= (div-ceil 33 3)
                11)
           (is= (div-ceil 2 3)
                1))}
  [dividend divisor]
  (bigint (/ (+ dividend (dec divisor))
             divisor)))

(defn prod-that-makes-diff
  {:test (fn []
           (is= (prod-that-makes-diff {:num  [7 13]
                                       :diff 1})
                77)
           (is= (prod-that-makes-diff {:num  [17 13]
                                       :diff 2})
                102)
           (is= (prod-that-makes-diff {:num  [(* 17 13) 19]
                                       :diff 105})
                3315))}
  [{[a b] :num
    diff  :diff}]
  (let []
    (loop [factor-a 1
           factor-b 1]
      (let [a-val (* a factor-a)
            b-val (* b factor-b)
            d-diff (- (+ a-val diff)
                      b-val)]
        (cond
          (zero? d-diff) a-val
          (neg? d-diff) (recur (inc factor-a) factor-b)
          (pos? d-diff) (recur factor-a (+ factor-b
                                           (div-ceil d-diff b))))))))

(defn real-shuttle
  {:test (fn []
           (is= (real-shuttle ["939" "17,x,13,19"])
                3417)
           (is= (real-shuttle ["" "67,7,59,61"])
                754018)
           (is= (real-shuttle ["" "67,x,7,59,61"])
                779210)
           (is= (real-shuttle ["" "67,7,x,59,61"])
                1261476)
           (is= (real-shuttle ["" "1789,37,47,1889"])
                1202161486)
           (is= (real-shuttle ["" "7,13,x,x,59,x,31,19"])
                1068781))}
  [strings]
  (let [num-diffs (parse-extended strings)
        res (reduce (fn [prev {[a b] :num
                               diff  :diff}]
                      (let [in-diff (+ (:diff prev) diff)]
                        (-> prev
                            (assoc :diff (+ (prod-that-makes-diff {:num  [(* (:v prev) a) b]
                                                                   :diff in-diff})
                                            in-diff))
                            (update :v (partial * a))
                            (update :tot-diff (partial + diff)))))
                    {:v        1
                     :diff     0
                     :tot-diff 0}
                    num-diffs)]
    (- (:diff res) (:tot-diff res))))
