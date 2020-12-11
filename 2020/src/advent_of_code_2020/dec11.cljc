(ns advent-of-code-2020.dec11
  (:require [ysera.test :refer [is=]]
            [advent-of-code-2020.cartesian-products :refer [cartesian-prod]]))

(defn parse-chairs
  {:test (fn []
           (is= (parse-chairs ["LL.."
                               ".L.L"])
                [[:chair :chair :floor :floor]
                 [:floor :chair :floor :chair]]))}
  [string-rows]
  (->> string-rows
       (map (fn [row]
              (vec (map #(case %
                           \L :chair
                           :floor)
                        row))))
       (vec)))

(defn chairs-and-neighbors-map
  {:test (fn []
           (is= (chairs-and-neighbors-map [[:chair]])
                {[0 0] {:val       :empty
                        :neighbors #{}}})
           (is= (chairs-and-neighbors-map [[:floor :floor :floor]
                                           [:floor :chair :floor]
                                           [:floor :floor :floor]])
                {[1 1] {:val       :empty
                        :neighbors #{}}})
           (is= (chairs-and-neighbors-map [[:chair :floor]
                                           [:floor :chair]])
                {[0 0] {:val       :empty
                        :neighbors #{[1 1]}}
                 [1 1] {:val       :empty
                        :neighbors #{[0 0]}}})
           (is= (chairs-and-neighbors-map [[:chair :chair]
                                           [:chair :chair]])
                {[0 0] {:val       :empty
                        :neighbors #{[1 0] [0 1] [1 1]}}
                 [1 0] {:val       :empty
                        :neighbors #{[0 0] [0 1] [1 1]}}
                 [0 1] {:val       :empty
                        :neighbors #{[0 0] [1 0] [1 1]}}
                 [1 1] {:val       :empty
                        :neighbors #{[0 0] [0 1] [1 0]}}}))}
  [chair-matrix]
  (let [height (count chair-matrix)
        width (count (first chair-matrix))
        get-pos (fn [x y] (get (get chair-matrix y) x))
        neighbors [[-1 -1] [0 -1] [1 -1]
                   [-1 0],,,,,,, [1 0]
                   [-1 1] [0 1] [1 1]]]
    (->> (cartesian-prod (max height width) 2)
         (filter (fn [[x y]] (and (< x width)
                                  (< y height))))
         (map (fn [[x y]] {:pos [x y]
                           :val (get-pos x y)}))
         (filter (comp #{:chair} :val))
         (map (fn [m]
                (assoc m :neighbors
                         (->> neighbors
                              (map (partial map + (:pos m)))
                              (filter (comp #{:chair}
                                            (partial apply get-pos)))
                              set))))
         (map (fn [{pos       :pos
                    neighbors :neighbors}]
                [pos {:val       :empty
                      :neighbors neighbors}]))
         (into {}))))

(defn update-chair
  {:test (fn []
           (is= (-> (update-chair {:val       :empty
                                   :neighbors #{}}
                                  {})
                    :val)
                :occupied)
           (is= (-> (update-chair {:val       :empty
                                   :neighbors #{[1 1] [2 2]}}
                                  {[1 1] {:val :empty}
                                   [2 2] {:val :empty}})
                    :val)
                :occupied)
           (is= (-> (update-chair {:val       :empty
                                   :neighbors #{[1 1] [2 2]}}
                                  {[1 1] {:val :occupied}
                                   [2 2] {:val :empty}})
                    :val)
                :empty)
           (is= (-> (update-chair {:val       :occupied
                                   :neighbors #{}}
                                  {})
                    :val)
                :occupied)
           (is= (-> (update-chair {:val       :occupied
                                   :neighbors #{[1 1] [2 2] [3 3] [4 4] [5 5]}}
                                  {[1 1] {:val :occupied}
                                   [2 2] {:val :occupied}
                                   [3 3] {:val :occupied}
                                   [4 4] {:val :occupied}
                                   [5 5] {:val :occupied}})
                    :val)
                :empty))}
  [chair chair-map]
  (let [state (:val chair)
        occupied-neighbors (->> (:neighbors chair)
                                (filter #(= (:val (get chair-map %))
                                            :occupied))
                                (count))]
    (cond (and (= state :empty)
               (= occupied-neighbors 0))
          (assoc chair :val :occupied)
          (and (= state :occupied)
               (>= occupied-neighbors 4))
          (assoc chair :val :empty)
          :else chair)))

(defn update-chairs
  {:test (fn []
           (let [initial (-> ["L.LL.LL.LL"
                              "LLLLLLL.LL"
                              "L.L.L..L.."
                              "LLLL.LL.LL"
                              "L.LL.LL.LL"
                              "L.LLLLL.LL"
                              "..L.L....."
                              "LLLLLLLLLL"
                              "L.LLLLLL.L"
                              "L.LLLLL.LL"]
                             (parse-chairs)
                             (chairs-and-neighbors-map))]
             (is= (->> initial
                       (update-chairs)
                       (vals)
                       (map :val)
                       (set))
                  #{:occupied})
             (is= (->> initial
                       (update-chairs)
                       (update-chairs)
                       (filter #(= :occupied (:val (second %))))
                       (map first)
                       (set))
                  #{[0 0] [6 0] [8 0] [9 0]
                    [0 1] [9 1]
                    [0 3] [9 3]
                    [0 4]
                    [0 5] [6 5] [8 5] [9 5]
                    [0 7] [9 7]
                    [0 8]
                    [0 9] [2 9] [8 9] [9 9]})))}
  [chair-map]
  (reduce-kv (fn [m k chair]
               (assoc m k (update-chair chair chair-map)))
             {}
             chair-map))

(defn update-until-no-change
  [in-chairs]
  (let [updated-chairs (update-chairs in-chairs)]
    (if (= in-chairs updated-chairs)
      in-chairs
      (recur updated-chairs))))

(defn seating-system
  {:test (fn []
           (is= (seating-system ["L.LL.LL.LL"
                                 "LLLLLLL.LL"
                                 "L.L.L..L.."
                                 "LLLL.LL.LL"
                                 "L.LL.LL.LL"
                                 "L.LLLLL.LL"
                                 "..L.L....."
                                 "LLLLLLLLLL"
                                 "L.LLLLLL.L"
                                 "L.LLLLL.LL"])
                37))}
  [strings-rows]
  (->> strings-rows
       (parse-chairs)
       (chairs-and-neighbors-map)
       (update-until-no-change)
       (filter (fn [[_ {val :val}]] (= val :occupied)))
       (count)))