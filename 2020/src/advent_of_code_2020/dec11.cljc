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
           (is= (chairs-and-neighbors-map [[:chair]] 1)
                {[0 0] {:val       :empty
                        :neighbors #{}}})
           (is= (chairs-and-neighbors-map [[:floor :floor :floor]
                                           [:floor :chair :floor]
                                           [:floor :floor :floor]]
                                          1)
                {[1 1] {:val       :empty
                        :neighbors #{}}})
           (is= (chairs-and-neighbors-map [[:chair :floor]
                                           [:floor :chair]]
                                          1)
                {[0 0] {:val       :empty
                        :neighbors #{[1 1]}}
                 [1 1] {:val       :empty
                        :neighbors #{[0 0]}}})
           (is= (chairs-and-neighbors-map [[:chair :chair]
                                           [:chair :chair]]
                                          1)
                {[0 0] {:val       :empty
                        :neighbors #{[1 0] [0 1] [1 1]}}
                 [1 0] {:val       :empty
                        :neighbors #{[0 0] [0 1] [1 1]}}
                 [0 1] {:val       :empty
                        :neighbors #{[0 0] [1 0] [1 1]}}
                 [1 1] {:val       :empty
                        :neighbors #{[0 0] [0 1] [1 0]}}})
           (is= (chairs-and-neighbors-map [[:chair :floor :floor]
                                           [:floor :floor :floor]
                                           [:floor :floor :chair]]
                                          5)
                {[0 0] {:val       :empty
                        :neighbors #{[2 2]}}
                 [2 2] {:val       :empty
                        :neighbors #{[0 0]}}})
           (is= (chairs-and-neighbors-map [[:chair :floor :floor]
                                           [:floor :chair :floor]
                                           [:floor :floor :chair]]
                                          5)
                {[0 0] {:val       :empty
                        :neighbors #{[1 1]}}
                 [2 2] {:val       :empty
                        :neighbors #{[1 1]}}
                 [1 1] {:val       :empty
                        :neighbors #{[0 0] [2 2]}}})
           (let [case (chairs-and-neighbors-map (vec (repeat 3
                                                             (vec (repeat 3 :chair))))
                                                3)]
             (is= (-> case
                      (get [1 1])
                      (:neighbors)
                      (count))
                  8)
             (is= (-> case
                      (get [0 0])
                      (:neighbors)
                      (count))
                  3)
             (is= (->> (get case [0 0])
                       (:neighbors)
                       (map count))
                  [2 2 2])))}
  [chair-matrix max-distance]
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
                              (map (fn [n]
                                     (->> (range max-distance)
                                          (map (comp (partial map * n)
                                                     (partial repeat 2)
                                                     inc))
                                          (map (partial map + (:pos m)))
                                          (filter (comp #{:chair}
                                                        (partial apply get-pos)))
                                          (first))))
                              (filter (comp not empty?))
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
                                  {} 4)
                    :val)
                :occupied)
           (is= (-> (update-chair {:val       :empty
                                   :neighbors #{[1 1] [2 2]}}
                                  {[1 1] {:val :empty}
                                   [2 2] {:val :empty}}
                                  4)
                    :val)
                :occupied)
           (is= (-> (update-chair {:val       :empty
                                   :neighbors #{[1 1] [2 2]}}
                                  {[1 1] {:val :occupied}
                                   [2 2] {:val :empty}}
                                  4)
                    :val)
                :empty)
           (is= (-> (update-chair {:val       :occupied
                                   :neighbors #{}}
                                  {}
                                  4)
                    :val)
                :occupied)
           (is= (-> (update-chair {:val       :occupied
                                   :neighbors #{[1 1] [2 2] [3 3] [4 4] [5 5]}}
                                  {[1 1] {:val :occupied}
                                   [2 2] {:val :occupied}
                                   [3 3] {:val :occupied}
                                   [4 4] {:val :occupied}
                                   [5 5] {:val :occupied}}
                                  4)
                    :val)
                :empty))}
  [chair chair-map neighbor-limit]
  (let [state (:val chair)
        occupied-neighbors (->> (:neighbors chair)
                                (filter #(= (:val (get chair-map %))
                                            :occupied))
                                (count))]
    (cond (and (= state :empty)
               (= occupied-neighbors 0))
          (assoc chair :val :occupied)

          (and (= state :occupied)
               (>= occupied-neighbors neighbor-limit))
          (assoc chair :val :empty)

          :else
          chair)))

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
                             (chairs-and-neighbors-map 1))]
             (is= (as-> initial $
                        (update-chairs $ 4)
                        (vals $)
                        (map :val $)
                        (set $))
                  #{:occupied})
             (is= (as-> initial $
                        (update-chairs $ 4)
                        (update-chairs $ 4)
                        (filter #(= :occupied (:val (second %))) $)
                        (map first $)
                        (set $))
                  #{[0 0] [6 0] [8 0] [9 0]
                    [0 1] [9 1]
                    [0 3] [9 3]
                    [0 4]
                    [0 5] [6 5] [8 5] [9 5]
                    [0 7] [9 7]
                    [0 8]
                    [0 9] [2 9] [8 9] [9 9]}))
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
                             (chairs-and-neighbors-map 10))]
             (is= (as-> initial $
                        (update-chairs $ 5)
                        (update-chairs $ 5)
                        (filter #(= :occupied (:val (second %))) $)
                        (map first $)
                        (set $))
                  #{[0 0] [9 0]
                    [0 1]
                    [9 7]
                    [0 8]
                    [0 9] [9 9]})
             (is= (as-> initial $
                        (update-chairs $ 5)
                        (update-chairs $ 5)
                        (update-chairs $ 5)
                        (filter #(= :occupied (:val (second %))) $)
                        (map first $)
                        (set $))
                  #{[0, 0], [3, 0], [5, 0], [6, 0], [9, 0]
                    [0, 1], [2, 1], [3, 1], [4, 1], [5, 1], [6, 1]
                    [2, 2], [4, 2], [7, 2]
                    [0, 3], [1, 3], [3, 3], [5, 3], [6, 3], [8, 3], [9, 3]
                    [0, 4], [2, 4], [3, 4], [5, 4], [8, 4], [9, 4]
                    [0, 5], [2, 5], [3, 5], [4, 5], [5, 5], [6, 5], [8, 5]
                    [2, 6], [4, 6]
                    [3, 7], [4, 7], [5, 7], [6, 7], [9, 7]
                    [0, 8], [3, 8], [4, 8], [5, 8], [6, 8], [7, 8]
                    [0, 9], [3, 9], [4, 9], [5, 9], [6, 9], [9, 9]})
             (is= (as-> initial $
                        (update-chairs $ 5)
                        (update-chairs $ 5)
                        (update-chairs $ 5)
                        (update-chairs $ 5)
                        (filter #(= :occupied (:val (second %))) $)
                        (map first $)
                        (set $))
                  #{[0, 0], [3, 0], [6, 0], [9, 0], [0, 1], [7, 2], [0, 3], [1, 3], [9, 3], [9, 4], [0, 5], [9, 7], [0, 8], [7, 8], [0, 9], [3, 9], [6, 9], [9, 9]})))}
  [chair-map neighbor-limit]
  (reduce-kv (fn [m k chair]
               (assoc m k (update-chair chair chair-map neighbor-limit)))
             {}
             chair-map))

(defn update-until-no-change
  [in-chairs neighbor-limit]
  (let [updated-chairs (update-chairs in-chairs neighbor-limit)]
    (if (= in-chairs updated-chairs)
      in-chairs
      (update-until-no-change updated-chairs neighbor-limit))))

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
                                 "L.LLLLL.LL"]
                                1 4)
                37)
           (is= (seating-system ["L.LL.LL.LL"
                                 "LLLLLLL.LL"
                                 "L.L.L..L.."
                                 "LLLL.LL.LL"
                                 "L.LL.LL.LL"
                                 "L.LLLLL.LL"
                                 "..L.L....."
                                 "LLLLLLLLLL"
                                 "L.LLLLLL.L"
                                 "L.LLLLL.LL"]
                                10 5)
                26))}
  [strings-rows look-far neighbor-limit]
  (as-> strings-rows $
        (parse-chairs $)
        (chairs-and-neighbors-map $ look-far)
        (update-until-no-change $ neighbor-limit)
        (filter (fn [[_ {val :val}]] (= val :occupied)) $)
        (count $)))