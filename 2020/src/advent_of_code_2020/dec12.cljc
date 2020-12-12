(ns advent-of-code-2020.dec12
  (:require [ysera.test :refer [is=]]))

(defn parse-directions
  [strings]
  (map
    (fn [str]
      (let [res {:value (read-string (subs str 1))}]
        (case (first str)
          \N (assoc res :action :north)
          \E (assoc res :action :east)
          \S (assoc res :action :south)
          \W (assoc res :action :west)
          \F (assoc res :action :forward)
          \L (assoc res
               :action :rotate
               :value (- (/ (:value res)
                            90)))
          \R (assoc res
               :action :rotate
               :value (/ (:value res)
                         90)))))
    strings))

(defn rotate
  {:test (fn []
           (is= (rotate {:direction :east} 1)
                {:direction :south})
           (is= (rotate {:direction :east} -2)
                {:direction :west})
           (is= (rotate {:direction :east} 4)
                {:direction :east})
           (is= (rotate {:direction :east} -4)
                {:direction :east}))}
  [boat steps]
  (let [directions [:north :east :south :west]
        index-of-direction (fn [dir]
                             (->> directions
                                  (map-indexed #(vector %1 (= %2 dir)))
                                  (filter (comp true? second))
                                  (first)
                                  (first)))]
    (update boat :direction #(get directions
                                  (mod (+ (index-of-direction %)
                                          steps)
                                       4)))))

(defn travel
  [boat {action :action
         value  :value}]
  (case action
    :forward (travel boat {:action (:direction boat)
                           :value  value})
    :rotate (rotate boat value)
    :east (update boat :x #(+ % value))
    :west (update boat :x #(- % value))
    :north (update boat :y #(+ % value))
    :south (update boat :y #(- % value))))

(defn rain-risk
  {:test (fn []
           (is= (rain-risk ["F10"
                            "N3"
                            "F7"
                            "R90"
                            "F11"])
                25))}
  [strings]
  (as-> strings $
        (parse-directions $)
        (reduce travel
                {:direction :east
                 :x         0
                 :y         0}
                $)
        (select-keys $ [:x :y])
        (vals $)
        (map #(max % (- %)) $)
        (apply + $)))

(defn waypoint-forward
  {:test (fn []
           (is= (-> (waypoint-forward {:waypoint {:x 3
                                                  :y 5}
                                       :x        1
                                       :y        2}
                                      2)
                    (select-keys [:x :y]))
                {:x 7 :y 12})
           (is= (-> (waypoint-forward {:waypoint {:x -3
                                                  :y -5}
                                       :x        1
                                       :y        2}
                                      2)
                    (select-keys [:x :y]))
                {:x -5 :y -8}))}
  [boat times]
  (reduce-kv (fn [boat key wp-val]
               (update boat
                       key
                       (partial + (* wp-val times))))
             boat
             (:waypoint boat)))

(defn rotate-waypoint
  {:test (fn []
           (is= (rotate-waypoint {:x 2 :y 3} 0)
                {:x 2 :y 3})
           (is= (rotate-waypoint {:x 2 :y 3} 1)
                {:x 3 :y -2})
           (is= (rotate-waypoint {:x 2 :y 3} 2)
                {:x -2 :y -3})
           (is= (rotate-waypoint {:x 2 :y 3} 3)
                {:x -3 :y 2})
           (is= (rotate-waypoint {:x 2 :y 3} -1)
                {:x -3 :y 2}))}
  [waypoint steps]
  (let [step (mod steps 4)
        x (:x waypoint)
        y (:y waypoint)]
    (if (= 0 step)
      waypoint
      (rotate-waypoint {:x y
                        :y (- x)}
                       (dec step)))))

(defn travel-waypoint
  [boat action]
  (let [{act   :action
         value :value} action]
    (if (#{:north :east :south :west} act)
      (update boat :waypoint #(travel % action))
      (case act
        :forward (waypoint-forward boat value)
        :rotate (update boat :waypoint #(rotate-waypoint % value))))))

(defn rain-risk-waypoint
  {:test (fn []
           (is= (rain-risk-waypoint ["F10"
                                     "N3"
                                     "F7"
                                     "R90"
                                     "F11"])
                286))}
  [strings]
  (as-> strings $
        (parse-directions $)
        (reduce travel-waypoint
                {:waypoint {:x 10
                            :y 1}
                 :x        0
                 :y        0}
                $)
        (select-keys $ [:x :y])
        (vals $)
        (map #(max % (- %)) $)
        (apply + $)))
