(ns advent-of-code-2020.dec7
  (:require [clojure.string :refer [replace
                                    split]
             :rename {replace replace-str}]))

(defn parse-bag
  [string]
  (let [stripped-str (replace-str string #"\.| bags?" "")
        [container contents-str] (split stripped-str #" contain ")
        contents (split contents-str #", ")
        parsed-contents (map (fn [str]
                               (let [[num-str bag-name] (split str #" " 2)]
                                 {:bag bag-name
                                  :num (read-string num-str)}))
                             (filter (comp not #{"no other"})
                                     contents))]
    {:bag     container
     :content parsed-contents}))

(defn reverse-bag-hierarchy
  {:test (fn []
           (clojure.test/is (= (reverse-bag-hierarchy [{:bag     "bright red"
                                                        :content [{:bag "quite green"
                                                                   :num 1}
                                                                  {:bag "very blue"
                                                                   :num 2}]}
                                                       {:bag     "very blue"
                                                        :content [{:bag "quite green"
                                                                   :num 10}]}])
                               {"quite green" ["very blue" "bright red"]
                                "very blue"   ["bright red"]})))}
  [bags]
  (reduce (fn [coll
               {bag      :bag
                contents :content}]
            (reduce (fn [coll inner-bag]
                      (update coll inner-bag #(conj % bag)))
                    coll (map :bag contents)))
          {} bags))

(defn find-all-containing-bags
  [bag-strings searched-bag]
  (let [bags (map parse-bag bag-strings)
        hierarchy (reverse-bag-hierarchy bags)
        find-parents (partial get hierarchy)
        find-recurs (fn [bags rec]
                      (if (empty? bags)
                        []
                        (let [parents (apply concat (map find-parents bags))]
                          (apply conj parents (rec parents rec)))))]
    (->> (find-recurs [searched-bag] find-recurs)
         (into #{}))))

(defn total-bags-inside
  {:test (fn []
           (clojure.test/is (= (total-bags-inside [{:bag     "bright red"
                                                    :content [{:bag "quite green"
                                                               :num 1}
                                                              {:bag "very blue"
                                                               :num 2}]}
                                                   {:bag     "very blue"
                                                    :content [{:bag "quite green"
                                                               :num 10}]}]
                                                  "bright red")
                               23)))}
  [bags top-bag]
  (let [bag-map (first (filter #(= (:bag %) top-bag)
                               bags))
        children (:content bag-map)]
    (if (empty? children) 0
                          (apply + (map #(* (:num %)
                                            (inc (total-bags-inside bags
                                                                    (:bag %))))
                                        children)))))

(defn total-bags-inside-str
  [bags-strs top-bag]
  (-> (map parse-bag bags-strs)
      (total-bags-inside top-bag)))