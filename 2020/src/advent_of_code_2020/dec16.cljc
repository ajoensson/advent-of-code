(ns advent-of-code-2020.dec16
  (:require [ysera.test :refer [is=]]
            [advent-of-code-2020.helpers :refer [split-by-empty-lines]]
            [clojure.string :as str :refer [split starts-with?]]))

(defn parse-constraint
  {:test (fn []
           (is= (parse-constraint "class: 1-3 or 5-7")
                {:name   "class"
                 :lower  [1 3]
                 :higher [5 7]}))}
  [str]
  (let [[_ name lower1 lower2 higher1 higher2] (re-matches #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" str)]
    {:name   name
     :lower  [(read-string lower1) (read-string lower2)]
     :higher [(read-string higher1) (read-string higher2)]}))

(defn eval-constraint
  {:test (fn []
           (is= (eval-constraint (parse-constraint "class: 1-3 or 5-7")
                                 4)
                false)
           (is= (eval-constraint (parse-constraint "class: 1-3 or 5-7")
                                 8)
                false)
           (is= (eval-constraint (parse-constraint "class: 1-3 or 5-7")
                                 1)
                true))}
  [{lower  :lower
    higher :higher} val]
  (boolean (some identity (map (fn [[l h]]
                                 (<= l val h))
                               [lower higher]))))

(defn ticket-translation
  {:test (fn []
           (is= (ticket-translation ["class: 1-3 or 5-7"
                                     "row: 6-11 or 33-44"
                                     "seat: 13-40 or 45-50"
                                     ""
                                     "your ticket:"
                                     "7,1,14"
                                     ""
                                     "nearby tickets:"
                                     "7,3,47"
                                     "40,4,50"
                                     "55,2,20"
                                     "38,6,12"])
                71))}
  [strings]
  (let [[constraint-str _ nearbys] (split-by-empty-lines strings "\n")
        constraints (map parse-constraint (str/split constraint-str #"\n"))
        vals (rest (map read-string (str/split nearbys #"\n|,")))]
    (->> vals
         (filter #(not (some (fn [c] (eval-constraint c %))
                             constraints)))
         (apply +))))

(defn parse-ticket
  [string]
  (map read-string
       (str/split string #",")))

(defn constraint-order
  {:test (fn []
           (is= (->> (constraint-order [[3 9 18]
                                        [15 1 5]
                                        [5 14 9]]
                                       [(parse-constraint "class: 0-1 or 4-19")
                                        (parse-constraint "row: 0-5 or 8-19")
                                        (parse-constraint "seat: 0-13 or 16-19")])
                     (map :name))
                ["row" "class" "seat"]))}
  [tickets constraints]
  (->> (apply map vector tickets)                           ; zip basically
       (reduce (fn [[found rest] column]
                 (let [constr (first (filter #(every? (partial eval-constraint %)
                                                      column)
                                             rest))]
                   [(conj found constr)
                    (remove #{constr} rest)]))
               [[] constraints])
       (first)))

(defn my-ticket-translation-values
  [strings]
  (let [[constraint-str my-ticket-str nearbys] (split-by-empty-lines strings "\n")
        constraints (map parse-constraint (str/split constraint-str #"\n"))
        my-ticket (parse-ticket
                    (second (str/split my-ticket-str #"\n")))
        tickets (map parse-ticket
                     (rest (str/split nearbys #"\n")))]
    (as-> tickets $
          (filter #(every? (fn [v]
                                  (some (fn [c]
                                          (eval-constraint c v))
                                        constraints))
                       %)
                  $)
          (constraint-order (conj $ my-ticket) constraints)
          (map (fn [val constraint]
                 [(:name constraint)
                  val])
               my-ticket $)
          (filter #(and (first %)
                        (str/starts-with? (first %) "departure"))
                  $)
          (map second $)
          (apply * $))))