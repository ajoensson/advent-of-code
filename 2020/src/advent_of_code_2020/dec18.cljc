(ns advent-of-code-2020.dec18
  (:require [ysera.test :refer [is=]]))

(defn eval-part
  [expr]
  (let [to-val #(if (number? %)
                  %
                  (eval-part %))]
    (reduce
      (fn [agg [op val]]
        (case op
          * (* agg (to-val val))
          + (+ agg (to-val val))))
      (to-val (first expr))
      (partition 2 (rest expr)))))

(defn eval-row
  {:test (fn []
           (is= (eval-row "2 * 3 + (4 * 5)")
                26)
           (is= (eval-row "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
                13632))}
  [string]
  (eval-part (read-string (str "(" string ")"))))

(defn operation-order
  [strings]
  (->> strings
       (map eval-row)
       (apply +)))

