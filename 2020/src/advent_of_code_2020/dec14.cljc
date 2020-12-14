(ns advent-of-code-2020.dec14
  (:require [ysera.test :refer [is=]]))

(defn read-bin [matcher str]
  (->> str
       (map #(if (matcher %)
               1 0))
       (reduce (fn [agg v]
                 (+ v (* agg
                         2))))))

(def max-int-36
  (- (apply * (repeat 36 2))
     1))

(defn parse-mask
  [string]
  (let [mask (subs string 7)]
    {:ones  (read-bin #{\1} mask)
     :zeros (bit-not (read-bin #{\0} mask))}))

(defn parse-call
  {:test (fn []
           (is= (parse-call "mem[12] = 123")
                {:addr 12
                 :val  123}))}
  [string]
  (let [[_ addr val] (re-matches #"mem\[(\d+)\] = (\d+)" string)]
    {:addr (read-string addr)
     :val  (read-string val)}))

(defn mask-value
  [mask value]
  (bit-and (:zeros mask)
           (bit-or (:ones mask)
                   value)))

(defn parse-inputs
  [mask-parser strings]
  (->> strings
       (partition-by (comp #{"mask"} #(subs % 0 4)))
       (partition-all 2)
       (map (fn [[[mask] & [calls]]]
              {:mask  (mask-parser mask)
               :calls (map parse-call calls)}))))

(defn docking-data
  {:test (fn []
           (is= (docking-data ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                               "mem[8] = 11"
                               "mem[7] = 101"
                               "mem[8] = 0"])
                165))}
  [strings]
  (->> strings
       (parse-inputs parse-mask)
       (map (fn [{mask  :mask
                  calls :calls}]
              (map #(update % :val (partial mask-value mask))
                   calls)))
       (reduce concat)
       (map vals)
       (map vec)
       (into {})
       (vals)
       (apply +)))

(defn parse-floating
  {:test (fn []
           (is= (parse-floating "" "000010")
                ["XXXX1X"])
           (is= (parse-floating "" "00X")
                ["XX0" "XX1"])
           (is= (parse-floating "" "1X00X")
                ["10XX0" "10XX1"
                 "11XX0" "11XX1"]))}
  [prefix string]
  (if (empty? string)
    [prefix]
    (case (first string)
      \0 (recur (str prefix \X) (subs string 1))
      \1 (recur (str prefix \1) (subs string 1))
      \X (concat
           (parse-floating (str prefix \0) (subs string 1))
           (parse-floating (str prefix \1) (subs string 1))))))

(defn parse-floating-mask
  [string]
  (->> (subs string 7)
       (parse-floating "")
       (map parse-mask)))

(defn docking-data-floating
  {:test (fn []
           (is= (docking-data-floating ["mask = 000000000000000000000000000000X1001X"
                                        "mem[42] = 100"
                                        "mask = 00000000000000000000000000000000X0XX"
                                        "mem[26] = 1"])
                208))}
  [strings]
  (->> strings
       (parse-inputs parse-floating-mask)
       (map (fn [{masks :mask
                  calls :calls}]
              (map (fn [call]
                     (map #(update call :addr (partial mask-value %))
                          masks))
                   calls)))
       (flatten)
       (map #(vector (:addr %)
                     (:val %)))
       (into {})
       (vals)
       (apply +)))
