(ns advent-of-code-2020.dec4
  (:require [clojure.string :refer [split]]
            [advent-of-code-2020.helpers :refer [split-by-empty-lines]]))

(defn passport-map
  [string]
  (->> (split string #"\s+")
       (map (comp  #(split % #":")))
       (into {})))

(defn all-there
  [input mandatory optional]
  (let [mandatory-set (into #{} mandatory)
        optional-set (into #{} optional)]
    (->> input
         (reduce (fn [ret item]
                (cond
                  ((:mandatory ret) item) (update ret :mandatory #(disj % item))
                  (not (optional-set item)) (update ret :missed #(conj % item))
                  :else ret))
              {:missed [] :mandatory mandatory-set})
         (vals)
         (every? empty?))))

(defn passport-processing
  [strings mandatory optional]
  (as-> (split-by-empty-lines strings) $
        (map (comp keys passport-map) $)
        (filter #(all-there % mandatory optional) $)
        (count $)))

(defn validate-passport-fields
  [m]
  (->> m
       (filter (fn [[k v]]
                 (cond (= k "byr") (<= 1920 (read-string v) 2002)
                       (= k "iyr") (<= 2010 (read-string v) 2020)
                       (= k "eyr") (<= 2020 (read-string v) 2030)
                       (= k "hgt") (let [[_ length-str unit] (re-matches #"(\d+)(\w+)" v)
                                         length (read-string length-str)]
                                     (if (= unit "cm") (<= 150 length 193)
                                                       (<= 59 length 76)))
                       (= k "hcl") (re-matches #"#[0-9a-fA-F]{6}" v)
                       (= k "ecl") (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v)
                       (= k "pid") (re-matches #"\d{9}" v)
                       :else false)))
       (into {})))

(defn advanced-passport-processing
  [strings mandatory optional]
  (->> (split-by-empty-lines strings)
       (map passport-map)
       (map validate-passport-fields)
       (filter (comp #(all-there % mandatory optional) keys))
       (count)))
