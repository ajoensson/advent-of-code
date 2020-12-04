(ns advent-of-code-2020.core
  (:require [advent-of-code-2020.dec1 :refer [report-repair
                                              report-repair-elves]]
            [advent-of-code-2020.dec2 :refer [password-philosophy
                                              password-philosophy-2]]
            [advent-of-code-2020.dec3 :refer [toboggan-trajectory]]))

(defn read-strs
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr))))

(defn read-ints
  [filename]
  (map read-string (read-strs filename)))

(defn dec1
  "Day 1: Report Repair"
  []
  (-> (read-ints "inputs/dec1")
      (report-repair 2020)))


(defn dec1pt2
  "Day 1: Report Repair"
  []
  (-> (read-ints "inputs/dec1")
      (report-repair-elves 2020)))

(defn dec2
  "Day 2: Password Philosphy"
  []
  (-> (read-strs "inputs/dec2")
      (password-philosophy)))

(defn dec2pt2
  "Day 2: Password Philosphy"
  []
  (-> (read-strs "inputs/dec2")
      (password-philosophy-2)))

(defn dec3
  "Day 2: Toboggan Trajectory"
  []
  (-> (read-strs "inputs/dec3")
      (toboggan-trajectory 3 1)))

(defn dec3pt2
  "Day 2: Toboggan Trajectory 2"
  []
  (let [environment (read-strs "inputs/dec3")
        slopes [[1 1]
                [3 1]
                [5 1]
                [7 1]
                [1 2]]]
    (->> slopes
         (map (partial apply toboggan-trajectory environment))
         (reduce *))))
