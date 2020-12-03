(ns advent-of-code-2020.core
  (:require [clojure.edn :refer [read-string]]
            [advent-of-code-2020.dec1 :refer [report-repair
                                              report-repair-elves]]))

(defn read-ints
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (map read-string (line-seq rdr)))))

(defn dec1
  "Day 1: Report Repair"
  []
  (-> (read-ints "dec1/input")
      (report-repair 2020)))


(defn dec1pt2
  "Day 1: Report Repair"
  []
  (-> (read-ints "dec1/input")
      (report-repair-elves 2020)))

