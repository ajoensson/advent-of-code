(ns advent-of-code-2020.core
  (:require [advent-of-code-2020.dec1 :refer [report-repair
                                              report-repair-elves]]
            [advent-of-code-2020.dec2 :refer [password-philosophy
                                              password-philosophy-2]]
            [advent-of-code-2020.dec3 :refer [toboggan-trajectory]]
            [advent-of-code-2020.dec4 :refer [passport-processing
                                              advanced-passport-processing]]
            [advent-of-code-2020.dec5 :refer [binary-boarding
                                              fasten-seatbelt]]
            [advent-of-code-2020.dec6 :refer [custom-customs
                                              actual-custom-customs]]
            [advent-of-code-2020.dec7 :refer [find-all-containing-bags
                                              total-bags-inside-str]]
            [advent-of-code-2020.dec8 :refer [handheld-halting
                                              corrupted-program]]
            [advent-of-code-2020.dec9 :refer [check-n-previous
                                              smallest-contiguous-sum]]
            [advent-of-code-2020.dec10 :refer [jolt-outlets
                                               adapter-arrangements]]
            [advent-of-code-2020.dec11 :refer [seating-system]]))

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
  "Day 3: Toboggan Trajectory"
  []
  (-> (read-strs "inputs/dec3")
      (toboggan-trajectory 3 1)))

(defn dec3pt2
  "Day 3: Toboggan Trajectory 2"
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

(defn dec4
  "Day 4: Passport Processing"
  []
  (-> (read-strs "inputs/dec4")
      (passport-processing
        ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]
        ["cid"])))

(defn dec4pt2
  "Day 4: Passport Processing"
  []
  (-> (read-strs "inputs/dec4")
      (advanced-passport-processing
        ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]
        ["cid"])))

(defn dec5
  "Day 5: Binary Boarding"
  []
  (-> (read-strs "inputs/dec5")
      (binary-boarding)))

(defn dec5pt2
  "Day 5: Binary Boarding"
  []
  (-> (read-strs "inputs/dec5")
      (fasten-seatbelt)))

(defn dec6
  "Day 6: Custom Customs"
  []
  (-> (read-strs "inputs/dec6")
      (custom-customs)))

(defn dec6pt2
  "Day 6: Custom Customs"
  []
  (-> (read-strs "inputs/dec6")
      (actual-custom-customs)))

(defn dec7
  "Day 7: Handy Haversacks"
  []
  (-> (read-strs "inputs/dec7")
      (find-all-containing-bags "shiny gold")
      (count)))

(defn dec7pt2
  "Day 7: Handy Haversacks"
  []
  (-> (read-strs "inputs/dec7")
      (total-bags-inside-str "shiny gold")))

(defn dec8
  "Day 8: Handheld Halting"
  []
  (-> (read-strs "inputs/dec8")
      (handheld-halting)
      (:acc)))

(defn dec8pt2
  "Day 8: Handheld Halting"
  []
  (-> (read-strs "inputs/dec8")
      (corrupted-program)))

(defn dec9
  "Day 9: Encoding Error"
  []
  (-> (read-ints "inputs/dec9")
      (check-n-previous 25)))

(defn dec9pt2
  "Day 9: Encoding Error"
  []
  (let [ints (read-ints "inputs/dec9")]
    (smallest-contiguous-sum ints
                             (check-n-previous ints 25))))

(defn dec10
  "Day 10: Adapter Array"
  []
  (-> (read-ints "inputs/dec10")
      (jolt-outlets)))

(defn dec10pt2
  "Day 10: Adapter Array"
  []
  (-> (read-ints "inputs/dec10")
      (adapter-arrangements)))

(defn dec11
  "Day 11: Seating System"
  []
  (-> (read-strs "inputs/dec11")
      (seating-system)))
