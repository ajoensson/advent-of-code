(ns advent-of-code-2020.dec8
  (:require [clojure.string :refer [split]]))

(defn parse-instruction
  [str]
  (let [[op numstr] (split str #"\s+")
        num (read-string numstr)]
    ;(println op numstr (= op "nop"))
    (case op
      "acc" {:op  :acc
             :val num}
      "jmp" {:op  :jmp
             :val num}
      "nop" {:op  :jmp
             :val 1})))

(defn execute-op
  [program-state {op-type :op
                  op-val  :val}]
  (case op-type
    :jmp (update program-state :pc (partial + op-val))
    :acc (-> program-state
             (update :acc (partial + op-val))
             (update :pc inc))))

(defn program-runner
  [program program-state]
  (let [{pc      :pc
         ran-ops :ran-ops} program-state
        next-op (get program pc)]
    (cond
      (nil? next-op) {:state :done
                      :acc   (:acc program-state)}
      (ran-ops pc) {:state :inf-loop
                    :acc   (:acc program-state)}
      :else (program-runner program (-> program-state
                                        (update :ran-ops #(conj % pc))
                                        (execute-op next-op))))))

(defn handheld-halting
  [strs]
  (as-> strs $
        (map parse-instruction $)
        (vec $)
        (program-runner $ {:pc      0
                           :acc     0
                           :ran-ops #{}})))

(defn swap-op
  [input]
  (let [[op numstr] (split input #"\s+")]
    (case op
      "nop" (str "jmp " numstr)
      "jmp" (str "nop " numstr)
      input)))

(defn corrupted-program
  [strs]
  (let [len (count strs)
        strs-vec (vec strs)]
    (->> (map (fn [index]
                (update strs-vec index swap-op))
              (range len))
         (map handheld-halting)
         (filter (comp (partial = :done) :state))
         (first)
         (:acc))))