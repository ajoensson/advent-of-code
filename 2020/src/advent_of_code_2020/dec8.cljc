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
    (if (ran-ops pc)
      (:acc program-state)
      (program-runner program (-> program-state
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
