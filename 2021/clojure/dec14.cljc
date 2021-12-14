(require '[clojure.string :as str])

(defn get-next [insertion str]
  (let [pairs (map vector str (rest str))]
    (conj (reduce #(conj %1 (second %2)
                         (get insertion %2))
                  '()
                  (reverse pairs))
          (first str))))

(defn parse-input [input]
  (let [[[polymer] _ str-pairs] (partition-by
                                 #{""}
                                 (str/split-lines input))]
    [polymer
     (->> str-pairs
          (map #(let [[key to] (str/split % #" -> ")]
                  [(vec key) (first to)]))
          (reduce concat)
          (apply hash-map))]))

(defn max-minus-min [vals]
  (- (apply max vals)
     (apply min vals)))

(defn dec14 [input]
  (let [[polymer insertion] (parse-input input)]
    (-> polymer
        (->> (iterate (partial get-next insertion)))
        (nth 10)
        (frequencies)
        (vals)
        (max-minus-min))))

(dec14 input-full)

; Doing pairwise recursion (depth-first) with memoization seems better
(def get-num (memoize (fn [insertion pair n]
                        (if (zero? n)
                          (hash-map (first pair) 1)
                          (let [[a b] pair
                                x (insertion pair)]
                            (reduce-kv (fn [m k v]
                                         (update m k #(+ v (or % 0))))
                                       (get-num insertion [a x] (dec n))
                                       (get-num insertion [x b] (dec n))))))))

(defn dec14-extra [input]
  (let [[polymer insertion] (parse-input input)
        pairs (map vector polymer (rest polymer))]
    (-> (map #(get-num insertion % 40) pairs)
        (->> (reduce (partial reduce-kv (fn [m k v]
                                          (update m k #(+ v (or % 0)))))))
        (update (last polymer) inc)
        (vals)
        (max-minus-min))))

(dec14-extra input-full)

(def input "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(def input-full "FSHBKOOPCFSFKONFNFBB

FO -> K
FF -> H
SN -> C
CC -> S
BB -> V
FK -> H
PC -> P
PH -> N
OB -> O
PV -> C
BH -> B
HO -> C
VF -> H
HB -> O
VO -> N
HK -> N
OF -> V
PF -> C
KS -> H
KV -> F
PO -> B
BF -> P
OO -> B
PS -> S
KC -> P
BV -> K
OC -> B
SH -> C
SF -> P
NH -> C
BS -> C
VH -> F
CH -> S
BC -> B
ON -> K
FH -> O
HN -> O
HS -> C
KK -> V
OK -> K
VC -> H
HV -> F
FS -> H
OV -> P
HF -> F
FB -> O
CK -> O
HP -> C
NN -> V
PP -> F
FC -> O
SK -> N
FN -> K
HH -> F
BP -> O
CP -> K
VV -> S
BO -> N
KN -> S
SB -> B
SC -> H
OS -> S
CF -> K
OP -> P
CO -> C
VK -> C
NB -> K
PB -> S
FV -> B
CS -> C
HC -> P
PK -> V
BK -> P
KF -> V
NS -> P
SO -> C
CV -> P
NP -> V
VB -> F
KO -> C
KP -> F
KH -> N
VN -> S
NO -> P
NF -> K
CB -> H
VS -> V
NK -> N
KB -> C
SV -> F
NC -> H
VP -> K
PN -> H
OH -> K
CN -> N
BN -> F
NV -> K
SP -> S
SS -> K
FP -> S")
