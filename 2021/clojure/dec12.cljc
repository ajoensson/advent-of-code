(require '[clojure.string :as str])

(def demo-input
  {"start" ["A" "b"]
   "A" ["start" "end" "c" "b"]
   "b" ["start" "end" "A" "d"]
   "c" ["A"]
   "d" ["b"]})

(defn has-double-visit [path]
  (let [fr (frequencies path)]
    (->> (keys fr)
         (filter #(< 1 (fr %)))
         (filter #(= % (str/lower-case %)))
         empty?
         not)))
(has-double-visit ["a" "b" "C" "C" "end"])

(defn get-paths [link pred current-path]
  (let [nexts (map (partial conj current-path)
                   (get link (first current-path)))
        has-end (group-by #(= "end" (first %)) nexts)]
    (concat (get has-end true)
            (->> (get has-end false)
                 (remove pred)
                 (mapcat (partial get-paths link pred))))))

(count (get-paths demo-input has-double-visit (list "start")))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(str/split % #"-"))
       (reduce (fn [m [a b]]
                 (-> m
                     (update a #(conj % b))
                     (update b #(conj % a)))) {})))

(defn dec12 [input]
    (-> input
        parse-input
        (get-paths has-double-visit '("start"))
        (count)))

(dec12 input-full)

(defn has-single-double-visit [path]
    (let [fr (frequencies path)
            ok? #(or (empty? %)
                    (and (= 1 (count %))
                        (not= "start" (first %))
                        (= 2 (fr (first %)))))]
      (->> (keys fr)
           (filter #(< 1 (fr %)))
           (filter #(= % (str/lower-case %)))
           (ok?))))
(has-single-double-visit ["start" "a" "b" "C" "C" "start" "end"])

(defn dec12-extra [input]
    (-> input
        parse-input
        (get-paths (comp not has-single-double-visit) '("start"))
        (count)))

(dec12-extra input-full)

(def input "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(def input-full "FK-gc
gc-start
gc-dw
sp-FN
dw-end
FK-start
dw-gn
AN-gn
yh-gn
yh-start
sp-AN
ik-dw
FK-dw
end-sp
yh-FK
gc-gn
AN-end
dw-AN
gn-sp
gn-FK
sp-FK
yh-gc")