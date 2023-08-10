(ns aoc.day02
  (:require
   [clojure.string :as str]))

(def input
  (->> (slurp "data/input02.txt")
       (str/split-lines)
       (map #(str/split % #"\s+"))
       (mapv #(mapv parse-long %))))

(defn max-min-diff [coll]
  (- (apply max coll)
     (apply min coll)))

(defn part1 []
  (->> input
       (map max-min-diff)
       (reduce +)))

(defn find-quot [el _ candidate]
  (if (zero? (rem el candidate))
    (reduced (/ el candidate))
    0))

(defn divisable-quot [coll]
  (let [el-candidates (for [i (range (dec (count coll)))]
                        [(nth coll i) (drop (inc i) coll)])]
    (reduce (fn [acc [el candidates]]
              (if (pos? acc) 
                (reduced acc)
                (reduce (partial find-quot el) 0 candidates)))
            0 el-candidates)))

(defn part2 []
  (->> input
       (map #(sort > %))
       (map divisable-quot)
       (reduce +)))

(comment
  (part1)
  (part2))
