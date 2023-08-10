(ns aoc.day05
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "data/input05.txt")
       (str/split-lines)
       (mapv parse-long)))

(defn jump [{:keys [pos table]}]
  (let [offset (get table pos)
        new-pos (+ pos offset)
        table (assoc table pos (inc offset))]
    {:pos new-pos :table table}))

(defn check-state [jumps {:keys [pos table]}]
  (if (nil? (get table pos))
    (reduced jumps)
    (inc jumps)))

(defn part1 []
  (reduce check-state 0 (iterate jump {:pos 0 :table input})))

(defn part1-v2 []
  (->> (map vector (range) (iterate jump {:pos 0 :table input}))
       (drop-while (fn [[_ {:keys [pos table]}]] (some? (get table pos))))
       ffirst))

(defn jump-part2 [{:keys [pos table]}]
  (let [offset (get table pos)
        new-pos (+ pos offset)
        new-offset (if (> offset 2) (dec offset) (inc offset))
        table (assoc table pos new-offset)]
    {:pos new-pos :table table}))

(defn part2 []
  (reduce check-state 0 (iterate jump-part2 {:pos 0 :table input})))

(comment
  (part1)
  (part1-v2)
  (dotimes [_ 10] (time (dotimes [_ 10] (part1)))) ; "Elapsed time: 480.911584 msecs"
  (dotimes [_ 10] (time (dotimes [_ 10] (part1-v2)))) ; "Elapsed time: 738.415167 msecs"
  (part2)
  (dotimes [_ 10] (time (part2)))) ; "Elapsed time: 3388.156667 msecs"
