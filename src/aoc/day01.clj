(ns aoc.day01
  (:require
   [clojure.string :as str]))

(def input
  (->> (slurp "data/input01.txt")
       str/trim-newline
       vec
       (mapv #(parse-long (str %)))))

(defn part1 []
  (->> input
       first
       (conj input)
       (partition 2 1)
       (filter (fn [[a b]] (= a b)))
       (map first)
       (reduce +)))

(defn part2 []
  (let [len (count input)
        n (/ len 2)
        extended (into [] (take (+ len n) (cycle input)))
        pairs (for [i (range len)] [(extended i) (extended (+ i n))])]
    (->> pairs
         (filter (fn [[a b]] (= a b)))
         (map first)
         (reduce +))))

(comment
  (part1)
  (part2))
