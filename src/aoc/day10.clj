(ns aoc.day10
  (:require [clojure.string :as str]))

(def input
  (mapv parse-long
        (-> (slurp "data/input10.txt")
            str/trim-newline
            (str/split #","))))

(def ring-len 256)
(def init-state {:pos 0 :ring (range ring-len)})

(defn twist [{:keys [pos ring]} [len skip]]
  (let [double-ring (concat ring ring)
        rev-seg (->> double-ring (drop pos) (take len) reverse)
        new-double-ring (concat (take pos double-ring)
                                rev-seg
                                (drop (+ pos len) double-ring))
        [p1 p2] (partition ring-len new-double-ring)
        new-ring (concat (take pos p2)
                         (drop pos p1))
        new-pos (rem (+ pos len skip) ring-len)]
    {:pos new-pos :ring new-ring}))

(defn part1 []
  (->> (reduce twist init-state (map vector input (range)))
       :ring
       (take 2)
       (apply *)))

(def additional-lens [17 31 73 47 23])
(def rounds 64)

(def input-part2
  (as-> (slurp "data/input10.txt") $
    (str/trim-newline $)
    (map int $)
    (concat $ additional-lens)))

(def rounds-seq
  (map vector
       (take (* rounds (count input-part2)) (cycle input-part2))
       (range)))

(defn part2 []
  (->> (reduce twist init-state rounds-seq)
       :ring
       (partition 16)
       (map #(apply bit-xor %))
       (map #(format "%02x" %))
       (apply str)))

(comment
  (part1)
  (part2))
