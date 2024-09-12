(ns day02
  (:require aoc
            [clojure.string :as str]))


(defn maximums [line]
  (reduce
   (fn [maximums cube]
     (let [[amount color] (str/split cube #" ")]
       (update maximums (keyword color) max (parse-long amount))))
   {:red 0 :green 0 :blue 0}
   (rest line)))


(defn valid? [{:keys [red green blue]}]
  (and (<= red   12)
       (<= green 13)
       (<= blue  14)))

(defn find-valid [games]
  (keep-indexed
   (fn [idx game]
     (when (valid? game) (inc idx)))
   games))

(defn power [game]
  (reduce * (vals game)))


(defn solve [input]
  (let [lines (aoc/parse-input input :words {:word-sep #": |; |, "})
        games (map maximums lines)]
    [(reduce + (find-valid games))
     (aoc/sum-map power games)]))


(solve (aoc/read-file 2))
