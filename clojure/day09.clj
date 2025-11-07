(ns day09
  (:require [aoc-utils.core :as aoc]))


(defn extrapolate [history]
  (loop [[hd & tl :as values] history
         res 0]
    (if (every? zero? values) res
        (recur (map - values tl) (+ res hd)))))


(defn prev-sum [transformation histories]
  (aoc/sum-pmap (comp extrapolate transformation) histories))


(defn solve [input]
  (let [histories (aoc/parse-lines input :ints)]
    [(prev-sum rseq histories)
     (prev-sum identity histories)]))


(solve (aoc/read-input 9))
