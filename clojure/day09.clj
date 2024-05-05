(ns day09
  (:require aoc))


(defn extrapolate [history]
  (loop [[hd & tl :as values] history
         res 0]
    (if (every? zero? values) res
        (recur (map - values tl) (+ res hd)))))


(defn prev-sum [transformation histories]
  (->> histories
       (pmap (comp extrapolate transformation))
       (reduce +)))


(defn solve [input]
  (let [histories (aoc/parse-input input :ints)]
    [(prev-sum rseq histories)
     (prev-sum identity histories)]))


(solve (aoc/read-file 9))
