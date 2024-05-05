(ns day12
  (:require aoc
            [clojure.string :as str]))


(def cache (atom {}))

(def is-operational? #{\. \?})
(def is-damaged? #{\# \?})


(defn parse-line [line]
  (let [[pattern groups] (str/split line #" ")]
    [pattern (aoc/integers groups)]))

(defn unfold [[patts groups]]
  [(->> patts
        (repeat 5)
        (str/join "?"))
   (->> groups
        (repeat 5)
        flatten)])


(defn arrangements [[pattern groups :as pg]]
  (if-let [res (@cache pg)]
    res
    (if (empty? groups)
      (if (every? is-operational? pattern) 1 0)
      (let [[size & tl] groups
            post (+ (reduce + tl) (count tl))
            score (atom 0)]
        (doseq [pre (range (inc (- (count pattern) post size)))
                :let [[before pattern'] (split-at pre pattern)
                      [current remaining] (split-at size pattern')]
                :while (every? is-operational? before)
                :when (every? is-damaged? current)]
          (cond
            (empty? tl) (when (every? is-operational? remaining)
                          (swap! score inc))
            (is-operational? (first remaining))
            (swap! score + (arrangements [(rest remaining) tl]))))
        (swap! cache assoc pg @score)
        @score))))


(defn solve [input]
  (let [lines (aoc/parse-input input parse-line)
        unfolded (map unfold lines)]
    [(reduce + (map arrangements lines))
     (reduce + (pmap arrangements unfolded))]))


(solve (aoc/read-file 12))
