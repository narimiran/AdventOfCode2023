(ns day06
  (:require aoc
            [clojure.math :refer [sqrt ceil]]
            [clojure.string :as str]))


(defn fix-keming [nrs]
  (parse-long (str/join nrs)))

(defn find-root [b c]
  (/ (- b (sqrt (- (* b b) (* 4 c)))) 2))

(defn first-larger [root]
  (int (ceil (+ 0.0000001 root))))

(defn find-winners [[time distance]]
  (let [x1 (first-larger (find-root time distance))]
    (inc (- (- time x1) x1))))


(defn solve [input-file]
  (let [[times distances] (aoc/read-input input-file :ints)
        time-2 (fix-keming times)
        distance-2 (fix-keming distances)]
    [(transduce (map find-winners) * (zipmap times distances))
     (find-winners [time-2 distance-2])]))


(solve 6)
