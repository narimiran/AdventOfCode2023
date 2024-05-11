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


(defn solve [input]
  (let [document (aoc/parse-input input :ints)]
    [(transduce (map find-winners) * (aoc/transpose document))
     (find-winners (mapv fix-keming document))]))


(solve (aoc/read-file 6))
