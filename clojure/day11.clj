(ns day11
  (:require aoc))


(def ^:const multi (dec 1000000))


(defn empty-lines [lines]
  (keep
   (fn [[i line]]
     (when (every? #(= % \.) line) i))
   (map-indexed vector lines)))

(defn expand [universe]
  (let [empty-rows (empty-lines universe)
        empty-cols (empty-lines (aoc/transpose universe))]
    (for [[^long y line] (map-indexed vector universe)
          [^long x chr]  (map-indexed vector line)
          :when (= chr \#)
          :let [move-right (aoc/count-if #(< ^long % x) empty-cols)
                move-down  (aoc/count-if #(< ^long % y) empty-rows)]]
      [[(+ x move-right)
        (+ y move-down)]
       [(+ x (* multi move-right))
        (+ y (* multi move-down))]])))

(defn distances [coords]
  (for [[^long x1 ^long y1] coords
        [^long x2 ^long y2] coords
        :while (not (and (= x1 x2) (= y1 y2)))]
    (+ (- x1 x2) (abs (- y1 y2)))))

(defn solve [input-file]
  (let [universe (->> (aoc/read-input input-file)
                      expand
                      sort)
        p1-galaxies (map first universe)
        p2-galaxies (map second universe)]
    [(reduce + (distances p1-galaxies))
     (reduce + (distances p2-galaxies))]))


(solve 11)
