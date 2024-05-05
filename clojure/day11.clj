(ns day11
  (:require aoc))


(def ^:const multi (dec 1000000))


(defn find-galaxies [lines]
  (for [[y line] (map-indexed vector lines)
        [x chr] (map-indexed vector line)
        :when (= chr \#)]
    [x y]))


(defn empty-lines [lines]
  (keep
   (fn [[i line]]
     (when (every? #{\.} line) i))
   (map-indexed vector lines)))


(defn calc-dist [galaxies]
  (->> galaxies
       (reduce (fn [{:keys [coeff] :as acc} coord]
                 (-> acc
                     (update :sum + (* coeff coord))
                     (update :coeff + 2)))
               {:sum 0
                :coeff (- 1 (count galaxies))})
       :sum))


(defn expansion [galaxies empties]
  (let [total (count galaxies)]
    (reduce
     (fn [acc coord]
       (let [before (count (take-while #(< % coord) galaxies))
             after (- total before)]
         (+ acc (* before after))))
     0
     empties)))


(defn solve [input]
  (let [lines (aoc/parse-input input)
        galaxies (find-galaxies lines)
        galaxies-x (sort (map first galaxies))
        galaxies-y (sort (map second galaxies))
        empty-rows (empty-lines lines)
        empty-cols (empty-lines (aoc/transpose lines))
        distances (+ (calc-dist galaxies-x)
                     (calc-dist galaxies-y))
        expansions (+ (expansion galaxies-x empty-cols)
                      (expansion galaxies-y empty-rows))]
    [(+ distances expansions)
     (+ distances (* multi expansions))]))


(solve (aoc/read-file 11))
