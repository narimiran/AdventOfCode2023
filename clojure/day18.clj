(ns day18
  (:require aoc
            [clojure.string :as str]))


(def deltas {"R" [1 0] "D" [0 1] "L" [-1 0] "U" [0 -1]
             "0" [1 0] "1" [0 1] "2" [-1 0] "3" [0 -1]})


(defn parse-line [line]
  (let [[d n c]    (str/split line #" ")
        c          (str/replace c #"[(#)]" "")
        [dist dir] (map str/join (split-at 5 c))
        dist       (Integer/parseInt dist 16)]
    {:p1 {:dir  (deltas d)
          :dist (parse-long n)}
     :p2 {:dir  (deltas dir)
          :dist dist}}))


(defn dig-trench [input]
  ;; Shoelace formula + Pick's theorem
  (->> input
       (reduce (fn [{:keys [total x]}
                    {:keys [dir dist]}]
                 (let [[dx dy] dir
                       nx      (+ x (* dist dx))
                       area    (* nx dist dy)]
                   {:x     nx
                    :total (+ total area (/ dist 2))}))
               {:total 1
                :x     0})
       :total
       long))


(defn solve [input]
  (let [dig-plan (aoc/parse-input input parse-line)]
    [(dig-trench (map :p1 dig-plan))
     (dig-trench (map :p2 dig-plan))]))


(solve (aoc/read-file 18))
