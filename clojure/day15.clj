(ns day15
  (:require [aoc-utils.core :as aoc]
            [flatland.ordered.map :refer [ordered-map]]
            [clojure.string :as str]))


(defn word-hash [word]
  (reduce
   (fn [acc c]
     (-> acc
         (+ (int c))
         (* 17)
         (mod 256)))
   0
   word))


(defn parse-instruction [instr]
  (if (str/ends-with? instr "-")
    (let [name (drop-last instr)]
      {:name name
       :pos  (word-hash name)
       :func dissoc})
    (let [name (drop-last 2 instr)]
      {:name  name
       :pos   (word-hash name)
       :func  assoc
       :focal (parse-long (str (last instr)))})))


(defn hashmap [instructions]
  (reduce
   (fn [boxes {:keys [name pos func focal]}]
     (update boxes pos func name focal))
   (vec (repeat 256 (ordered-map)))
   instructions))


(defn focusing-power [boxes]
  (reduce + (for [[i box]       (map-indexed vector boxes)
                  [j [_ focal]] (map-indexed vector box)]
              (* (inc i) (inc j) focal))))


(defn solve [input]
  (let [steps        (aoc/parse-input input :words #",")
        instructions (map parse-instruction steps)]
    [(aoc/sum-map word-hash steps)
     (focusing-power (hashmap instructions))]))


(solve (aoc/read-input 15))
