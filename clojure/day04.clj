(ns day04
  (:require aoc
            [clojure.string :as str]
            [clojure.math :refer [pow]]))


(defn parse-line [line]
  (let [[_ w y] (str/split line #":|\|")]
    {:winning (set (aoc/integers w))
     :your    (aoc/integers y)}))

(defn winning-numbers [{:keys [winning your]}]
  (count (keep winning your)))

(defn points [amount]
  (if (zero? amount) 0
      (int (pow 2 (dec amount)))))


(defn new-rules [winners]
  (let [size (count winners)]
    (reduce
     (fn [amounts line]
       (let [copies (amounts line)]
         (reduce (fn [amounts delta]
                   (update amounts (+ line delta 1) + copies))
                 amounts
                 (range (winners line)))))
     (vec (repeat size 1))
     (range size))))


(defn solve [input-file]
  (let [lines   (aoc/read-input input-file parse-line)
        winners (mapv winning-numbers lines)]
    [(reduce + (map points winners))
     (reduce + (new-rules winners))]))


(solve 4)
