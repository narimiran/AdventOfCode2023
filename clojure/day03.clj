(ns day03
  (:require [aoc-utils.core :as aoc]
            [clojure.string :as str]))


(def digits (set (str/join (range 10))))

(defn symbol-coords [grid]
  (set (keys (remove #(digits (val %)) grid))))


(defn gear-ratios [gear-values]
  (keep
   #(when (= 2 (count %))
      (reduce * %))
   gear-values))

(defn touching [symbs x y len]
  (for [yy (range (dec y) (+ y 2))
        xx (range (dec x) (inc (+ x len)))
        :let [pt [xx yy]]
        :when (symbs pt)]
    pt))


(defn find-solution [lines symbs gears]
  (loop [lines lines
         y 0
         acc 0
         gear-map {}]
    (if-let [line (first lines)]
      (let [[acc' gear-map' _]
            (reduce
             (fn [[acc'' gear-map'' start] n]
               (let [x (str/index-of line n start)
                     number (parse-long n)
                     len (count n)
                     nearby-gears (touching gears x y len)
                     near-symbs? (or (seq nearby-gears)
                                     (seq (touching symbs x y len)))]
                 [(if near-symbs? (+ acc'' number) acc'')
                  (if (seq nearby-gears)
                    (reduce (fn [gmap gear]
                              (update gmap gear conj number))
                            gear-map''
                            nearby-gears)
                    gear-map'')
                  (+ x len)]))
             [acc gear-map 0]
             (re-seq #"\d+" line))]
        (recur (rest lines) (inc y) acc' gear-map'))
      {:num-sum acc
       :gear-ratios (reduce + (gear-ratios (vals gear-map)))})))


(defn solve [input]
  (let [lines    (aoc/parse-lines input)
        points   (aoc/grid->point-map lines #(not= % \.))
        symbs    (symbol-coords points)
        gears    (set (filter #(= \* (points %)) symbs))
        solution (find-solution lines symbs gears)]
    ((juxt :num-sum :gear-ratios) solution)))


(solve (aoc/read-input 3))
