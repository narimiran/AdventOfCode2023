(ns solutions-benchmark
  (:require
   [criterium.core :as c]
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [aoc-utils.core :as aoc]
   day01 day02 day03 day04 day05
   day06 day07 day08 day09 day10
   day11 day12 day13 day14 day15
   day16 day17 day18 day19 day20
   day21 day22 day23 day24 day25))


(defn extract-time [res]
  (->> res
       str/split-lines
       second
       (drop-while #(not (#{\:} %)))
       rest
       str/join))

(def results (atom []))

(def first-day 1)
(def last-day 25)

(doseq [i (range first-day (inc last-day))]
  (println "BENCHMARKING DAY" i)
  (let [inp (aoc/read-input i)
        res (with-out-str
              (c/quick-bench ((eval (symbol (format "day%02d" i) "solve")) inp)))]
    (println res)
    (swap! results conj {:day i :time (extract-time res)}))
  (println "\n\n\n"))


(println "SUMMARY:\n")
(pp/print-table @results)
