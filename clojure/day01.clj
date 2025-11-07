(ns day01
  (:require [aoc-utils.core :as aoc]
            [clojure.string :as str]))


(def words
  {"one"   "1"
   "two"   "2"
   "three" "3"
   "four"  "4"
   "five"  "5"
   "six"   "6"
   "seven" "7"
   "eight" "8"
   "nine"  "9"})


(def word-pattern (str/join "|" (keys words)))
(def first-patt (re-pattern (str word-pattern #"|\d")))
(def last-patt (re-pattern (str (str/reverse word-pattern) #"|\d")))


(defn calibration-value [line patt rev-patt]
  (let [first-match (re-find patt line)
        last-match  (str/reverse (re-find rev-patt (str/reverse line)))
        first-digit (words first-match first-match)
        last-digit  (words last-match last-match)]
    (parse-long (str first-digit last-digit))))


(defn calibration-sum [lines patt rev-patt]
  (aoc/sum-map #(calibration-value % patt rev-patt) lines))


(defn solve [input]
  (let [lines (aoc/parse-lines input)]
    [(calibration-sum lines #"\d" #"\d")
     (calibration-sum lines first-patt last-patt)]))


(solve (aoc/read-input 1))
