;; # Day 1: Trebuchet?!
;;




^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns day01-clerk
  {:nextjournal.clerk/auto-expand-results? true}
  (:require aoc
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

;; ## Loading the data

;; Let's load both the example input and the real one:
(def example-input (aoc/read-file "01_test"))
(def input (aoc/read-file 1))

;; Parsing the input is easy, just split the lines of the input:
(def example-data (aoc/parse-input example-input))
(def data (aoc/parse-input input))


;; ## Helpers
;;
;; For Part 2, the numbers can be spelled out with letters.
;; We'll convert them back to digits with the following mappping:
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


;; Now, the regex pattern to match either numbers as words or plain numbers:
(def word-pattern (str/join "|" (keys words)))
(def first-patt (re-pattern (str word-pattern #"|\d")))

;; For Part 2, we need to have these word patterns backwards:
(def last-patt (re-pattern (str (str/reverse word-pattern) #"|\d")))



;; ## Calculate calibration value

;; To calculate the calibration value, we need to find the first and
;; the last number in a line and concat them togeter:
(defn calibration-value [line patt rev-patt]
  (let [first-match (re-find patt line)
        last-match  (str/reverse (re-find rev-patt (str/reverse line)))
        first-digit (words first-match first-match)
        last-digit  (words last-match last-match)]
    (parse-long (str first-digit last-digit))))

;; For example:
(def data-sample (take 10 data))
(clerk/table {"line" data-sample
              "p1 score" (mapv #(calibration-value % #"\d" #"\d") data-sample)
              "p2 score" (mapv #(calibration-value % first-patt last-patt) data-sample)})





;; We need to take a sum of a calibration value of each line:
(defn calibration-sum [lines patt rev-patt]
  (aoc/sum-map #(calibration-value % patt rev-patt) lines))



;; ## Putting it all together:
(defn solve [input]
  (let [lines (aoc/parse-input input)]
    [(calibration-sum lines #"\d" #"\d")
     (calibration-sum lines first-patt last-patt)]))

(solve input)
