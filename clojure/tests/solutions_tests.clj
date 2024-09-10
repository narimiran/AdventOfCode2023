(ns solutions-tests
  (:require
   day01 day02 day03 day04 day05
   day06 day07 day08 day09 day10
   day11 day12 day13 day14 day15
   day16 day17 day18 day19 day20
   day21 day22 day23 day24 day25
   [clojure.test :refer [deftest is run-tests successful?]]))



(defmacro check-day [day test-results real-results]
  (let [day        (format "%02d" day)
        full-day   (str "day" day)
        solve-fn   (symbol (str full-day "/solve"))
        test-name  (symbol (str full-day "-test"))
        test-input (str day "_test")
        real-input day]
    `(deftest ~test-name
       (when ~test-results
         (is (= ~test-results (~solve-fn (aoc/read-file ~test-input)))))
       (is (= ~real-results (~solve-fn (aoc/read-file ~real-input)))))))




(check-day 1 nil [56049 54530])
(check-day 2 [8 2286] [2239 83435])
(check-day 3 [4361 467835] [528799 84907174])
(check-day 4 [13 30] [17803 5554894])
(check-day 5 [35 46] [579439039 7873084])
(check-day 6 [288 71503] [503424 32607562])
(check-day 7 [6440 5905] [253954294 254837398])
(check-day 8 nil [11567 9858474970153])
(check-day 9 [114 2] [1581679977 889])
(check-day 10 [70 8] [6864 349])
(check-day 11 [374 82000210] [9724940 569052586852])
(check-day 12 [21 525152] [7286 25470469710341])
(check-day 13 [405 400] [32723 34536])
(check-day 14 [136 64] [108857 95273])
(check-day 15 [1320 145] [498538 286278])
(check-day 16 [46 51] [6921 7594])
(check-day 17 [102 94] [758 892])
(check-day 18 [62 952408144115] [47045 147839570293376])
(check-day 19 [19114 167409079868000] [386787 131029523269531])
(check-day 20 nil [836127690 240914003753369])
(check-day 21 nil [3697 608152828731262])
(check-day 22 [5 7] [418 70702])
(check-day 23 nil [2394 6554])
(check-day 24 nil [13892 843888100572888])
(check-day 25 54 603368)


(let [summary (run-tests)]
  (when-not (successful? summary)
    (throw (Exception. "tests failed"))))
