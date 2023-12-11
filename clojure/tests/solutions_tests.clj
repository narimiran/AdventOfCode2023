(ns solutions-tests
  (:require
   day01 day02 day03 day04 day05
   day06 day07 day08 day09 day10
   day11 ; day12 day13 day14 day15
   ; day16 day17 day18 day19 day20
   ; day21 day22 day23 day24 day25
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
         (is (= (~solve-fn ~test-input) ~test-results)))
       (is (= (~solve-fn ~real-input) ~real-results)))))




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

(let [summary (run-tests)]
  (when-not (successful? summary)
    (throw (Exception. "tests failed"))))
