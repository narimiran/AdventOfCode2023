(ns aoc-tests
  (:require
   aoc
   [clojure.test :refer [deftest testing is run-tests successful?]]))


(def int-lines "123\n-456\n789")
(def ints-lines "1 2 3\n4 -5 6\n7 8 9")
(def char-lines "abc\ndef\nghi")
(def word-lines "a b c\nd e f\ng h i")
(def comma-sep "ab cd,ef gh\nij kl,mn op")
(def int-paragraphs "1,2\n3,4\n\n5,6\n7,8")

(defn test-parsing
  ([input result] (test-parsing input nil result))
  ([input f result]
   (is (= result (aoc/parse-input input f)))))

(deftest parsing
  (testing "digits"
    (is (= [1 2 3] (aoc/string->digits "123")))
    (is (= [1 2] (aoc/string->digits "ab1cd2e"))))
  (testing "ints"
    (test-parsing int-lines         ["123" "-456" "789"])
    (test-parsing int-lines :int    [123 -456 789])
    (test-parsing int-lines :digits [[1 2 3] [4 5 6] [7 8 9]])
    (test-parsing ints-lines :ints  [[1 2 3] [4 -5 6] [7 8 9]]))
  (testing "chars"
    (test-parsing char-lines        ["abc" "def" "ghi"])
    (test-parsing char-lines :chars [[\a \b \c] [\d \e \f] [\g \h \i]]))
  (testing "words"
    (test-parsing word-lines        ["a b c" "d e f" "g h i"])
    (test-parsing word-lines :words [["a" "b" "c"] ["d" "e" "f"] ["g" "h" "i"]]))
  (testing "custom func"
    (test-parsing int-lines #(mod (abs (parse-long %)) 10) [3 6 9]))
  (testing "separators"
    (is (= [["ab" "cd,ef" "gh"] ["ij" "kl,mn" "op"]]
           (aoc/parse-input comma-sep :words)))
    (is (= [["ab cd" "ef gh"] ["ij kl" "mn op"]]
           (aoc/parse-input comma-sep :words {:word-sep #","})))
    (is (= [["ab" "cd" "ef" "gh"] ["ij" "kl" "mn" "op"]]
           (aoc/parse-input comma-sep :words {:word-sep #",| "}))))
  (testing "pragraphs"
    (is (= ["1,2\n3,4" "5,6\n7,8"]
           (aoc/parse-input int-paragraphs nil {:nl-sep #"\n\n"})))
    (is (= [[1 2 3 4] [5 6 7 8]]
           (aoc/parse-input int-paragraphs :ints {:nl-sep #"\n\n"})))))



(def pt1 [2 3])
(def pt2 [7 -5])

(defn test-neighbours [pt amount result]
  (is (= result (aoc/neighbours amount pt))))

(deftest points
  (is (= 12 (aoc/manhattan pt2)))
  (is (= 13 (aoc/manhattan pt1 pt2)))
  (is (= [9 -2] (aoc/pt+ pt1 pt2)))

  (is (aoc/inside? 10 5 7))
  (is (not (aoc/inside? 10 5 17)))
  (is (aoc/inside? 10 0 0))
  (is (not (aoc/inside? 10 0 -1)))
  (is (aoc/inside? 10 20 9 17))
  (is (not (aoc/inside? 10 20 17 9)))

  (test-neighbours pt1 4 '(      [2 2]
                           [1 3]       [3 3]
                                 [2 4]))

  (test-neighbours pt1 5 '(      [2 2]
                           [1 3] [2 3] [3 3]
                                 [2 4]))

  (test-neighbours pt1 8 '([1 2] [2 2] [3 2]
                           [1 3]       [3 3]
                           [1 4] [2 4] [3 4]))

  (test-neighbours pt1 9 '([1 2] [2 2] [3 2]
                           [1 3] [2 3] [3 3]
                           [1 4] [2 4] [3 4])))



(def grid ["#.." "..#" "##."])
(def walls {[0 0] \# , [2 1] \# , [0 2] \# , [1 2] \#})

(deftest vec->map
  (is (= walls (aoc/grid->points grid #{\#}))))



(def evens [2 4 6 8 -24 156])
(def stevens [2 4 6 21 32])

(deftest helpers
  (testing "none?"
    (is (aoc/none? odd? evens))
    (is (not (aoc/none? odd? stevens))))
  (testing "count-if"
    (is (= 6 (aoc/count-if even? evens)))
    (is (zero? (aoc/count-if odd? evens)))
    (is (= 4 (aoc/count-if even? stevens)))
    (is (= 1 (aoc/count-if odd? stevens))))
  (testing "find-first"
    (is (= 2 (aoc/find-first even? evens)))
    (is (nil? (aoc/find-first odd? evens)))
    (is (= 21 (aoc/find-first odd? stevens)))))


(deftest gcd-lcm
  (testing "gcd"
    (is (= 1 (aoc/gcd 2 3)))
    (is (= 4 (aoc/gcd 4 12)))
    (is (= 5 (aoc/gcd 25 15)))
    (is (= 1 (aoc/gcd 7 17))))
  (testing "lcm"
    (is (= 6 (aoc/lcm 2 3)))
    (is (= 12 (aoc/lcm 4 12)))
    (is (= 75 (aoc/lcm 25 15)))
    (is (= (* 7 17) (aoc/lcm 7 17)))))




(let [summary (run-tests)]
  (when-not (successful? summary)
    (throw (Exception. "some tests failed"))))
