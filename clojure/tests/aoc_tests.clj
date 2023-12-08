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
   (is (= (aoc/parse-multiline-string input f) result))))

(deftest parsing
  (testing "digits"
    (is (= (aoc/string->digits "123") [1 2 3]))
    (is (= (aoc/string->digits "ab1cd2e") [1 2])))
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
    (is (= (aoc/parse-multiline-string comma-sep :words)
           [["ab" "cd,ef" "gh"] ["ij" "kl,mn" "op"]]))
    (is (= (aoc/parse-multiline-string comma-sep :words {:word-sep #","})
           [["ab cd" "ef gh"] ["ij kl" "mn op"]]))
    (is (= (aoc/parse-multiline-string comma-sep :words {:word-sep #",| "})
           [["ab" "cd" "ef" "gh"] ["ij" "kl" "mn" "op"]])))
  (testing "pragraphs"
    (is (= (aoc/parse-multiline-string int-paragraphs nil {:nl-sep #"\n\n"})
           ["1,2\n3,4" "5,6\n7,8"]))
    (is (= (aoc/parse-multiline-string int-paragraphs :ints {:nl-sep #"\n\n"})
           [[1 2 3 4] [5 6 7 8]]))))



(def pt1 [2 3])
(def pt2 [7 -5])

(defn test-neighbours [pt amount result]
  (is (= (aoc/neighbours pt amount) result)))

(deftest points
  (is (= (aoc/manhattan pt2) 12))
  (is (= (aoc/manhattan pt1 pt2) 13))
  (is (= (aoc/pt+ pt1 pt2) [9 -2]))

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

(deftest vec->set
  (is (= (aoc/grid->points grid #(= % \#)) walls)))



(def evens [2 4 6 8 -24 156])
(def stevens [2 4 6 21 32])

(deftest helpers
  (testing "none?"
    (is (aoc/none? odd? evens))
    (is (not (aoc/none? odd? stevens))))
  (testing "count-if"
    (is (= (aoc/count-if even? evens) 6))
    (is (= (aoc/count-if odd? evens) 0))
    (is (= (aoc/count-if even? stevens) 4))
    (is (= (aoc/count-if odd? stevens) 1)))
  (testing "find-first"
    (is (= (aoc/find-first even? evens) 2))
    (is (nil? (aoc/find-first odd? evens)))
    (is (= (aoc/find-first odd? stevens) 21))))


(deftest gcd-lcm
  (testing "gcd"
    (is (= (aoc/gcd 2 3) 1))
    (is (= (aoc/gcd 4 12) 4))
    (is (= (aoc/gcd 25 15) 5))
    (is (= (aoc/gcd 7 17) 1)))
  (testing "lcm"
    (is (= (aoc/lcm 2 3) 6))
    (is (= (aoc/lcm 4 12) 12))
    (is (= (aoc/lcm 25 15) 75))
    (is (= (aoc/lcm 7 17) (* 7 17)))))




(let [summary (run-tests)]
  (when-not (successful? summary)
    (throw (Exception. "some tests failed"))))
