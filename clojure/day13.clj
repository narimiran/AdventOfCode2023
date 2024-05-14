(ns day13
  (:require aoc))


(defn differences [a b]
  (aoc/count-if false? (map = a b)))


(defn mirror? [part pattern nrettap line]
  (let [before (take-last line nrettap)
        after  (drop line pattern)
        diffs  (map differences before after)]
    (case part
      1 (every? zero? diffs)
      2 (= 1 (reduce + diffs)))))


(defn mirror-line [part pattern]
  (aoc/find-first
   #(mirror? part pattern (rseq pattern) %)
   (range 1 (count pattern))))


(defn find-mirror [part pattern]
  (if-let [horizontal-mirror (mirror-line part pattern)]
    (* 100 horizontal-mirror)
    (mirror-line part (aoc/transpose pattern))))


(defn notes-sum [patterns part]
  (reduce + (pmap #(find-mirror part %) patterns)))


(defn solve [input]
  (let [patterns (aoc/parse-input-paragraphs input)]
    [(notes-sum patterns 1)
     (notes-sum patterns 2)]))


(solve (aoc/read-file 13))
