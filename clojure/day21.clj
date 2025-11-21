(ns day21
  (:require [aoc-utils.core :as aoc]))


(def p2-steps 26501365)
(def sq-size 131)
(def p2-squares (quot p2-steps sq-size))

(defn normalize [[x y]]
  (+ (* 1000 (mod y sq-size))
     (mod x sq-size)))


(defn calc-total [[a b c]]
  (+ a
     (* p2-squares
        (+ (- b a)
           (* (dec p2-squares)
              (quot (- (+ c a) b b) 2))))))

(defn traverse [walls start]
  (let [p1 (atom 0)
        p2 (atom [])
        seen-even (atom #{})
        seen-odd (atom #{})]
    (reduce (fn [current step]
              (let [even-step? (zero? (mod step 2))]
                (when (= step 64)
                  (reset! p1 (count @seen-even)))
                (when (= 65 (mod step sq-size))
                  (if even-step?
                    (swap! p2 conj (count @seen-even))
                    (swap! p2 conj (count @seen-odd))))
                (persistent!
                 (reduce
                  (fn [c nbs]
                    (reduce (fn [c nb]
                              (if even-step?
                                (swap! seen-odd conj nb)
                                (swap! seen-even conj nb))
                              (conj! c nb))
                            c
                            nbs))
                  (transient #{})
                  (map (fn [pt] (aoc/neighbours-4
                                 pt
                                 (fn [nb]
                                   (and (not (walls (normalize nb)))
                                        (if even-step?
                                          (not (@seen-odd nb))
                                          (not (@seen-even nb)))))))
                       current)))))
            [start]
            (range (+ 1 65 (* 2 sq-size))))
    [@p1 (calc-total @p2)]))


(defn solve [input]
  (let [input (aoc/parse-lines input :chars)
        size (count input)
        start [(quot size 2) (quot size 2)]
        walls (:walls (aoc/create-hashed-grid input {\# :walls}))]
    (traverse walls start)))


(solve (aoc/read-input 21))
