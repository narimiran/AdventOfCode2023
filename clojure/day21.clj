(ns day21
  (:require aoc))


(def p2-steps 26501365)
(def sq-size 131)
(def p2-squares (quot p2-steps sq-size))


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
                (reduce (fn [c pt]
                          (into c (for [[x y] (aoc/neighbours 4 pt)
                                        :let [nb [(mod x sq-size) (mod y sq-size)]]
                                        :when (and (not (walls nb))
                                                   (if even-step?
                                                     (not (@seen-odd [x y]))
                                                     (not (@seen-even [x y]))))]
                                    (do (if even-step?
                                          (swap! seen-odd conj [x y])
                                          (swap! seen-even conj [x y]))
                                        [x y]))))
                        #{}
                        current)))
            [start]
            (range (+ 1 65 (* 2 sq-size))))
    [@p1 (calc-total @p2)]))


(defn solve [input]
  (let [input (aoc/parse-input input :chars)
        size (count input)
        start [(quot size 2) (quot size 2)]
        walls (set (keys (aoc/grid->points input #{\#})))]
    (traverse walls start)))


(solve (aoc/read-file 21))
