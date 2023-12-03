(ns day03
  (:require aoc
            [clojure.set :as set]))


(def digits (set (apply str (range 10))))

(defn symbol-coords [grid]
  (set (keys (remove #(digits (val %)) grid))))

(defn adjacent-symbols [pt symbs]
  (set/intersection symbs (set (aoc/neighbours pt 8))))

(defn gear-ratios [gear-values]
  (keep
   #(when (= (count %) 2)
      (reduce * %))
   gear-values))


(defn find-solution [lines symbs gears]
  (let [h (count lines)
        w (count (first lines))
        zero-state {:pt [0 0]
                    :curr ""
                    :near-symbol? false
                    :numbers []
                    :current-gears #{}
                    :gear-values {}}]
    (loop [{:keys [curr near-symbol? numbers current-gears gear-values]
            [x y :as pt] :pt
            :as state} zero-state]
      (cond
        (= y h)
        {:nums numbers
         :gear-ratios (gear-ratios (vals gear-values))}

        (or (= x w)
            (= \. ((lines y) x))
            (symbs pt))
        (if (and near-symbol? (seq curr))
          (let [number (parse-long curr)]
            (recur (assoc zero-state
                          :pt (if (= x w) [0 (inc y)] [(inc x) y])
                          :numbers (conj numbers number)
                          :gear-values (reduce (fn [gvals gear]
                                                 (update gvals gear conj number))
                                               gear-values
                                               current-gears))))
          (recur (assoc zero-state
                        :pt (if (= x w) [0 (inc y)] [(inc x) y])
                        :numbers numbers
                        :gear-values gear-values)))

        :else
        (recur (assoc state
                      :pt [(inc x) y]
                      :curr (str curr ((lines y) x))
                      :near-symbol? (or near-symbol? (seq (adjacent-symbols pt symbs)))
                      :current-gears (set/union current-gears (adjacent-symbols pt gears))))))))


(defn solve [input-file]
  (let [lines    (aoc/read-input input-file :chars)
        points   (aoc/grid->points lines #(not= % \.))
        symbs    (symbol-coords points)
        gears    (set (filter #(= \* (points %)) symbs))
        solution (find-solution lines symbs gears)]
    [(reduce + (:nums solution))
     (reduce + (:gear-ratios solution))]))


(solve 3)
