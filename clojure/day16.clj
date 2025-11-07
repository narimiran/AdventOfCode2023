(ns day16
  (:require [aoc-utils.core :as aoc]))


(defn conj' [col [a b :as xs]]
  ;; to make the main function a bit nicer
  (if (int? a)
    (conj col xs)
    (conj col a b)))

(defn my-hash [x y dx dy]
  ;; 2x faster than the built-in `hash`
  (+ (* 12345 x)
     (* 1234 y)
     (* 123 dx)
     dy))

(def ^:dynamic seen-starts)

(defn traverse [contraption x y dx dy]
  (if (@seen-starts [x y]) 0
      (let [size (count contraption)]
        (loop [seen      (transient #{})
               energized (transient #{})
               stack     (list [x y dx dy])]
          (if-let [[x y dx dy] (peek stack)]
            (let [h (my-hash x y dx dy)]
              (cond
                (not (< -1 x size))
                (do
                  (swap! seen-starts conj [(- x dx) y])
                  (recur seen energized (pop stack)))

                (not (< -1 y size))
                (do
                  (swap! seen-starts conj [x (- y dy)])
                  (recur seen energized (pop stack)))

                (seen h)
                (recur seen energized (pop stack))

                :else
                (recur (conj! seen h)
                       (conj! energized (+ x (* size y)))
                       (conj' (pop stack)
                              (case ((contraption y) x)
                                \. [(+ x dx) (+ y dy) dx dy]
                                \/ [(- x dy) (- y dx) (- dy) (- dx)]
                                \\ [(+ x dy) (+ y dx) dy dx]
                                \| (if (zero? dx)
                                     [x (+ y dy) 0 dy]
                                     [[x (dec y) 0 -1] [x (inc y) 0 1]])
                                \- (if (zero? dy)
                                     [(+ x dx) y dx 0]
                                     [[(dec x) y -1 0] [(inc x) y 1 0]]))))))
            (count energized))))))


(defn max-energy [contraption]
  (let [size (count contraption)
        l (dec size)]
    (->> (pmap (juxt #(traverse contraption % 0  0  1)
                     #(traverse contraption % l  0 -1)
                     #(traverse contraption 0 %  1  0)
                     #(traverse contraption l % -1  0))
               (range size))
         flatten
         (reduce max))))


(defn solve [input]
  (let [contraption (aoc/parse-lines input :chars)]
    (binding [seen-starts (atom #{})]
      [(traverse contraption 0 0 1 0)
       (max-energy contraption)])))


(solve (aoc/read-input 16))
