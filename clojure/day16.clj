(ns day16
  (:require aoc))


(defn conj' [col [a b :as xs]]
  ; to make the main function a bit nicer
  (if (int? a)
    (conj col xs)
    (conj (conj col a) b)))


(defn traverse [contraption x y dx dy]
  (let [size (count contraption)]
    (loop [seen      (transient #{})
           energized (transient #{})
           stack     [[x y dx dy]]]
      (if-let [[x y dx dy :as current] (peek stack)]
        (let [h (hash current)]
          (if (or (not (< -1 x size))
                  (not (< -1 y size))
                  (seen h))
            (recur seen energized (pop stack))
            (recur
             (conj! seen h)
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
        (count energized)))))


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
  (let [contraption (aoc/parse-input input :chars)]
    [(traverse contraption 0 0 1 0)
     (max-energy contraption)]))


(solve (aoc/read-file 16))
