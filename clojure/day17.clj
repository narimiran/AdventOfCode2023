(ns day17
  (:require aoc
            [clojure.data.priority-map :refer [priority-map]]))


(defn heat-loss [city x y dx dy n]
  (reduce
   (fn [acc m]
     (+ acc ((city (+ y (* m dy))) (+ x (* m dx)))))
   (range (inc n))))


(defn traverse [city min-straight max-straight]
  (let [size (count city)
        end (dec size)
        valid? (fn [x y] (and (< -1 x size)
                              (< -1 y size)))
        queue (priority-map [0 0 1 0] 0
                            [0 0 0 1] 0)]
    (loop [seen #{}
           queue queue]
      (let [[[x y dx dy :as state] heat] (peek queue)
            queue' (pop queue)]
        (if (and (= x end)
                 (= y end))
          heat
          (if (seen state)
            (recur seen queue')
            (recur
             (conj seen state)
             (reduce
              (fn [q [dx' dy']]
                (reduce
                 (fn [q n]
                   (let [nx (+ x (* n dx'))
                         ny (+ y (* n dy'))]
                     (if-not (valid? nx ny)
                       (reduced q)
                       (let [heat' (+ heat (heat-loss city x y dx' dy' n))
                             state' [nx ny dx' dy']]
                         (assoc q state' (min heat' (q state' Integer/MAX_VALUE)))))))
                 q
                 (range min-straight (inc max-straight))))
              queue'
              [[(- dy) dx]
               [dy (- dx)]]))))))))


(defn solve [input-file]
  (let [city (aoc/read-input input-file :digits)]
    [(traverse city 1 3)
     (traverse city 4 10)]))


(solve 17)

