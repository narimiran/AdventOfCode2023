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
        queue (priority-map [0 0 1 0] 0
                            [0 0 0 1] 0)]
    (loop [seen #{}
           queue queue]
      (let [[[x y dx dy :as state] heat] (peek queue)
            queue' (pop queue)]
        (if (= end x y)
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
                     (if-not (aoc/inside? size nx ny)
                       (reduced q)
                       (let [heat' (+ heat (heat-loss city x y dx' dy' n))
                             state' [nx ny dx' dy']]
                         (assoc q state' (min heat' (q state' Integer/MAX_VALUE)))))))
                 q
                 (range min-straight (inc max-straight))))
              queue'
              [[(- dy) dx]
               [dy (- dx)]]))))))))


(defn solve [input]
  (let [city (aoc/parse-input input :digits)]
    [(traverse city 1 3)
     (traverse city 4 10)]))


(solve (aoc/read-file 17))
