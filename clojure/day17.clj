(ns day17
  (:require aoc
            [clojure.data.priority-map :refer [priority-map]]))


(defn heat-loss [city x y dx dy n]
  (reduce
   (fn [acc m]
     (+ acc ((city (+ y (* m dy))) (+ x (* m dx)))))
   (range (inc n))))

(defn my-hash ^long [^long x ^long y ^long dx ^long dy]
  (+ (* 12345 x)
     (* 1234 y)
     (* 123 dx)
     dy))

(defn traverse [city min-straight max-straight]
  (let [size     (count city)
        end      (dec size)
        straight (range min-straight (inc max-straight))
        queue    (priority-map [0 0 1 0] 0
                               [0 0 0 1] 0)]
    (loop [seen  (transient #{})
           queue queue]
      (let [[[x y dx dy] heat] (peek queue)
            queue'             (pop queue)]
        (if (= end x y)
          heat
          (recur (conj! seen (my-hash x y dx dy))
                 (reduce
                  (fn [q [dx' dy']]
                    (reduce (fn [q n]
                              (let [nx (+ x (* n dx'))
                                    ny (+ y (* n dy'))]
                                (cond
                                  (not (aoc/inside? size nx ny)) (reduced q)
                                  (seen (my-hash nx ny dx' dy')) q
                                  :else (let [heat'  (+ heat (heat-loss city x y dx' dy' n))
                                              state' [nx ny dx' dy']]
                                          (assoc q state' (min heat' (q state' heat')))))))
                            q
                            straight))
                  queue'
                  [[(- dy) dx]
                   [dy (- dx)]])))))))


(defn solve [input]
  (let [city (aoc/parse-input input :digits)
        p1 (future (traverse city 1 3))
        p2 (future (traverse city 4 10))]
    [@p1 @p2]))


(solve (aoc/read-file 17))
