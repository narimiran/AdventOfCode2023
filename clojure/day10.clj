(ns day10
  (:require aoc))


(defn find-start [sketch]
  (key (first (aoc/grid->points sketch #(= % \S)))))

(defn delta [[x y] [px py]]
  [(- x px) (- y py)])

(defn traverse [sketch start]
  (loop [[x y :as curr] (aoc/pt+ start [0 1])
         prev start
         seen #{}
         verticals {}]
    (let [seen' (conj seen curr)
          [dx dy] (delta curr prev)]
      (case ((sketch y) x)
        \S {:pipes seen'
            :verts verticals}
        \J (recur (if (zero? dy) [x (dec y)] [(dec x) y])
                  curr seen' (update verticals y conj x))
        \L (recur (if (zero? dy) [x (dec y)] [(inc x) y])
                  curr seen' (update verticals y conj x))
        \7 (recur (if (zero? dy) [x (inc y)] [(dec x) y])
                  curr seen' verticals)
        \F (recur (if (zero? dy) [x (inc y)] [(inc x) y])
                  curr seen' verticals)
        \| (recur [(+ x dx) (+ y dy)]
                  curr seen' (update verticals y conj x))
        \- (recur [(+ x dx) (+ y dy)]
                  curr seen' verticals)))))

(defn enclosed [seen verticals h w]
  (for [y (range h)
        :let [row-verts (verticals y)]
        :when row-verts
        :let [min-vert (apply min row-verts)
              max-vert (apply max row-verts)]
        x (range w)
        :when (and (< min-vert x max-vert)
                   (not (seen [x y]))
                   (odd? (aoc/count-if #(< % x) row-verts)))]
    1))

(defn solve [input-file]
  (let [sketch (aoc/read-input input-file :chars)
        start (find-start sketch)
        h (count sketch)
        w (count (first sketch))
        {:keys [pipes verts]} (traverse sketch start)]
    [(/ (count pipes) 2)
     (count (enclosed pipes verts h w))]))


(solve 10)
