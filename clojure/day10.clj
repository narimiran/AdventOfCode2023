(ns day10
  (:require aoc))


(defn find-start [sketch]
  (key (first (aoc/grid->points sketch #{\S}))))


(defn traverse [sketch start]
  (loop [[x y :as curr] (aoc/pt+ start [0 1])
         [px py] start
         seen (transient #{})
         verticals (transient {})]
    (let [seen' (conj! seen curr)
          dx (- x px)
          dy (- y py)
          vy (verticals y)]
      (case ((sketch y) x)
        \S {:pipes seen'
            :verts (persistent! verticals)}
        \J (recur (if (zero? dy) [x (dec y)] [(dec x) y])
                  curr seen' (assoc! verticals y (conj vy x)))
        \L (recur (if (zero? dy) [x (dec y)] [(inc x) y])
                  curr seen' (assoc! verticals y (conj vy x)))
        \7 (recur (if (zero? dy) [x (inc y)] [(dec x) y])
                  curr seen' verticals)
        \F (recur (if (zero? dy) [x (inc y)] [(inc x) y])
                  curr seen' verticals)
        \| (recur [(+ x dx) (+ y dy)]
                  curr seen' (assoc! verticals y (conj vy x)))
        \- (recur [(+ x dx) (+ y dy)]
                  curr seen' verticals)))))

(defn enclosed [seen verticals h w]
  (for [y (range h)
        :let [row-verts (verticals y)]
        :when row-verts
        :let [min-vert (reduce min row-verts)
              max-vert (reduce max row-verts)]
        x (range w)
        :when (and (< min-vert x max-vert)
                   (not (seen [x y]))
                   (odd? (aoc/count-if #(< % x) row-verts)))]
    1))

(defn solve [input]
  (let [sketch (aoc/parse-input input :chars)
        start (find-start sketch)
        h (count sketch)
        w (count (first sketch))
        {:keys [pipes verts]} (traverse sketch start)]
    [(/ (count pipes) 2)
     (count (enclosed pipes verts h w))]))


(solve (aoc/read-file 10))
