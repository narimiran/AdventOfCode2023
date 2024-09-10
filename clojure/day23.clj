(ns day23
  (:require aoc
            [better-cond.core :as b]
            [clojure.data.int-map :as i]))


(defn part-1 [trails]
  (let [size (count trails)
        end  [(dec (dec size)) (dec size)]]
    (loop [stack   (list [[1 1] #{[1 0]}])
           longest 0]
      #_{:clj-kondo/ignore [:unresolved-symbol]}
      (b/cond
        (not (seq stack)) longest

        :let [[[x y :as pt] seen] (peek stack)
              stack' (pop stack)]
        (= pt end) (recur stack' (max longest (count seen)))

        :let [curr  ((trails y) x)
              seen' (conj seen pt)
              nbs   (for [[nx ny] (aoc/neighbours 4 pt)
                          :when   (and (not (seen [nx ny]))
                                       (not (#{\#} ((trails ny) nx))))]
                      [[nx ny] seen'])]
        (= curr \.) (recur (into stack' nbs) longest)

        :let [pt' (if (= curr \>) [(inc x) y] [x (inc y)])]
        (seen pt') (recur stack' longest)

        (recur (conj stack' [pt' seen']) longest)))))



(defn int-hash [x y]
  (+ (* 150 x) y))

(defn compress-graph [trails]
  (let [adjacencies (atom (i/int-map))
        size        (count trails)]
    (doseq [[y row] (map-indexed vector trails)
            [x c]   (map-indexed vector row)
            :when   (not= \# c)
            [nx ny] (aoc/neighbours 4 [x y])
            :when   (and (aoc/inside? size nx ny)
                         (not= \# ((trails ny) nx)))]
      (swap! adjacencies update (int-hash x y) assoc (int-hash nx ny) 1))
    (doseq [pt    (keys @adjacencies)
            :let  [nbs (@adjacencies pt)]
            :when (= 2 (count nbs))
            :let  [[a b] (keys nbs)
                   dist (+ (nbs a) (nbs b))]]
      (swap! adjacencies dissoc pt)
      (swap! adjacencies update a assoc b dist)
      (swap! adjacencies update a dissoc pt)
      (swap! adjacencies update b assoc a dist)
      (swap! adjacencies update b dissoc pt))
    @adjacencies))


(defn part-2 [adjacencies]
  (let [edges (sort (keys adjacencies))
        start (first edges)
        ;; only one way to end - cuts time in half:
        [exit dist-to-end] (first (adjacencies (last edges)))]
    (loop [stack (list [start (i/int-set) 0])
           longest 0]
      #_{:clj-kondo/ignore [:unresolved-symbol]}
      (b/cond
        (not (seq stack)) (+ longest dist-to-end)

        :let [[pt seen score] (peek stack)
              stack' (pop stack)
              seen' (conj seen pt)]
        (= pt exit) (recur stack' (max longest score))

        (recur (into stack'
                     (for [[nb cost] (adjacencies pt)
                           :when (not (seen nb))
                           :when (if (= 3 (count (adjacencies pt)))
                                   ;; if on a perimeter, go only right and down
                                   ;; makes everything 3x faster
                                   (or (> nb pt)
                                       (> (mod nb 150) (mod pt 150)))
                                   true)]
                       [nb seen' (+ score cost)]))
               longest)))))


(defn solve [input-file]
  (let [trails (aoc/parse-input input-file :chars)
        adjacencies (compress-graph trails)]
    [(part-1 trails)
     (part-2 adjacencies)]))


(solve (aoc/read-file 23))
