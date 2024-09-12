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
              seen' (conj seen pt)]
        (= curr \.) (let [nbs (aoc/neighbours
                               4 pt
                               (fn [[nx ny]]
                                 (and (not (seen [nx ny]))
                                      (not (#{\#} ((trails ny) nx))))))]
                      (recur (reduce (fn [s nb] (conj s [nb seen']))
                                     stack'
                                     nbs)
                             longest))

        :let [pt' (if (= curr \>) [(inc x) y] [x (inc y)])]
        (seen pt') (recur stack' longest)

        (recur (conj stack' [pt' seen']) longest)))))


(defn compress-graph [trails]
  (let [adjacencies (atom (i/int-map))
        size        (count trails)
        int-hash    (fn [x y] (+ (* 150 x) y))]
    (doseq [[y row] (map-indexed vector trails)
            [x c]   (map-indexed vector row)
            :when   (not= \# c)
            [nx ny] (aoc/neighbours 4 [x y]
                                    (fn [[nx ny]]
                                      (and (aoc/inside? size nx ny)
                                           (not= \# ((trails ny) nx)))))]
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

        (recur (reduce (fn [s [nb cost]]
                         (if (and (not (seen nb))
                                  (if (= 3 (count (adjacencies pt)))
                                    ;; if on a perimeter, go only right and down
                                    ;; makes everything 3x faster
                                    (or (> nb pt)
                                        (> (mod nb 150) (mod pt 150)))
                                    true))
                           (conj s [nb seen' (+ score cost)])
                           s))
                       stack'
                       (adjacencies pt))
               longest)))))


(defn solve [input-file]
  (let [trails (aoc/parse-input input-file :chars)
        adjacencies (compress-graph trails)
        p1 (future (part-1 trails))
        p2 (future (part-2 adjacencies))]
    [@p1 @p2]))


(solve (aoc/read-file 23))
