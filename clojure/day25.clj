(ns day25
  (:require aoc))


(defn build-graph [components]
  (let [adjacencies (atom {})]
    (doseq [[k & vs] components
            v vs]
      (swap! adjacencies update k conj v)
      (swap! adjacencies update v conj k))
    @adjacencies))


(defn add-edges [edge-counts vs]
  (reduce (fn [edge-counts [a b]]
            (update edge-counts (sort [a b]) (fnil inc 0)))
          edge-counts
          (partition 2 1 vs)))

(defn traverse [graph start end edge-counts]
  (loop [queue (conj aoc/empty-queue [start []])
         seen  (transient #{})]
    (if-let [[curr prevs] (peek queue)]
      (cond
        (= curr end) (add-edges edge-counts (conj prevs curr))
        (seen curr)  (recur (pop queue) seen)
        :else        (recur (reduce (fn [q pt] (conj q [pt (conj prevs curr)]))
                                    (pop queue)
                                    (graph curr))
                            (conj! seen curr)))
      (count seen))))

(defn find-most-frequent-edges [graph]
  (reduce (fn [edge-counts [start end]]
            (traverse graph start end edge-counts))
          {}
          (repeatedly 200 #(shuffle (keys graph)))))

(defn remove-most-frequent [edge-counts graph]
  (let [most-frequent (keys (take 3 (sort-by val > edge-counts)))]
    (reduce (fn [graph [a b]]
              (-> graph
                  (update a (fn [nbs] (remove #{b} nbs)))
                  (update b (fn [nbs] (remove #{a} nbs)))))
            graph
            most-frequent)))

(defn find-groups [graph]
  (let [edge-counts (find-most-frequent-edges graph)
        graph'      (remove-most-frequent edge-counts graph)
        total-nodes (count (keys graph))
        island      (traverse graph' (key (first graph)) nil nil)]
    (* (- total-nodes island) island)))


(defn solve [input-file]
  (let [components (aoc/parse-input input-file :words {:word-sep #": | "})
        graph      (build-graph components)]
    (find-groups graph)))


(solve (aoc/read-file 25))
