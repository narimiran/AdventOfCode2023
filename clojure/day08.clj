(ns day08
  (:require aoc
            [clojure.string :as str]))


(defn parse-network [network]
  (reduce
   (fn [acc line]
     (let [[start left right] (map keyword (re-seq #"\w+" line))]
       (assoc acc start {:L left :R right})))
   {}
   network))

(defn steps [instrs network start]
  (loop [n 0
         [hd & tl] (cycle instrs)
         node start]
    (if (str/ends-with? node "Z")
      n
      (recur (inc n) tl ((network node) hd)))))

(defn ghost-steps [instrs network starts]
  (transduce
   (map (partial steps instrs network))
   aoc/lcm
   starts))


(defn solve [input]
  (let [[[instrs'] network'] (aoc/parse-input-paragraphs input)
        network (parse-network network')
        instrs (mapv (comp keyword str) instrs')
        starts (filter #(str/ends-with? % "A") (keys network))]
    [(steps instrs network :AAA)
     (ghost-steps instrs network starts)]))


(solve (aoc/read-file 8))
