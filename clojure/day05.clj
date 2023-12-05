(ns day05
  (:require aoc))


(defn parse-line [line]
  (let [[dest src rng] (aoc/integers line)]
    {:lo   src
     :hi   (+ src (dec rng))
     :diff (- dest src)}))

(defn parse-maps [maps]
  (->> (rest maps)
       (map parse-line)
       (sort-by :lo)))

(defn seed-ranges [seeds]
  (->> (for [[st rng] (partition 2 seeds)]
         {:start st
          :stop (+ st (dec rng))})
       (sort-by :start)))


(defn convert-number [rules src]
  (reduce
   (fn [src {:keys [lo hi diff]}]
     (if (<= lo src hi)
       (reduced (+ src diff))
       src))
   src
   rules))

(defn convert-1 [srcs rules]
  (map #(convert-number rules %) srcs))


(defn convert-2 [srcs rules]
  (loop [[{:keys [start stop]} & rem-srcs :as srcs] srcs
         [{:keys [lo hi diff]} & rem-rules :as rules] rules
         result []]
    (if (or (empty? srcs) (empty? rules))
      (sort-by :start (into result srcs))
      (cond
        (> start hi)          (recur srcs rem-rules result)
        (< stop lo)           (recur rem-srcs
                                     rules
                                     (conj result {:start start
                                                   :stop  stop}))
        (<= lo start stop hi) (recur rem-srcs
                                     rules
                                     (conj result {:start (+ diff start)
                                                   :stop  (+ diff stop)}))
        (<= lo start hi stop) (recur (conj rem-srcs {:start (inc hi)
                                                     :stop  stop})
                                     rem-rules
                                     (conj result {:start (+ diff start)
                                                   :stop  (+ diff hi)}))
        (<= start lo stop hi) (recur rem-srcs
                                     rules
                                     (-> result
                                         (conj {:start start
                                                :stop  (dec lo)})
                                         (conj {:start (+ diff lo)
                                                :stop  (+ diff stop)})))))))


(defn part-1 [seeds maps]
  (->> (reduce convert-1 seeds maps)
       (reduce min)))

(defn part-2 [seeds maps]
  (->> (reduce convert-2 seeds maps)
       first
       :start))


(defn solve [input-file]
  (let [[[seeds] & maps] (aoc/read-input-paragraphs input-file)
        seeds-1 (aoc/integers seeds)
        seeds-2 (seed-ranges seeds-1)
        rules (map parse-maps maps)]
    [(part-1 seeds-1 rules)
     (part-2 seeds-2 rules)]))


(solve 5)
