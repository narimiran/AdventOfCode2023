(ns day22
  (:require aoc))


(defn third [[_ _ z]] z)

(def initial-heights
  (into {}
        (for [x (range 10)
              y (range 10)]
          [[x y] {:height 0
                  :brick-id nil}])))

(def initial-state
  {:heights initial-heights
   :supports {}
   :non-removables {}
   :on-top-of {}})


(defn part-1 [blocks]
  (reduce
   (fn [{:keys [heights supports non-removables on-top-of]}
        [id [x1 y1 z1 x2 y2 z2]]]
     (let [positions     (for [x (range x1 (inc x2))
                               y (range y1 (inc y2))]
                           [x y])
           heights-below (select-keys heights positions)
           max-height    (apply max (map :height (vals heights-below)))
           curr-supports (->> (vals heights-below)
                              (filter (fn [{:keys [height]}]
                                        (= height max-height)))
                              (map :brick-id)
                              (remove nil?)
                              set)
           new-heights   (update-vals heights-below
                                      (fn [_] {:height (+ (inc max-height) (- z2 z1))
                                               :brick-id id}))]
       {:heights        (merge heights new-heights)
        :supports       (reduce (fn [acc support]
                                  (update acc support conj id))
                                supports
                                curr-supports)
        :on-top-of      (update on-top-of id into curr-supports)
        :non-removables (if (= 1 (count curr-supports))
                          (update non-removables (first curr-supports) conj id)
                          non-removables)}))
   initial-state
   (map-indexed vector blocks)))


(defn traverse [supports on-top-of nr]
  (loop [stack (list nr)
         visited #{}]
    (if-not (seq stack)
      (dec (count visited))
      (let [current (peek stack)
            stack' (pop stack)]
        (if (visited current)
          (recur stack' visited)
          (let [candidates (supports current)
                vis' (conj visited current)
                goods (filter (fn [x] (every? vis' (on-top-of x))) candidates)]
            (recur (reduce conj stack' goods) vis')))))))


(defn solve [input-file]
  (let [blocks (->> (aoc/parse-input input-file :ints)
                    (sort-by third))
        {:keys [non-removables supports on-top-of]} (part-1 blocks)]
    [(- (count blocks) (count non-removables))
     (aoc/sum-map #(traverse supports on-top-of %) (keys non-removables))]))


(solve (aoc/read-file 22))
