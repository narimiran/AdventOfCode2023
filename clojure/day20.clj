(ns day20
  (:require aoc
            [clojure.string :as str]))


(defn extract-name [src]
  (keyword (str/replace src #"%|&" "")))

(defn parse-line [line]
  (let [[src dest] (str/split line #" -> ")
        destinations (mapv keyword (str/split dest #", "))
        amt (count destinations)]
    (case (first src)
      \b [:broadcaster {:typ :b
                        :dest destinations
                        :amt (inc amt)}]
      \% [(extract-name src) {:typ :%
                              :dest destinations
                              :amt amt
                              :pulse 0}]
      \& [(extract-name src) {:typ :&
                              :dest destinations
                              :amt amt
                              :pulses {}}])))

(defn init-pulses [modules]
  (reduce (fn [m k]
            (reduce (fn [m d]
                      (case (:typ (m d))
                        :& (update-in m [d :pulses] conj {k 0})
                        m))
                    m
                    (:dest (m k))))
          modules
          (keys modules)))


(defn traverse [state n]
  (loop [queue (conj aoc/empty-queue [:button :broadcaster 0])
         state state]
    (if-let [[from curr v] (peek queue)]
      (let [queue' (pop queue)
            {:keys [typ dest amt pulse pulses]} ((:modules state) curr)]
        (cond
          (or (nil? typ)
              (and (= typ :%) (= v 1)))
          (recur queue' state)

          (= curr :broadcaster)
          (recur (into queue' (map (fn [d] [curr d v]) dest))
                 (update state :low-cnt + amt))

          :else
          (let [pulses' (assoc pulses from v)
                pulse' (case typ
                         :% (- 1 pulse)
                         :& (if (not-any? zero? (vals pulses')) 0 1))
                zp? (zero? pulse')]
            (recur (into queue' (map (fn [d] [curr d pulse']) dest))
                   (cond-> state
                     (= typ :%) (assoc-in [:modules curr :pulse] pulse')
                     (= typ :&) (assoc-in [:modules curr :pulses] pulses')
                     zp?        (update :low-cnt  + amt)
                     (not zp?)  (update :high-cnt + amt)
                     (and (not zp?)
                          (#{:ks :jf :qs :zk} curr)) ; manually found these keys
                     (assoc-in [:periods curr] n))))))
      state)))


(defn push-button [times modules]
  (reduce
   (fn [state n]
     (if (= (count (:periods state)) 4)
       (reduced state)
       (traverse state (inc n))))
   {:modules modules
    :low-cnt 0
    :high-cnt 0
    :periods {}}
   (range times)))


(defn part-1 [modules]
  (let [m (push-button 1000 modules)]
    (* (:low-cnt m) (:high-cnt m))))

(defn part-2 [modules]
  (let [m (push-button 999999 modules)]
    (reduce aoc/lcm (vals (:periods m)))))


(defn solve [input-file]
  (let [modules (->> (aoc/read-input input-file parse-line)
                     (into {})
                     init-pulses)]
    [(part-1 modules)
     (part-2 modules)]))


(solve 20)
