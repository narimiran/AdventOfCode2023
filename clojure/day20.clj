(ns day20
  (:require [aoc-utils.core :as aoc]
            [clojure.string :as str]
            [better-cond.core :as b]))


(defn extract-name [src]
  (keyword (str/replace src #"%|&" "")))

(defn parse-line [line]
  (let [[src dest]   (str/split line #" -> ")
        destinations (mapv keyword (str/split dest #", "))
        amt          (count destinations)]
    (case (first src)
      \b [:broadcaster {:typ  :b
                        :dest destinations
                        :amt  (inc amt)}]
      \% [(extract-name src) {:typ   :%
                              :dest  destinations
                              :amt   amt
                              :pulse 0}]
      \& [(extract-name src) {:typ    :&
                              :dest   destinations
                              :amt    amt
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
      #_{:clj-kondo/ignore [:unresolved-symbol]}
      (b/cond
        :let [queue' (pop queue)
              {:keys [typ dest amt pulse pulses]} ((:modules state) curr)]

        (or (nil? typ)
            (and (= typ :%) (= v 1)))
        (recur queue' state)

        (= curr :broadcaster)
        (recur (reduce (fn [q d] (conj q [curr d v])) queue' dest)
               (update state :low-cnt + amt))

        :let [pulses' (assoc pulses from v)
              pulse' (case typ
                       :% (- 1 pulse)
                       :& (if (not-any? zero? (vals pulses')) 0 1))
              zp? (zero? pulse')]

        (recur (reduce (fn [q d] (conj q [curr d pulse'])) queue' dest)
               (cond-> state
                 (= typ :%) (aoc/assoc-3 :modules curr :pulse pulse')
                 (= typ :&) (aoc/assoc-3 :modules curr :pulses pulses')
                 zp?        (update :low-cnt  + amt)
                 (not zp?)  (update :high-cnt + amt)
                 (and (not zp?)
                      (#{:ks :jf :qs :zk} curr)) ; manually found these keys
                 (aoc/assoc-2 :periods curr n))))
      state)))


(defn push-button [times modules]
  (reduce
   (fn [state n]
     (if (= 4 (count (:periods state)))
       (reduced state)
       (traverse state (inc n))))
   {:modules  modules
    :low-cnt  0
    :high-cnt 0
    :periods  {}}
   (range times)))


(defn part-1 [modules]
  (let [m (push-button 1000 modules)]
    (* (:low-cnt m) (:high-cnt m))))

(defn part-2 [modules]
  (let [m (push-button 999999 modules)]
    (reduce aoc/lcm (vals (:periods m)))))


(defn solve [input]
  (->> (aoc/parse-lines input parse-line)
       (into {})
       init-pulses
       ((juxt part-1 part-2))))


(solve (aoc/read-input 20))
