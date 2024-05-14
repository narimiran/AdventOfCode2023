(ns day14
  (:require aoc
            [clojure.string :as str]))


(defn rotate [platform]
  (->> platform
       aoc/transpose
       (map str/join)))


(defn move-line [dir line]
  (->> (str/split line #"#" -1)
       (map (comp str/join
                  (case dir
                    :left  reverse
                    :right identity)
                  sort))
       (str/join "#")))


(defn move-east [platform]
  (pmap #(move-line :right %) platform))

(defn move-west [platform]
  (pmap #(move-line :left %) platform))

(defn move-north [platform]
  (-> platform rotate move-west rotate))

(defn move-south [platform]
  (-> platform rotate move-east rotate))

(defn spin-cycle [platform]
  (-> platform move-north move-west move-south move-east))


(defn calc-score [platform]
  (let [platform (vec platform)
        size     (count platform)]
    (reduce
     (fn [acc n]
       (+ acc (* (- size n)
                 (aoc/count-if #{\O} (platform n)))))
     0
     (range size))))


(defn remaining-shakes [platform prev curr]
  (let [remain  (- 1000000000 prev)
        to-spin (mod remain (- curr prev))]
    (nth (iterate spin-cycle platform) to-spin)))


(defn shake [platform]
  (reduce
   (fn [[seen platform] n]
     (if-let [prev (seen platform)]
       (reduced (remaining-shakes platform prev n))
       [(assoc seen platform n)
        (spin-cycle platform)]))
   [{} platform]
   (range)))


(defn solve [input]
  (let [platform (aoc/parse-input input)]
    [(calc-score (move-north platform))
     (calc-score (shake platform))]))


(solve (aoc/read-file 14))
