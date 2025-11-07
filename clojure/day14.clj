(ns day14
  (:require [aoc-utils.core :as aoc]
            [clojure.string :as str]))


(defn transpose [platform]
  (apply map str platform))

(defn move-line [dir line]
  (->> (str/split line #"#" -1)
       (map (comp str/join
                  (case dir
                    :left  reverse
                    :right identity)
                  sort))
       (str/join "#")))


(defn move-east [platform]
  (vec (pmap #(move-line :right %) platform)))

(defn move-west [platform]
  (vec (pmap #(move-line :left %) platform)))

(defn move-north [platform]
  (-> platform transpose move-west transpose))

(defn move-south [platform]
  (-> platform transpose move-east transpose))

(defn spin-cycle [platform]
  (-> platform move-north move-west move-south move-east))


(defn calc-score [platform]
  (let [platform (vec platform)
        size     (count platform)]
    (aoc/sum-map
     (fn [n] (* (- size n)
                (aoc/count-if #{\O} (platform n))))
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
  (let [platform (aoc/parse-lines input)]
    [(calc-score (move-north platform))
     (calc-score (shake platform))]))


(solve (aoc/read-input 14))
