(ns day19
  (:require [aoc-utils.core :as aoc]
            [clojure.string :as str]))


(defn parse-rule [rule]
  (let [[cnd dest] (str/split rule #":")]
    (if (nil? dest)
      [(keyword cnd)]
      (let [[c op & v] cnd
            k (keyword (str c))
            f ({\> > \< <} op)
            v (parse-long (str/join v))]
        [(keyword dest) f k v]))))

(defn parse-workflow [workflow]
  (let [[_ k rules] (re-find #"(\w+)\{(.+)\}" workflow)
        k (keyword k)
        rules (map parse-rule (str/split rules #","))]
    [k rules]))

(defn parse-rating [rating]
  (let [[x m a s] (aoc/integers rating)]
    {:x x , :m m , :a a , :s s}))


(defn accepted [workflows rating]
  (loop [wf :in]
    (case wf
      :R 0
      :A (reduce + (vals rating))
      (recur (loop [[[dest f k v] & tl] (workflows wf)]
               (cond
                 (nil? f) dest
                 (f (rating k) v) dest
                 :else (recur tl)))))))



(defn split-out [workflow rating]
  (loop [[[dest f k v] & tl] workflow
         out []
         rating rating]
    (cond
      (nil? dest) out
      (nil? f) (conj out [dest rating])
      (= f <) (let [[a b] (rating k)]
                (recur tl
                       (conj out [dest (assoc rating k [a (dec v)])])
                       (assoc rating k [v b])))
      (= f >) (let [[a b] (rating k)]
                (recur tl
                       (conj out [dest (assoc rating k [(inc v) b])])
                       (assoc rating k [a v]))))))

(defn accepted-combinations [workflows rating]
  (loop [stack (list [:in rating])
         score 0]
    (if-let [[wf rating] (peek stack)]
      (let [stack' (pop stack)]
        (case wf
          :R (recur stack' score)
          :A (recur stack' (+ score (reduce * (map (fn [[a b]] (- (inc b) a))
                                                   (vals rating)))))
          (recur (reduce conj stack' (split-out (workflows wf) rating)) score)))
      score)))



(defn solve [input]
  (let [[wrkfls rtngs] (aoc/parse-paragraphs input)
        workflows (into {} (map parse-workflow wrkfls))
        ratings (map parse-rating rtngs)
        rating-2 (zipmap [:x :m :a :s] (repeat [1 4000]))]
    [(aoc/sum-map #(accepted workflows %) ratings)
     (accepted-combinations workflows rating-2)]))


(solve (aoc/read-input 19))
