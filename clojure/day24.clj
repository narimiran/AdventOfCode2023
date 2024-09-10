(ns day24
  (:require aoc
            [clojure.set :as set]))


(def lo 200000000000000)
(def hi 400000000000000)

(defn line-equation [[x y _ dx dy _]]
  [(/ dy dx)
   (- y (* (/ dy dx) x))])

(defn intersection [l1 l2]
  (let [[a c] (line-equation l1)
        [b d] (line-equation l2)]
    (when (not= a b)
      (let [x (/ (- d c) (- a b))
            y (+ (* a x) c)]
        [x y]))))

(defn <> [v]
  (if (neg? v) < >))

(defn part-1 [lines]
  (count
   (for [[i [ax _ _ adx :as l1]] (map-indexed vector lines)
         [j [bx _ _ bdx :as l2]] (map-indexed vector lines)
         :while (not= i j)
         :let [[x y] (intersection l1 l2)]
         :when (and (some? x)
                    (<= lo x hi)
                    (<= lo y hi)
                    ((<> adx) x ax)
                    ((<> bdx) x bx))]
     1)))



(defn set-range [from to]
  (into #{} (for [x (range from (inc to))] x)))

(def potential-dxs (atom (set-range -1000 1000)))
(def potential-dys (atom (set-range -1000 1000)))
(def potential-dzs (atom (set-range -1000 1000)))


(defn find-candidates [velocity pos-diff]
  (loop [candidates #{}
         v -1000]
    (cond
      (= v 1000) candidates
      (= v velocity) (recur candidates (inc v))
      (zero? (mod pos-diff (- v velocity))) (recur (conj candidates v) (inc v))
      :else (recur candidates (inc v)))))

(defn update-potentials! [velocity pos-diff potentials]
  (let [candidates (find-candidates velocity pos-diff)]
    (when (seq candidates)
      (swap! potentials set/intersection candidates))))

(defn calc-position [[ax ay az adx ady adz]
                     [bx by _  bdx bdy _]]
  ;; https://old.reddit.com/r/adventofcode/comments/18pnycy/2023_day_24_solutions/keqf8uq/
  (let [rock-dx (first @potential-dxs)
        rock-dy (first @potential-dys)
        rock-dz (first @potential-dzs)
        ka (/ (- ady rock-dy)
              (- adx rock-dx))
        kb (/ (- bdy rock-dy)
              (- bdx rock-dx))
        ca (- ay (* ka ax))
        cb (- by (* kb bx))
        x (/ (- cb ca)
             (- ka kb))
        y (+ (* ka x) ca)
        t (/ (- x ax) (- adx rock-dx))
        z (+ az (* t (- adz rock-dz)))]
    (long (+ x y z))))


(defn part-2 [lines]
  (doseq [[i [ax ay az adx ady adz]] (map-indexed vector lines)
          [j [bx by bz bdx bdy bdz]] (map-indexed vector lines)
          :while (and (not= i j)
                      (not= 1
                            (count @potential-dxs)
                            (count @potential-dys)
                            (count @potential-dzs)))]
    (when (= adx bdx)
      (update-potentials! adx (- bx ax) potential-dxs))
    (when (= ady bdy)
      (update-potentials! ady (- by ay) potential-dys))
    (when (= adz bdz)
      (update-potentials! adz (- bz az) potential-dzs)))
  (calc-position (first lines) (second lines)))


(defn solve [input-file]
  (let [input (aoc/parse-input input-file :ints)]
    ((juxt part-1 part-2) input)))


(solve (aoc/read-file 24))
