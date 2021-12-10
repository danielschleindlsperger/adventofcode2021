(ns adventofcode2021.day09
  (:require [aocd.core :as data]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-nums [s] (vec (map #(Integer/parseInt %) (str/split s #""))))
(defn parse-input [s]
  (vec (map parse-nums (str/split s #"\n"))))

(defn at-coord [[x y] matrix] (-> matrix (get y) (get x)))

;; part 1
(defn neighbours [[x y] matrix]
  (->> [[(dec x) y]
        [(inc x) y]
        [x (dec y)]
        [x (inc y)]]
       (map #(hash-map :coords %, :value (at-coord % matrix)))
       (filter #(-> % :value some?))))

(defn lowpoint? [{:keys [height neighbours]}]
  (let [[lowest second-lowest] (sort (conj (map :value neighbours) height))]
    (and (= lowest height) (not= second-lowest height))))

(defn find-lowpoints [matrix]
  (->> (for [y (range 0 (count matrix))]
         (for [x (range 0 (count (get matrix y)))]
           {:height     (at-coord [x y] matrix)
            :neighbours (neighbours [x y] matrix)
            :coords     [x y]}))
       ;; flatten, but only one level
       (mapcat identity)
       ;flatten
       (filter lowpoint?)))

(defn risk-level-sum [input]
  (let [matrix (parse-input input)]
    (->> matrix
         (find-lowpoints)
         (map :height)
         (map inc)
         (apply +))))

;; part 2

(defn next-lower-value? [current-val next-val]
  (and (not= 9 next-val) (> current-val next-val)))

(defn follow-the-water
  "For the point [x y], recursively find the lowest surrounding point, until we reach the basin lowpoint."
  [matrix [x y]]
  (loop [pos [x y]]
    (let [value (at-coord pos matrix)
          neighbours (neighbours pos matrix)
          [next-val] (->> neighbours (filter #(next-lower-value? value (:value %))) (sort-by :value))]
      (cond
        (= 9 value) nil
        (some? next-val) (recur (:coords next-val))
        :else pos))))

(defn sum-basin-sizes [input]
  (let [matrix (parse-input input)
        basins (for [y (range 0 (count matrix))]
                 (for [x (range 0 (count (get matrix y)))
                       :when (not= 9 (at-coord [x y] matrix))]
                   (follow-the-water matrix [x y])))]
    (->> basins
         ;; flatten, but only one level
         (mapcat identity)
         (frequencies)
         (sort-by val)
         (reverse)
         (take 3)
         (map val)
         (apply *))))

(comment
  (def example-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")
  (def real-input (data/input 2021 9))

  ;; part 1
  (risk-level-sum example-input)                            ;; 15
  (risk-level-sum real-input)

  ;; part 2

  (sum-basin-sizes example-input)                           ;; 1134
  (sum-basin-sizes real-input)
  )