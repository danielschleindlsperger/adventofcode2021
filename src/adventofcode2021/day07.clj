(ns adventofcode2021.day07
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(defn parse-input [s]
  (map #(Integer/parseInt %) (str/split (str/trim s) #",")))

(defn fuel-to-pos [fuel-for-distance position-counts target-pos]
  (reduce (fn [fuel [pos pos-count]]
            (let [distance (Math/abs ^int (- pos target-pos))]
              (+ fuel (* (fuel-for-distance distance) pos-count))))
          0
          position-counts))

(defn best-crab-alignment [fuel-for-distance input]
  (let [crab-positions (parse-input input)
        position-counts (frequencies crab-positions)
        possible-positions (range (apply min crab-positions) (inc (apply max crab-positions)))]
    (->> possible-positions
         (map (partial fuel-to-pos fuel-for-distance position-counts))
         (sort)
         (first))))

;; part 1
(def best-crab-alignment-naive (partial best-crab-alignment identity))

;; part 2
(defn summation [n] (apply + (range 0 (inc n))))
(def best-crab-alignment-expensive (partial best-crab-alignment summation))

(comment
  (def example-input "16,1,2,0,4,2,7,1,2,14")
  (def real-input (data/input 2021 7))

  ;; part 1
  (best-crab-alignment-naive example-input)                 ;; 37
  (best-crab-alignment-naive real-input)

  ;; part 2
  (best-crab-alignment-expensive example-input)             ;; 168
  (best-crab-alignment-expensive real-input)
  )