(ns adventofcode2021.day09
  (:require [aocd.core :as data]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-nums [s] (vec (map #(Integer/parseInt %) (str/split s #""))))
(defn parse-input [s]
  (vec (map parse-nums (str/split s #"\n"))))

;; part 1
(defn neighbours [matrix y x]
  (filter some? [(get (get matrix y) (dec x))
                 (get (get matrix y) (inc x))
                 (get (get matrix (dec y)) x)
                 (get (get matrix (inc y)) x)]))

(defn lowpoint? [{:keys [height neighbours]}]
  (let [[lowest second-lowest] (sort (conj neighbours height))]
    (and (= lowest height) (not= second-lowest height))))

(defn risk-level-sum [input]
  (let [matrix (parse-input input)]
    (->> matrix (map-indexed (fn [i row]
                               (map-indexed (fn [y, height] {:height height :neighbours (neighbours matrix i y)})
                                            row)))
         flatten
         (filter lowpoint?)
         (map :height)
         (map inc)
         (apply +)
         )))

;; part 2

(comment
  (def example-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")
  (def real-input (data/input 2021 9))

  ;; part 1
  (risk-level-sum example-input)                                       ;; 15
  (risk-level-sum real-input)

  ;; part 2
  )