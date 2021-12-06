(ns adventofcode2021.day06
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(defn parse-input [s]
  (map #(Integer/parseInt %) (str/split (str/trim s) #",")))

(defn safe+ [& xs]
  (apply + (filter some? xs)))

(defn simulate-day [population]
  (reduce (fn [popu [fish-age fish-count]]
            (if (= 0 fish-age)
              (-> popu
                  (update 6 safe+ fish-count)
                  (update 8 safe+ fish-count)
                  (update fish-age - fish-count))
              (-> popu
                  (update (dec fish-age) safe+ fish-count)
                  (update fish-age - fish-count))))
          population
          population))

(defn lanternfish-after-days [initial-population after-days]
  (loop [population (frequencies initial-population), days 0]
    (if (<= after-days days)
      population
      (recur (simulate-day population) (inc days)))))

(defn sum-vals [m] (reduce (fn [sum x] (+ sum (val x))) 0 m))

(comment
  (def example-input "3,4,3,1,2")
  (def real-input (data/input 2021 6))

  ;; part 1
  (sum-vals (lanternfish-after-days (parse-input example-input) 80)) ;; 5934
  (sum-vals (lanternfish-after-days (parse-input real-input) 80))

  ;; part 2

  (time (sum-vals (lanternfish-after-days (parse-input example-input) 256))) ;; 26984457539
  (time (sum-vals (lanternfish-after-days (parse-input real-input) 256)))
  )