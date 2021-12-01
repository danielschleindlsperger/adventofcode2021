(ns adventofcode2021.day01
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(defn parse-input-lines [f s]
  (map (comp f str/trim) (str/split-lines s)))

;; part 1
(defn count-measurement-increases [measurements]
  (->> (parse-input-lines #(Integer/parseInt %) measurements)
       (partition 2 1)
       (filter (partial apply <))
       (count)))

;; part 2
(defn count-measurement-increases-windowed [measurements]
  (->> (parse-input-lines #(Integer/parseInt %) measurements)
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (filter (partial apply <))
       (count)))

(comment
  (def example-input "199\n200\n208\n210\n200\n207\n240\n269\n260\n263")
  (def real-input (data/input 2021 1))

  ;; part 1
  (count-measurement-increases example-input)
  (count-measurement-increases real-input)

  ;; part 2
  (count-measurement-increases-windowed example-input)
  (count-measurement-increases-windowed real-input))