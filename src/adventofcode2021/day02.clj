(ns adventofcode2021.day02
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(defn parse-cmd [s]
  (let [[cmd value] (str/split s #" ")]
    [cmd (Integer/parseInt value)]))

(defn parse-input [s]
  (map (comp parse-cmd str/trim) (str/split-lines s)))

(defn result [{:keys [depth horizontal]}] (* depth horizontal))

;; part 1
(defn apply-cmd-1 [m [cmd value]]
  (case cmd
    "down" (update m :depth + value)
    "up" (update m :depth - value)
    "forward" (update m :horizontal + value)))

(defn follow-instructions [input]
  (reduce apply-cmd-1 {:depth 0 :horizontal 0} (parse-input input)))

;; part 2
(defn apply-cmd-2 [m [cmd value]]
  (case cmd
    "down" (update m :aim + value)
    "up" (update m :aim - value)
    "forward" (-> m
                  (update :horizontal + value)
                  (update :depth + (* (:aim m) value)))))

(defn follow-instructions-2 [input]
  (reduce apply-cmd-2 {:depth 0 :horizontal 0 :aim 0} (parse-input input)))

(comment
  (def example-input "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2")
  (def real-input (data/input 2021 2))

  (parse-input example-input)

  ;; part 1
  (follow-instructions example-input)
  (follow-instructions real-input)
  (result (follow-instructions example-input))
  (result (follow-instructions real-input))

  ;; part 2
  (follow-instructions-2 example-input)
  (follow-instructions-2 real-input)
  (result (follow-instructions-2 example-input))
  (result (follow-instructions-2 real-input))
  )