(ns adventofcode2021.day11
  (:require [aocd.core :as data]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-line [s] (vec (map #(Integer/parseInt %) (str/split s #""))))
(defn parse-input [s] (vec (map parse-line (str/split s #"\n"))))

(defn at-coord [[x y] matrix] (-> matrix (get y) (get x)))
(defn matrix-map [f matrix]
  (vec (for [row matrix]
         (vec (for [cell row]
                (f cell))))))

;; part 1

(defn all-coords [board]
  (->> (for [y (range 0 (count board))
             x (range 0 (count (get board y)))
             ]
         [x y])
       flatten
       (partition 2)))

(defn adjacents [board [x y]]
  (filter (fn [coords] (some? (at-coord coords board)))
          [[x (dec y)]
           [(inc x) (dec y)]
           [(inc x) y]
           [(inc x) (inc y)]
           [x (inc y)]
           [(dec x) (inc y)]
           [(dec x) y]
           [(dec x) (dec y)]]))

(defn reset-flashes [board]
  (matrix-map #(if (< 9 %) 0 %) board))

(defn flash-loop [initial-octos]
  (loop [current-board initial-octos
         inc-stack (all-coords initial-octos)
         flash-count 0]
    (let [[x y] (first inc-stack)
          new-board (update-in current-board [y x] inc)
          flash? (= 10 (at-coord [x y] new-board))
          new-incs (if flash? (adjacents current-board [x y]) '())]
      (if (and (empty? (rest inc-stack)) (empty? new-incs))
        {:board (reset-flashes new-board) :flash-count flash-count}
        (recur new-board
               (into (rest inc-stack) new-incs)
               (if flash? (inc flash-count) flash-count))))))

(defn flash-count [input n]
  (let [initial-board (parse-input input)]
    (->> (range n)
         (reduce (fn [boards _n]
                   (let [{:keys [board]} (last boards)]
                     (conj boards (flash-loop board))))
                 [{:board initial-board :flash-count 0}])
         (map :flash-count)
         (apply +))))

;; part 2

(defn synchronized? [board]
  (->> board
       flatten
       (every? (partial = 0))))

(defn synchronized-flash [input]
  ;; unfortunately `reduce` consumes lazy sequence, so we can't share as much between the two part as we'd like
  (loop [board (parse-input input)
         n 1]
    (let [{new-board :board} (flash-loop board)]
      (if (synchronized? new-board)
        n
        (recur new-board (inc n))))))

(comment
  (def super-small-example-input "111\n199\n191")
  (def small-example-input "11111\n19991\n19191\n19991\n11111")
  (def example-input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")
  (def real-input (data/input 2021 11))

  ;; part 1
  (time (flash-count super-small-example-input 2))
  (time (flash-count small-example-input 10))
  (time (flash-count example-input 100))                    ;; 1656
  (time (flash-count real-input 100))

  ;; part 2
  (time (synchronized-flash example-input))                 ;; 195
  (time (synchronized-flash real-input))
  )