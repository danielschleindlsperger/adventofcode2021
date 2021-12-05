(ns adventofcode2021.day05
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(defn parse-line-coords
  "Parse each line to a format of [x1 y1, x2 y2]"
  [s]
  (map #(Long/parseLong %)
       (rest (re-find #"(\d+),(\d+) -> (\d+),(\d+)" s))))
(defn parse-input [s]
  (map parse-line-coords (str/split s #"\n")))

;; part 1

(defn horizontal-or-vertical? [[x1 y1, x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn points-covered-right-angle [[x1 y1, x2 y2 :as line-points]]
  (cond
    (not (horizontal-or-vertical? line-points)) []
    ;; y values change
    (= x1 x2) (map (fn [y] [x1 y]) (range (min y1 y2) (inc (max y1 y2))))
    ;; x values change
    (= y1 y2) (map (fn [x] [x y1]) (range (min x1 x2) (inc (max x1 x2))))))

;; part 2

(defn points-covered [[x1 y1, x2 y2]]
  (loop [x x1, y y1, points []]
    (if (and (= x x2) (= y y2))
      (conj points [x y])
      (recur (- x (Integer/signum (- x x2)))
             (- y (Integer/signum (- y y2)))
             (conj points [x y])))))

(defn count-overlaps [input right-angles-only?]
  (->> (parse-input input)
       (map (if right-angles-only? points-covered-right-angle points-covered))
       ;; flatten, but only one level
       (mapcat identity)
       (frequencies)
       (filter #(<= 2 (val %)))
       (count)))

(comment
  (def example-input "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2")
  (def real-input (data/input 2021 5))

  (parse-input example-input)

  ;; part 1
  (count-overlaps example-input true)                       ;; 5
  (count-overlaps real-input true)

  ;; part 2
  (count-overlaps example-input false)                      ;; 12
  (count-overlaps real-input false)
  )