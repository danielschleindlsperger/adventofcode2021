(ns adventofcode2021.day15
  (:require [aocd.core :as data]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map-keyfn]]))

(defn parse-input [s]
  (vec (map (fn [l] (vec (map #(Integer/parseInt %) (str/split l #""))))
            (str/split s #"\n"))))

(defn at-coord [matrix [x y]] (-> matrix (get y) (get x)))

(defn all-elements [matrix]
  (for [y (range (count matrix))
        x (range (count (get matrix y)))]
    [x y]))

(def infinity Long/MAX_VALUE)

;; part 1

(def nexts [[0 -1] [1 0] [0 1] [-1 0]])
(defn neighbors [board [x y]]
  (->> nexts
       (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
       (filter (partial at-coord board))))

(defn last-cell
  "[x, y] coordinate of the bottom right cell in in the matrix"
  [matrix]
  [(dec (count (first matrix))) (dec (count matrix))])

(defn next-node
  "select the unvisited node that is marked with the smallest tentative distance"
  [q]
  (let [x (first (seq q))]
    (when x (key x))))

(defn initial-q [board start]
  (reduce (fn [q node] (assoc q node {:tentative-rl (if (= node start) 0 infinity)
                                      :pos          node
                                      :rl           (at-coord board node)}))
          (priority-map-keyfn :tentative-rl)
          (all-elements board)))

(defn next-q [q current unvisited-neighbors]
  (-> (reduce (fn [q' neighbor]
                (let [old-distance (get-in q [neighbor :tentative-rl])
                      new-distance (+ (get-in q [current :tentative-rl])
                                      (get-in q [neighbor :rl]))]
                  (if (< new-distance old-distance)
                    (update q' neighbor assoc :tentative-rl new-distance)
                    q')))
              q
              unvisited-neighbors)
      (dissoc current)))

(defn dijkstra [board start target]
  (loop [current start
         q (initial-q board start)
         visited {}]
    (if (not (contains? q target))
      visited
      (let [unvisited-neighbors (filter #(contains? q %) (neighbors board current))
            q' (next-q q current unvisited-neighbors)
            nn (next-node q')]
        (recur nn q' (assoc visited current (q current)))))))

;; part 1

(defn part1 [input]
  (let [board (parse-input input)
        target (last-cell board)]
    (:tentative-rl (get (dijkstra (parse-input input) [0 0] target) target))))

;; part 2

(defn add-with-overflow [a b]
  (let [sum (+ a b)]
    (if (< 9 sum)
      (inc (rem sum 10))
      sum)))

(defn scale-board [board]
  (let [horizontal (map (fn [row]
                          (vec (apply concat row (map (fn [d] (map (partial add-with-overflow d) row)) (range 1 5)))))
                        board)
        verticals (map (fn [i] (map (fn [row]
                                      (vec (map (partial add-with-overflow i) row)))
                                    horizontal))
                       (range 1 5))]
    (vec (apply concat horizontal verticals))))

(defn part2 [input]
  (let [board (parse-input input)
        scaled-board (scale-board board)
        target (last-cell scaled-board)]
    (:tentative-rl (get (dijkstra scaled-board [0 0] target) target))))

(comment
  (def example-input "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581")
  (def real-input (data/input 2021 15))

  ;; part 1
  (part1 example-input)                                     ;; 40
  (part1 real-input)

  ;; part 2
  (part2 example-input)                                     ;; 315
  (part2 real-input)
  )
