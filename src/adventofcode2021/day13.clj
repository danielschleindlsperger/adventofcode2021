(ns adventofcode2021.day13
  (:require [aocd.core :as data]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-dot [s] (vec (map #(Integer/parseInt %) (str/split s #","))))
(defn parse-fold [s]
  (let [[_ dir value] (re-matches #"fold along (y|x)=(\d*)" s)]
    {:dir (keyword dir) :value (Integer/parseInt value)}))
(defn parse-input [s]
  (let [[dots folds & rest] (str/split s #"\n\n")]
    {:dots  (set (map parse-dot (str/split dots #"\n")))
     :folds (map parse-fold (str/split folds #"\n"))}))

;; part 1
;; fold along horizontal axis
(defn fold-y [points value]
  (->> points
       (remove (fn [[_x y]] (= y value)))
       (map (fn [[x y]]
              (let [delta-y (- y value)
                    new-y (if (< value y) (- y (* 2 delta-y)) y)]
                [x new-y])))
       (into #{})))

;; fold along vertical axis
(defn fold-x [points value]
  (->> points
       (remove (fn [[x _y]] (= x value)))
       (map (fn [[x y]]
              (let [delta-x (- x value)
                    new-x (if (< value x) (- x (* 2 delta-x)) x)]
                [new-x y])))
       (into #{})))

(defn fold [dots instruction]
  (let [{:keys [dir value]} instruction]
    (case dir
      :x (fold-x dots value)
      :y (fold-y dots value)
      (throw (ex-info "Unknown fold direction" {:instruction instruction})))))

(defn fold-first-instruction [input]
  (let [{:keys [dots folds]} (parse-input input)]
    (count (fold dots (first folds)))))

;; part 2

(defn find-code [input]
  (let [{:keys [dots folds]} (parse-input input)]
    (reduce fold dots folds)))

(defn draw-paper [dots]
  (let [max-x (->> dots (map first) sort last)
        max-y (->> dots (map last) sort last)
        table (for [y (range 0 (inc max-y))]
                (into {} (for [x (range 0 (inc max-x))]
                           [x (if (contains? dots [x y]) "#" ".")])))]
    (clojure.pprint/print-table (range 0 (inc max-x)) table)))

(comment
  (def example-input "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5")
  (def real-input (data/input 2021 13))

  ;; part 1
  (time (fold-first-instruction example-input))             ;; 17
  (time (fold-first-instruction real-input))

  ;; part 2
  (time (draw-paper (find-code example-input)))
  (time (draw-paper (find-code real-input)))
  )