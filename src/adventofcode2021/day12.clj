(ns adventofcode2021.day12
  (:require [aocd.core :as data]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-line [s] (into [] (map keyword (str/split s #"-"))))
(defn parse-input [s] (vec (map parse-line (str/split s #"\n"))))

(defn upper-case? [s] (= s (str/upper-case s)))
(defn separate
  "Returns a two-element vector with all truthy and all falsy elements respectively."
  [pred xs]
  (let [groups (group-by (comp boolean pred) xs)]
    [(get groups true) (get groups false)]))

(defn ->nav-tree [cave-conns]
  (reduce (fn [nav-tree [start end]]
            (-> nav-tree
                (assoc start (conj (or (get nav-tree start) #{}) end))
                (assoc end (conj (or (get nav-tree end) #{}) start))))
          {}
          cave-conns))

(defn find-paths
  [nav-tree valid-path?]
  (loop [paths #{}
         queued-paths (map (fn [x] [:start x]) (:start nav-tree))]
    (let [curr-path (first queued-paths)
          new-paths (->> (get nav-tree (last curr-path))
                         (filter (partial valid-path? curr-path))
                         (map (fn [x] (conj curr-path x))))
          [finished unfinished] (separate (comp (partial = :end) last) new-paths)]
      (if (empty? queued-paths)
        paths
        (recur
          (reduce (fn [ps p] (conj ps p)) paths finished)
          (into (rest queued-paths) unfinished))))))

;; part 1


(defn small-caves-at-most-once? [curr-path cave]
  (or (upper-case? (name cave))
      (zero? (count (filter (partial = cave) curr-path)))))

(defn part1 [input]
  (-> input
      parse-input
      ->nav-tree
      (find-paths small-caves-at-most-once?)
      count))

;; part 2

(defn part-2-pred [curr-path cave]
  (cond
    ;; start can only be visited once
    (= :start cave) false
    ;; big caves can be visited any number of times
    (upper-case? (name cave)) true
    :else (let [freqs (->> (conj curr-path cave)
                           (filter (comp (complement upper-case?) name))
                           (frequencies)
                           (map val))]
            (and
              ;; a small cave can only be visited twice at maximum
              (= 0 (count (filter (partial <= 3) freqs)))
              ;; at maximum one small cave can be visited twice
              (>= 1 (count (filter (partial = 2) freqs)))))))

(defn part2 [input]
  (-> input
      parse-input
      ->nav-tree
      (find-paths part-2-pred)
      count))

(comment
  (def example-input "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")
  (def real-input (data/input 2021 12))

  ;; part 1
  (time (part1 example-input))                              ;; 10
  (time (part1 real-input))

  ;; part 2
  (time (part2 example-input))                              ;; 36
  (time (part2 real-input))
  )