(ns adventofcode2021.day14
  (:require [aocd.core :as data]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-rule [s]
  (let [[_ a b c] (re-matches #"([A-Z])([A-Z]) -> ([A-Z])" s)]
    [[a b] c]))
(defn parse-input [s]
  (let [[template insertion-rules] (str/split s #"\n\n")]
    {:template (str/split template #"")
     :rules    (into {} (map parse-rule (str/split insertion-rules #"\n")))}))

(defn safe+ [& xs]
  (apply + (filter some? xs)))
(defn safe- [& xs]
  (apply - (filter some? xs)))

;; part 1
(defn insert-pairs [pair-counts rules]
  (reduce (fn [counts [match insert]]
            (let [old-count (get pair-counts match 0)]
              (-> counts
                  (update match safe- old-count)
                  (update [(first match) insert] safe+ old-count)
                  (update [insert (last match)] safe+ old-count))))
          pair-counts
          rules))


(defn element-count [counts el]
  (let [a (->> counts
               (filter (comp (partial = el) first key))
               (map val)
               (apply +)

               )
        b (->> counts
               (filter (comp (partial = el) second key))
               (map val)
               (apply +)

               )]
    (max a b)))

(defn poly-num [counts]
  (let [elements (->> counts (mapcat key) set)]
    (->> elements
         (map #(element-count counts %))
         (apply (juxt max min))
         (apply -))))

(defn polymerization [input n]
  (let [{:keys [template rules]} (parse-input input)]
    (->> (range n)
         (reduce (fn [poly _] (insert-pairs poly rules))
                 (frequencies (partition 2 1 template)))
         (poly-num))))

(comment
  (def example-input "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C")
  (def real-input (data/input 2021 14))

  (parse-input example-input)

  ;; part 1
  (time (polymerization example-input 10))                  ;; 1588
  (time (polymerization real-input 10))

  ;; part 2
  (time (polymerization example-input 40))
  (time (polymerization real-input 40))
  )
