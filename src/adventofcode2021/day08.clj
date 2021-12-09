(ns adventofcode2021.day08
  (:require [aocd.core :as data]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [s]
  (let [[signal-patterns output] (str/split s #" \| ")]
    {:signal-patterns (str/split signal-patterns #" ")
     :output          (str/split output #" ")}))
(defn parse-input [s]
  (map parse-line (str/split s #"\n")))

(defn find [f xs] (->> xs (filter f) first))

;; part 1
(def one? (comp (partial = 2) count))
(def four? (comp (partial = 4) count))
(def seven? (comp (partial = 3) count))
(def eight? (comp (partial = 7) count))
(def simple-digit? (some-fn one? four? seven? eight?))
(defn count-easy-digits [input]
  (->> (parse-input input)
       (mapcat #(->> % :output (filter simple-digit?)))
       (count)))

;; part 2
(defn fill-easy-digits [s]
  (condp = (count (str/split s #""))
    2 {:digit 1, :pattern s}
    4 {:digit 4, :pattern s}
    3 {:digit 7, :pattern s}
    7 {:digit 8, :pattern s}
    {:digit nil, :pattern s}))

(defn shares-elements-with? [digits digit n other-digit]
  (let [digit-set (set (:pattern digit))
        other-digit-set (set (:pattern (first (filter #(= other-digit (:digit %)) digits))))]
    (= n (count (set/intersection digit-set other-digit-set)))))

(defn deduce-6-9 [digits]
  (map (fn [digit]
         (cond
           (some? (:digit digit)) digit

           ;; deduce the value for `3`
           (and (= 5 (count (:pattern digit)))
                (shares-elements-with? digits digit 2 1)) (assoc digit :digit 3)

           ;; deduce the value for `6`
           (and (= 6 (count (:pattern digit)))
                (shares-elements-with? digits digit 1 1)) (assoc digit :digit 6)

           :else digit))
       digits))

(defn deduce-the-rest [digits]
  (map (fn [digit]
         (cond
           (some? (:digit digit)) digit

           ;; deduce the value for `9`
           (shares-elements-with? digits digit 5 3) (assoc digit :digit 9)

           ;; deduce the value for `2`
           (shares-elements-with? digits digit 4 6) (assoc digit :digit 2)

           (and (= 6 (count (:pattern digit)))
                (shares-elements-with? digits digit 4 3)) (assoc digit :digit 0)

           ;; 2 remains

           :else (assoc digit :digit 5)))
       digits))

(defn same-elements? [s1 s2]
  (and (= (count s1) (count s2))
       (= (count s1) (count (set/intersection (set s1) (set s2))))))

(defn ungarble-output [output patterns]
  (->> output
       (map (fn [pattern] (:pattern (find #(same-elements? pattern (:pattern %)) patterns))))
       (map (fn [pattern] (find #(= pattern (:pattern %)) patterns)))
       (map :digit))
  )

(defn decode-one-display [row]
  (let [solved (->> (:signal-patterns row)
                    (map fill-easy-digits)
                    deduce-6-9
                    deduce-the-rest
                    (ungarble-output (:output row)))]
    (Integer/parseInt (str/join "" (map str solved)))))

(defn decode-everythang [input]
  (->> (parse-input input)
       (map decode-one-display)
       (apply +)))

(comment
  (def example-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n")
  (def example-input-2 "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
  (def real-input (data/input 2021 8))

  (parse-input example-input)

  ;; part 1
  (count-easy-digits example-input)                         ;; 26
  (count-easy-digits real-input)

  ;; part 2
  (decode-everythang example-input)                         ;; 61229
  (decode-everythang real-input)
  )