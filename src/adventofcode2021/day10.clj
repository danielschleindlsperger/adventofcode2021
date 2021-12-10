(ns adventofcode2021.day10
  (:require [aocd.core :as data]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-line [s] (str/split s #""))
(defn parse-input [s] (map parse-line (str/split s #"\n")))

(def syntax {"(" ")", "[" "]", "{" "}", "<" ">"})
(defn opening-bracket? [s] (some? (syntax s)))
(defn brackets-match? [opening closing] (= (get syntax opening) closing))

(defn first-wrong-char-or-stack
  "Validate the syntax and return either the char that caused a syntax error, or the opening bracket stack up until that point if the line is incomplete."
  [xs]
  (reduce (fn [stack bracket]
            (cond
              (opening-bracket? bracket) (conj stack bracket)
              (brackets-match? (first stack) bracket) (rest stack)
              :else (reduced bracket)))
          '()
          xs))

;; part 1
(def char->error-score {")" 3, "]" 57, "}" 1197, ">" 25137})

(defn syntax-error-score [input]
  (->> (parse-input input)
       (map first-wrong-char-or-stack)
       (filter string?)
       (map char->error-score)
       (apply +)))

;; part 2

(def char->completion-score {")" 1, "]" 2, "}" 3, ">" 4})

(defn autocomplete-score [xs]
  (reduce #(+ (* %1 5) (char->completion-score %2)) 0 xs))

(defn middle [xs]
  (nth (sort xs) (Math/floor (/ (count xs) 2))))

(defn autocomplete-line [stack]
  (->> stack (map syntax)))

(defn autocomplete-middle-score [input]
  (->> (parse-input input)
       (map first-wrong-char-or-stack)
       (filter sequential?)
       (map autocomplete-line)
       (map autocomplete-score)
       (middle)))

(comment
  (def example-input "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]")
  (def real-input (data/input 2021 10))

  ;; part 1
  (syntax-error-score example-input)                        ;; 26397
  (syntax-error-score real-input)

  ;; part 2
  (autocomplete-middle-score example-input)                 ;; 288957
  (autocomplete-middle-score real-input)
  )