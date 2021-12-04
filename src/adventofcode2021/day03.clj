(ns adventofcode2021.day03
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(defn bin->dec [xs] (Integer/parseInt (str/join xs) 2))

(defn parse-input [s]
  (map #(str/split % #"") (map str/trim (str/split-lines s))))

(defn find-bit-by [f bits default-on-tie]
  (let [freq (frequencies bits)]
    (if (= (get freq "0") (get freq "1"))
      default-on-tie
      (key (apply f val freq)))))

(defn most-common-bit [bits] (find-bit-by max-key bits "1"))
(defn least-common-bit [bits] (find-bit-by min-key bits "0"))

;; part 1

(defn transpose [m]
  (apply mapv vector m))

(defn power-consumption [input]
  (let [matrix (transpose (parse-input input))
        gamma-rate (bin->dec (map most-common-bit matrix))
        epsilon-rate (bin->dec (map least-common-bit matrix))]
    (* gamma-rate epsilon-rate)))

;; part 2

(defn rating [bit-criteria-f diagnostics]
  (reduce (fn [entries i]
            (let [bits (map #(nth % i) entries)
                  mcb (bit-criteria-f bits)
                  remaining-entries (filter #(= mcb (nth % i)) entries)]
              (if (= 1 (count remaining-entries))
                (reduced (bin->dec (first remaining-entries)))
                remaining-entries)))
          diagnostics
          (range)))

(def oxygen-generator-rating (partial rating most-common-bit))
(def co2-scrubber-rating (partial rating least-common-bit))

(defn life-support-rating [input]
  (let [diagnostics (parse-input input)]
    (* (oxygen-generator-rating diagnostics)
       (co2-scrubber-rating diagnostics))))

(comment
  (def example-input "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010")
  (def real-input (data/input 2021 3))

  ;; part 1
  (power-consumption example-input)
  (power-consumption real-input)

  ;; part 2
  (life-support-rating example-input)
  (life-support-rating real-input)
  )