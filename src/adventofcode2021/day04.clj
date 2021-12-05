(ns adventofcode2021.day04
  (:require [aocd.core :as data]
            [clojure.string :as str]))

(defn parse-row [s] (->> (str/split s #" ")
                         (map str/trim)
                         (remove empty?)
                         (map #(Long/parseLong %))
                         vec))
(defn parse-board [s] (vec (map parse-row (str/split s #"\n"))))
(defn parse-input [s]
  (let [[play-seq & boards] (str/split s #"\n\n")]
    {:play-seq (map #(Long/parseLong %) (str/split play-seq #","))
     :boards   (map parse-board boards)}))

(defn find [pred coll] (->> coll (filter pred) first))
(defn transpose [m]
  (apply mapv vector m))

;; part 1

(defn bingo-turns [nums]
  (reduce (fn [turns num]
            (conj turns (vec (conj (last turns) num))))
          []
          nums))

(conj [] (conj [] 1))

(defn bingo? [row-or-col nums]
  ;; TODO: create set outside the closure
  (every? #(contains? (set nums) %) row-or-col))

(defn row-bingo
  "Check if the board contains a 'BINGO' in a row and return the row"
  [board nums]
  (find #(bingo? % nums) board))

(defn col-bingo
  "Check if the board contains a 'BINGO' in a column and return the column"
  [board nums]
  (find #(bingo? % nums) (transpose board)))

(defn bingo [board nums]
  (or (row-bingo board nums)
      (col-bingo board nums)))

(defn sum-unmarked [board nums]
  (->> board
       (flatten)
       (remove #(contains? (set nums) %))
       (reduce +)))

(defn play-bingo [boards turns]
  (->> turns
       (bingo-turns)
       (mapcat (fn [turn-nums]
                 (map (fn [board]
                        {:board     board
                         :turn-nums turn-nums
                         :bingo     (bingo board turn-nums)})
                      boards)))))

(defn result [board turns-until-win]
  (let [winning-number (last turns-until-win)]
    (* winning-number (sum-unmarked board turns-until-win))))

(defn win-bingo [input]
  (let [{:keys [play-seq boards]} (parse-input input)
        {:keys [board turn-nums]} (find :bingo (play-bingo boards play-seq))]
    (result board turn-nums)))

;; part 2

;; cases:
;; - when the board is not in boards-left, bail with the same boards-left
;; - when the turn contains a bingo, remove the board from boards-left
;; - when there's no more boards-left left, this is the last winning turn
(defn last-board-wins [boards results]
  (reduce (fn [boards-left result]
            (if (not (contains? boards-left (:board result)))
              boards-left
              (let [boards-left (if (some? (:bingo result))
                                  (disj boards-left (:board result))
                                  boards-left)]
                (if (= 0 (count boards-left))
                  (reduced result)
                  boards-left))))
          (set boards)
          results))

(defn lose-bingo [input]
  (let [{:keys [play-seq boards]} (parse-input input)
        {:keys [board turn-nums]} (last-board-wins boards (play-bingo boards play-seq))]
    (result board turn-nums)))

(comment
  (def example-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")
  (def real-input (data/input 2021 4))

  ;; part 1
  (win-bingo example-input)                                 ;; 4512
  (win-bingo real-input)

  ;; part 2
  (lose-bingo example-input)                                ;; 1924
  (lose-bingo real-input)
  )