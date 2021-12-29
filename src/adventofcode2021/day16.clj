(ns adventofcode2021.day16
  (:require [aocd.core :as data]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn split-str-at
  "Returns a 2-element vector after splitting the string at position (not index) `n`.
   The element at position `n` is contained in the first vector element."
  [s n]
  [(subs s 0 n) (subs s n)])

(def take-bits split-str-at)

(defn hex->bin [hex]
  (apply str (mapcat
               (fn [ch]
                 (get ["0000" "0001" "0010" "0011" "0100" "0101" "0110" "0111"
                       "1000" "1001" "1010" "1011" "1100" "1101" "1110" "1111"]
                      (Character/digit ^char ch 16)))
               hex)))

(defn bin->dec [s] (Long/parseLong s 2))

(defn parse-literal [bits]
  (loop [bits bits
         value-bits []]
    (let [[group bits] (take-bits bits 5)]
      (case (first group)
        \1 (recur bits (into value-bits (rest group)))
        ;; last group
        \0 [(bin->dec (apply str (into value-bits (rest group)))) bits]))))

(comment
  (parse-literal "101111111000101000")
  )

(defn parse [bits]
  (let [[version bits] (take-bits bits 3)
        [type-id bits] (take-bits bits 3)
        version (bin->dec version)
        type-id (bin->dec type-id)]
    ;; literal value
    (if (= 4 type-id)
      (let [[value bits] (parse-literal bits)]
        [{:type type-id, :version version, :value value} bits])
      ;; else (operator)
      (let [[length-type bits] (take-bits bits 1)]
        (case length-type
          ; The next 11 bits are a number that represents
          ; the number of sub-packets immediately contained
          "1" (let [[length bits] (take-bits bits 11)
                    length (bin->dec length)]
                (loop [children []
                       bits bits
                       length length]
                  (if (zero? length)
                    [{:type type-id, :version version, :children children} bits]
                    (let [[child bits] (parse bits)]
                      (recur (conj children child) bits (dec length))))))
          ; The next 15 bits are a number that represents
          ; the total length in bits of the sub-packets
          "0" (let [[length bits] (take-bits bits 15)
                    length (bin->dec length)
                    [bits continue-with] (take-bits bits length)]
                (loop [children []
                       bits bits]
                  (if (empty? bits)
                    [{:type type-id, :version version, :children children} continue-with]
                    (let [[child bits] (parse bits)]
                      (recur
                        (conj children child)
                        bits))))))))))

(defn sum-versions
  "Walk the tree, depth-first, and sum up the versions of all nodes along the way."
  [root-node]
  (loop [version-count 0
         stack (conj '() root-node)]
    (if (empty? stack)
      version-count
      (recur (+ version-count (:version (first stack)))
             (into (rest stack) (:children (first stack)))))))

(defn part1 [hex]
  (-> hex
      (hex->bin)
      (parse)
      (first)
      (sum-versions)))


(defn apply-children [node f]
  (apply f (map :value (:children node))))

(defn evaluate [root-node]
  (clojure.walk/postwalk
    (fn [node]
      (if (not (map? node))
        node
        (case (:type node)
          ;; literal value
          4 node

          0 (assoc node :value (apply-children node +))
          1 (assoc node :value (apply-children node *))
          2 (assoc node :value (apply-children node min))
          3 (assoc node :value (apply-children node max))

          5 (assoc node :value (if (apply-children node >) 1 0))
          6 (assoc node :value (if (apply-children node <) 1 0))
          7 (assoc node :value (if (apply-children node =) 1 0)))))
    root-node))

(defn part2 [hex]
  (->> hex
       (hex->bin)
       (parse)
       (first)
       (evaluate)
       :value))

(comment
  (def example-input "8A004A801A8002F478")
  (def real-input (data/input 2021 16))

  ;; part 1
  (part1 "8A004A801A8002F478")                              ;; 16
  (part1 "620080001611562C8802118E34")                      ;; 12
  (part1 "C0015000016115A2E0802F182340")                    ;; 23
  (part1 "A0016C880162017C3686B18A3D4780")                  ;; 31
  (part1 real-input)

  ;; part 2

  (part2 "D2FE28")                                          ;; 2021
  (part2 "8A004A801A8002F478")                              ;; 15
  (part2 "C200B40A82")                                      ;; 3
  (part2 "04005AC33890")                                    ;; 54
  (part2 "880086C3E88112")                                  ;; 7
  (part2 "CE00C43D881120")                                  ;; 9
  (part2 "D8005AC2A8F0")                                    ;; 1
  (part2 "F600BC2D8F")                                      ;; 0
  (part2 "9C005AC2F8F0")                                    ;; 0
  (part2 "9C0141080250320F1802104A08")                      ;; 1
  (part2 real-input)
  )
