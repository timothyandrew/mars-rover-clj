(ns mars-rover.core
  (:gen-class)
  (:use clojure.java.io)
  (:require [clojure.string :as str]))

(defn split-chars
  [string]
  (rest (str/split string #"")))

(defn mars-rover-turn-left
  [[x y heading]]
  (let [heading (cond
                  (= heading "N") "W"
                  (= heading "W") "S"
                  (= heading "S") "E"
                  (= heading "E") "N")]
    [x y heading]))

(defn mars-rover-turn-right
  [position]
  (-> position mars-rover-turn-left mars-rover-turn-left mars-rover-turn-left))

(defn mars-rover-move-forward
  [[x y heading]]
  (let [x (cond
            (= heading "W") (dec x)
            (= heading "E") (inc x)
            :else x)
        y (cond
            (= heading "N") (inc y)
            (= heading "S") (dec y)
            :else y)]
    [x y heading]))

(defn mars-rover-execute-instruction
  [position instruction]
  (cond
    (= instruction "L") (mars-rover-turn-left position)
    (= instruction "R") (mars-rover-turn-right position)
    (= instruction "M") (mars-rover-move-forward position)))

(defn mars-rover-initialize
  [position instructions]
  (loop [[x y heading] (str/split position #" ")
         [i & instructions] (split-chars instructions)]
    (let [position [(Integer/parseInt x) (Integer/parseInt y) heading]
          [x y heading] (mars-rover-execute-instruction position i)]
      (if instructions
        (recur [(str x) (str y) heading]  instructions)
        [x y heading]))))

(defn -main
  [filename & args]
  (let [[_ & lines] (-> filename slurp str/split-lines)]
    (loop [[position instructions & lines] lines]
      (println (mars-rover-initialize position instructions))
      (if lines (recur lines)))))
