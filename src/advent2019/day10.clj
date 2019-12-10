(ns advent2019.day10
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [gcd]]
            [clojure.set :as s]))

(def data
  (with-open [rdr (io/reader (io/resource "day10.in"))]
    (into [] (line-seq rdr))))

(defn s* [[x y] i] [(* x i) (* y i)])

(defn cap [i m]
  (cond
    (< i 0) 0
    (>= i m) (dec m)
    :else i))

(defn s-cap [[x y] w h]
  [(cap x w) (cap y h)])

(defn s-cap* [c w h i]
  (s-cap (s* c i) w h))

;; divide by GCD
;; e.g.
;; [5 5] -> [1 1]
;; [4 6] -> [2 2]
(defn to-angle [[cx cy] [x y]]
  (let [dx (- x cx)
        dy (- y cy)
        g (gcd dx dy)]

    [(quot dx g) (quot dy g)]))

;; (to-angle [0 0] [5 5])
;; (to-angle [0 0] [4 6])
;; (to-angle [0 0] [-5 5])

(defn l-hor [x1 x2 y] (map #(vector %1 y) (range x1 x2)))
(defn l-ver [x y1 y2] (map #(vector x %1) (range y1 y2)))

(defn rect [tlx tly brx bry]
  (concat 
   (l-hor tlx brx tly)
   (l-hor tlx brx bry)
   (l-ver tlx (inc tly) bry)
   (l-ver brx tly (inc bry))))

;; Expand rectangle with center in [x y]
;; Keep set of obstructed angles d = [dx dy]. Any i * d will be obstructed also.
(defn visible-count [grid x y w h]
  (let [max-d (max x y (- w x) (- h y))]
    (loop [d 1 oa #{}]
      (if (= d max-d) (count oa)
          (let [tlx (cap w (- x d))
                brx (cap w (+ x d))
                tly (cap h (- y d))
                bry (cap h (+ y d))

                obstructed-angles
                (->>
                 (rect tlx tly brx bry)
                 (filter (fn [[x y]] (= \# (get-in grid [y x]))))
                 (filter #(not= % [x y]))
                 (map (partial to-angle [x y]))
                 (set))]
            
            (recur (inc d)
                   (s/union oa obstructed-angles)))))))

(defn solution [data]
  (let [h (count data)
        w (count (first data))
        s (* h w)]
    (loop [i 0 m 0]
      (if (= i s) m
          (let [x (mod i w)
                y (quot i w)
                ast? (= \# (get-in data [y x]))]
            (if ast? 
              (recur (inc i) (max m (visible-count data x y w h)))
              (recur (inc i) m)))))))

(def tst [".#..#"
          "....."
          "#####"
          "....#"
          "...##"])

(assert (= 8 (solution tst)))
(assert (= 309 solution data))

(def tst1
  [".#..##.###...#######"
   "##.############..##."
   ".#.######.########.#"
   ".###.#######.####.#."
   "#####.##.#.##.###.##"
   "..#####..#.#########"
   "####################"
   "#.####....###.#.#.##"
   "##.#################"
   "#####.##.###..####.."
   "..######..##.#######"
   "####.##.####...##..#"
   ".#####..#.######.###"
   "##...#.##########..."
   "#.##########.#######"
   ".####.#.###.###.#.##"
   "....##.##.###..#####"
   ".#.#.###########.###"
   "#.#.#.#####.####.###"
   "###.##.####.##.#..##"])

(assert (= 210 (solution tst1)))

;; The Elves are placing bets on which will be the 200th asteroid to be
;; vaporized. Win the bet by determining which asteroid that will be; what do
;; you get if you multiply its X coordinate by 100 and then add its Y
;; coordinate? (For example, 8,2 becomes 802.)
