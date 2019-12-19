(ns advent2019.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [quil.core :as q]
            [quil.middleware :as m]
            [advent2019.intcode :refer [intcode-run halt? code-to-map]]))
(def data
  (with-open [rdr (io/reader (io/resource "day19.in"))]
    (into [] (map #(Long/parseLong %) (.split (.readLine rdr) ",")))))

(def code (code-to-map data))

(defn pulled? [y x]
  (when (and (>= x 0) (>= y 0))
    (->>
     {:input [x y]}
     (intcode-run code)
     (second)
     (:output)
     (= 1))))

(defn scan [pred win-height]
  (loop [y 1
         lines [[0 0]]]

    (if (pred lines) 
      [y lines]
      (let [[prev-y [min-x max-x]]
            (first (keep #(when (lines %) [(- y (- (count lines) %)) (lines %)])
                         (range (dec (count lines)) -1 -1)))

            delta (- y prev-y)]

        (let [min-x
              (first (filter (partial pulled? y) (range (- min-x delta) (+ min-x delta 1))))

              max-x
              (when min-x
                (first (filter (partial pulled? y) (range (+ max-x delta) (- max-x delta 1) -1))))]

          (recur (inc y)
                 (into [] (take-last win-height
                                     (conj lines (when min-x [min-x max-x]))))))))))

(def part1
  (->>
   (scan #(= 50 (count %)) 100)
   (last)
   (keep identity)
   (map (fn [[from to]] (- to from -1)))
   (reduce +)))

;; Find the 100x100 square closest to the emitter that fits entirely within the
;; tractor beam; within that square, find the point closest to the emitter. What
;; value do you get if you take that point's X coordinate, multiply it by 10000,
;; then add the point's Y coordinate? (In the example above, this would be
;; 250020.)
(defn santa-fits [w lines]
  (let [[top-min top-max] (first lines)
        [bot-min bot-max] (last lines)

        fits?
        (and top-min
             (= bot-min (- top-max w -1)))]

    (when fits?
      ;; (pp/pprint lines)
      true)))

(santa-fits 10
            [[17 34] [18 15] [19 37] [20 39] [21 39] [21 39] [22 39] [23 39] [24 39] [25 39]])


;; 6641088 high
;; 6601083 low
;; 6611083
(def part2
  (let [santa-width 100]
    (let [[y [[min-x max-x]]] (scan (partial santa-fits santa-width) santa-width)]
      (+ (* 10000 (- max-x santa-width -1)) (- y santa-width)))))

(defn setup []
  (q/frame-rate 30))

(defn draw [state]
  (q/background 255)
  (q/rect-mode :center)
  (q/stroke 0)

  (run!
   (fn [[x y]]
     (q/rect (* x 10) (* y 10) 9 9))
   part2))

(defn start []
  (q/defsketch beam
    :title "beam"
    :size [500 500]
    :setup setup
    :update identity
    :middleware [m/fun-mode]
    :draw (fn [& args]
            (q/translate 5 5)
            (apply draw args))))
