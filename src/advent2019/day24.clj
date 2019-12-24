(ns advent2019.day24
  (:require [clojure.string :as string]))

(def data
  ["#.#.."
   ".#.#."
   "#...#"
   ".#..#"
   "##.#."])

(def test-data
  ["....#"
   "#..#."
   "#..##"
   "..#.."
   "#...."])

(defn s-to-num [s]
  (->> s
       (map-indexed
        (fn [i v]
          (if (= \# v)
            (bit-shift-left 1 i)
            0)))
       (reduce +)))

(defn grid-to-num [g] (s-to-num (string/join g)))

(defn masks [w h]
  (into []
        (for [y (range h)
              x (range w)]

          (let [bit (+ x (* y w))
                v (bit-shift-left 1 bit)]
            (+
             (if (zero? x) 0 (bit-shift-right v 1))
             (if (= x (dec w)) 0 (bit-shift-left v 1))
             (if (zero? y) 0 (bit-shift-right v w))
             (if (= y (dec h)) 0 (bit-shift-left v w)))))))

(defn bit-count [v]
  (loop [v v c 0]
    (if (= v 0) c
        (recur (bit-shift-right v 1)
               (+ c (bit-and v 1))))))

;; A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
;; An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
(defn tick [masks grid]
  (->> masks
       (map-indexed
        (fn [i v]
          (let [around (bit-count (bit-and v grid))
                my-bit (bit-shift-left 1 i)
                me (bit-and my-bit grid)]
            (cond
              (and (= me 0) (or (= 1 around) (= 2 around))) my-bit
              (and (> me 0) (not= 1 around)) 0
              :else me))))
       (reduce +)))

(defn num-to-s [l v]
  (loop [v v c (transient []) l l]
    (if (= l 0)
      (apply str (persistent! c))
        (recur (bit-shift-right v 1)
               (conj! c (if (zero? (bit-and v 1)) \. \#))
               (dec l)))))

(defn print-grid [g]
  (println "---")
  (->> g
       (num-to-s 25)
       (partition 5)
       (map (partial apply str))
       (run! println))
  g)

(defn part1 [g]
  (let [g (grid-to-num g)
        m (masks 5 5) ]
    (loop [g g acc (transient #{g})]
      (let [v (tick m g)]
        (if (acc v) v
            (recur v (conj! acc v)))))))

(part1 data)
