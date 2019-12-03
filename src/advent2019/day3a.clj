(ns advent2019.day3a
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))


(with-open [rdr (io/reader (io/resource "day3.in"))]
  (def line1 (into [] (-> rdr
                          (.readLine)
                          (.split ","))))

  (def line2 (into [] (-> rdr
                          (.readLine)
                          (.split ",")))))

(def delta {\R [1 0]
            \L [-1 0]
            \U [0 1]
            \D [0 -1]})

(defn line-to-points [line]
  (loop [acc (transient {})
         x 0 y 0 l 0
         line line]
    (if (empty? line) (persistent! acc)
        
        (let [dir (first line)
              [dx dy] (get delta (first dir))
              len (inc (Integer/parseInt (subs dir 1)))

              points
              (take len
                    (iterate
                     (fn [[x y l]]
                       (let [next [(+ dx x) (+ dy y)]
                             next-l (or (get acc next) (inc l))]

                         (assoc! acc next next-l)
                         (conj next next-l)))

                     [x y l]))
              last-point (last points)
              point-map (apply hash-map (map #(into [] butlast %)))]

             (recur acc
                    (nth last-point 0) (nth last-point 1) (nth last-point 2)
                    (drop 1 line))))))

(def points1 (line-to-points (.split "U7,R6,D4,L3" ",")))
(def points2 (line-to-points line2))

(set/intersection (set (keys points1))
                  (set (keys points2)))

