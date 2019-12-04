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
  (loop [acc (atom (transient {}))
         x 0 y 0 l 0
         line line]
    (if (empty? line) (persistent! @acc)
        
        (let [dir (first line)
              [dx dy] (get delta (first dir))
              len (inc (Integer/parseInt (subs dir 1)))

              points
              (take len
                    (iterate
                     (fn [[x y l]]
                       (let [next [(+ dx x) (+ dy y)]
                             next-l (or (get acc next) (inc l))]

                         (swap! acc #(assoc! % next next-l))
                         (conj next next-l)))

                     [x y l]))
              last-point (last points)]
          
          (recur acc
                 (nth last-point 0) (nth last-point 1) (nth last-point 2)
                 (drop 1 line))))))

(def points1 (line-to-points line1))
(def points2 (line-to-points line2))
(def intersections (set/intersection (set (keys points1))
                                     (set (keys points2))))

(defn manhattan [[x y]]
  (+ (Math/abs y) (Math/abs x)))

(first (sort (map manhattan intersections)))
(first (sort (map (fn [v]
                    (let [l1 (get points1 v)
                          l2 (get points2 v)]
                      (+ l1 l2)))
                  intersections)))
