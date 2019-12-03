(ns advent2019.day3
  (:require [clojure.java.io :as io]))

(with-open [rdr (io/reader (io/resource "day3.in"))]
  (def line1 (into [] (-> rdr
                          (.readLine)
                          (.split ","))))

  (def line2 (into [] (-> rdr
                          (.readLine)
                          (.split ",")))))


(defn dir-to-delta [dir]
  (let [d (first dir)
        l (Integer/parseInt (subs dir 1))]
    (case d
      \R [l 0]
      \L [(- l) 0]
      \U [0 l]
      \D [0 (- l)])))

(assert (= (dir-to-delta "R75") [75 0]))
(assert (= (dir-to-delta "D30") [0 -30]))
(assert (= (dir-to-delta "U83") [0 83]))
(assert (= (dir-to-delta "L72") [-72 0]))

(defn seg-order [seg]
  "Sort points in SEG by x"
  (into [] (sort seg)))

(defn line-segments [dir-seq]
  "Transform DIR-SEQ to SEG-SEQ"
  (loop [acc (transient [])
         x 0 y 0
         dir-seq dir-seq]
    
    (if (empty? dir-seq)
      (persistent! acc)

      (let [dir (first dir-seq)
            delta (dir-to-delta dir)
            new-x (+ x (first delta))
            new-y (+ y (last delta))]

        (recur (conj! acc (seg-order [[x y] [new-x new-y]]))
               new-x new-y
               (drop 1 dir-seq))))))

(defn intersect? [[[x11 y11] [x12 y12]]
                  [[x21 y21] [x22 y22]]]

  (when (and (>= x21 x11) (<= x21 x12)
             (>= y11 y21) (<= y11 y22))
    [x21 y11]))

(defn sorted-segments [seq]
  (sort #(compare (first (first %1))
                  (first (first %2))) seq))

(defn intersections [line1 line2]
  (for [seg1 line1 seg2 line2]
    (let [vert1 (= (first (first seg1)) (first (second seg1)))
          vert2 (= (first (first seg2)) (first (second seg2)))
          both-vert? (= vert1 vert2)
          intersect (and (not both-vert?)
                         (intersect? (if vert1 seg2 seg1)
                                     (if vert1 seg1 seg2)))]

      intersect)))

(defn manhattan [[x y]]
  (+ (Math/abs y) (Math/abs x)))

(defn solution [line1 line2]
  (apply min
         (map manhattan
              (remove nil?
                      (intersections
                       (line-segments line1)
                       (line-segments line2))))))

;; Test
(assert
 (= 159 (solution 
         (.split "R75,D30,R83,U83,L12,D49,R71,U7,L72" ",")
         (.split "U62,R66,U55,R34,D71,R55,D58,R83", ","))))

(assert
 (= 135 (solution
         (.split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" ",")
         (.split "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ","))))

;; What is the Manhattan distance from the central port to the closest
;; intersection?

(solution line1 line2)

;; Bonus
;; 
;; To do this, calculate the number of steps each wire takes to reach each
;; intersection; choose the intersection where the sum of both wires' steps is
;; lowest. If a wire visits a position on the grid multiple times, use the steps
;; value from the first time it visits that position when calculating the total
;; value of a specific intersection.


(defn bonus [line1 line2]
  22
  )


(assert
 (= 159 (bonus 
         (.split "R75,D30,R83,U83,L12,D49,R71,U7,L72" ",")
         (.split "U62,R66,U55,R34,D71,R55,D58,R83", ","))))

(assert
 (= 135 (bonus
         (.split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" ",")
         (.split "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ","))))

(bonus line1 line2)
