(ns advent2019.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

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
         prev-step 0
         x 0 y 0
         dir-seq dir-seq]
    
    (if (empty? dir-seq)
      (persistent! acc)

      (let [dir (first dir-seq)
            delta (dir-to-delta dir)
            len (Math/abs (reduce + delta))
            step (+ prev-step len)
            new-x (+ x (first delta))
            new-y (+ y (last delta))]

        (recur (conj! acc [[x y] [new-x new-y] step])
               step
               new-x new-y
               (drop 1 dir-seq))))))

(defn adjust-step [x y11 s1 l1])

(defn between? [x a b]
  (and (>= x (min a b)) (<= x (max a b))))

(defn intersect? [[[x11 y11] [x12 y12] s1]
                  [[x21 y21] [x22 y22] s2]]

  (when (and (between? x21 x11 x12) 
             (between? y11 y21 y22))
    [[x21 y11] [(- s1 (Math/abs (- x12 x21)) (Math/abs (- y12 y11)))
                (- s2 (Math/abs (- x22 x21)) (Math/abs (- y22 y11)))]]))

(defn intersections [line1 line2]
  (for [seg1 line1 seg2 line2]
    (let [vert1 (= (first (first seg1)) (first (second seg1)))
          vert2 (= (first (first seg2)) (first (second seg2)))
          both-vert? (= vert1 vert2)
          intersect (and (not both-vert?)
                         (intersect? (if vert1 seg2 seg1)
                                     (if vert1 seg1 seg2)))]

      intersect)))

(defn manhattan [[[x y]]]
  (+ (Math/abs y) (Math/abs x)))

(defn solution [line1 line2]
  (second (sort (map manhattan
                     (filter identity
                             (intersections
                              (line-segments line1)
                              (line-segments line2)))))))

(map manhattan
     (filter identity
             (intersections
              (line-segments (.split "R75,D30,R83,U83,L12,D49,R71,U7,L72" ","))
              (line-segments (.split "U62,R66,U55,R34,D71,R55,D58,R83", ",")))))

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


(defn step-metric [[_ [s1 s2]]]
  (+ s1 s2))

(defn bonus [line1 line2]
  (second (sort (map step-metric
                     (filter identity
                             (intersections
                              (line-segments line1)
                              (line-segments line2)))))))

(defn line-to-path [line color]
  (str 
   "<path fill='none' stroke-width='5' stroke='" color "' d='M0 0 "
   (->> line
        (line-segments)
        (map (fn [[_ [x y]]] (format "L%d %d " x (- y))))
        (s/join))
   "'/>\n"))

(defn line-to-svg [file line1 line2]
  (with-open [wrt (io/writer file)]
    (.write wrt
            (str "<?xml version='1.0' encoding='UTF-8'?>\n"
                 "<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>\n"
                 "<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' "
                 "width='100%' height='100%' viewBox='-10000 -10000 20000 20000'>\n"
                 "<circle cx='0' cy='0' r='5' stroke='black' fill='red' />"

                 (line-to-path line1 "red")
                 (line-to-path line2 "blue")
    
                 "</svg>"))))

(line-to-svg "tst.svg"
             line1 line2)

(map step-metric 
     (filter identity
             (intersections
              (line-segments (.split "U7,R6,D4,L3" ","))
              (line-segments (.split "R8,U5,L5,D3", ",")))))

(map step-metric 
      (filter identity
              (intersections
               (line-segments line1)
               (line-segments line2))))

(assert
 (= 30 (bonus 
         (.split "U7,R6,D4,L3" ",")
         (.split "R8,U5,L5,D3", ","))))

(assert
 (= 610 (bonus 
         (.split "R75,D30,R83,U83,L12,D49,R71,U7,L72" ",")
         (.split "U62,R66,U55,R34,D71,R55,D58,R83", ","))))

(assert
 (= 410 (bonus
         (.split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" ",")
         (.split "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" ","))))

(bonus line1 line2)
