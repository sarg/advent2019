(ns advent2019.day11
  (:require [clojure.java.io :as io]
            [advent2019.intcode :refer [intcode-run halt?]]))

(def data
  (with-open [rdr (io/reader (io/resource "day11.in"))]
    (into [] (map #(Long/parseLong %) (.split (.readLine rdr) ",")))))

;; The Intcode program will serve as the brain of the robot. The program uses
;; input instructions to access the robot's camera: provide 0 if the robot is
;; over a black panel or 1 if the robot is over a white panel. Then, the program
;; will output two values:

;; First, it will output a value indicating the color to paint the panel the
;; robot is over: 0 means to paint the panel black, and 1 means to paint the
;; panel white. Second, it will output a value indicating the direction the
;; robot should turn: 0 means it should turn left 90 degrees, and 1 means it
;; should turn right 90 degrees.

;; After the robot turns, it should always move forward exactly one panel. The
;; robot starts facing up.

;; Before you deploy the robot, you should probably have an estimate of the area
;; it will cover: specifically, you need to know the number of panels it paints
;; at least once, regardless of color. In the example above, the robot painted 6
;; panels at least once. (It painted its starting panel twice, but that panel is
;; still only counted once; it also never painted the panel it ended on.)


(defn move [[x y] dir]
  ;; 0 u, 1 r, 2 d, 3 l
  (case dir
    0 [x (dec y)]
    1 [(inc x) y]
    2 [x (inc y)]
    3 [(dec x) y]))

(defn solution [data]
  (loop [grid (transient {})
         code data
         code-state {} 
         me [0 0]
         dir 0
         i 0]

    (let [color-run
          ;; first run with color under robot as input
          (intcode-run code (assoc code-state :input [(get grid me 0)]))

          ;; and get color to paint current panel
          color
          (get-in color-run [1 :output])

          ;; then run another time to get direction to turn
          turn-run
          (apply intcode-run color-run)

          ;; and it'll be 0 for left and 1 for right
          turn
          (-> (get-in turn-run [1 :output])
              (zero?)
              (if -1 1))

          new-dir
          (mod (+ dir turn) 4)]

      (if (halt? turn-run) ;; (> i 5)
        (persistent! grid)
      
        (recur (assoc! grid me color)
               (first turn-run)
               (last turn-run)
               (move me new-dir)
               new-dir
               (inc i))))))

(count (solution data))
