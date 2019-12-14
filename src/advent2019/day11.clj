(ns advent2019.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [quil.core :as q]
            [quil.middleware :as m]
            [advent2019.intcode :refer [intcode-run halt? code-to-map]]))

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

(defn solution [data init]
  (loop [state (init-state data init)]
    
    (if (= 'halt (get-in state [:state :state]))
      (:grid state)
      (recur (step state)))))

(assert (= 1934 (count (solution data 0))))

(defn init-state [data i]
  {:grid {[0 0] i}
   :code (code-to-map data)
   :state {}
   :me [0 0]
   :dir 0})

(defn step [{:keys [code state grid me dir halt]}]
  (when (and halt (= 'halt (:state state)))
        (q/exit))

  (let [color-run
        ;; first run with color under robot as input
        (intcode-run code (assoc state :input [(get grid me 0)]))

        ;; and get color to paint current panel
        color
        (get-in color-run [1 :output])

        ;; then run another time to get direction to turn
        turn-run
        (apply intcode-run color-run)

        ;; and it'll be 0 for counter-clockwise and 1 for clockwise
        turn
        (get-in turn-run [1 :output])

        new-dir
        (-> (if (zero? turn) -1 1)
            (+ dir)
            (mod 4))]

    {:grid (assoc grid me color)
     :code (first turn-run)
     :state (last turn-run)
     :me (move me new-dir)
     :dir new-dir}))

(defn draw [{:keys [grid me]}]
  (q/background 255)
  (q/fill 0)

  (doseq [tile grid
          :let [[[x y] c] tile]
          :when (= 1 c)]

    (q/ellipse (* x 5) (* y 5) 4 4))

  (let [[x y] me]
    (q/fill 0 255 0)
    (q/ellipse (* x 5) (* y 5) 4 4)))

(q/defsketch solution-sketch
  :size [220 50]
  :draw (fn [& args]
          (q/translate 10 10)
          (apply draw args))
  :setup (fn [] (q/frame-rate 25) (init-state data 1))
  :update step
  :middleware [m/fun-mode])

(q/defsketch bonus-sketch
  :size [500 500]
  :draw (fn [& args]
          (q/translate (/ (q/width) 2) (/ (q/height) 2))
          (apply draw args))
  :setup (fn [] (q/frame-rate 100) (init-state data 0))
  :update step
  :middleware [m/fun-mode])
