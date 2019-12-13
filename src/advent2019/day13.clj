(ns advent2019.day13
  (:require [clojure.java.io :as io]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.combinatorics :as cmb]
            [clojure.math.numeric-tower :refer [gcd]]
            [advent2019.intcode :refer [intcode-run halt? code-to-map]]
            [criterium.core :refer [bench with-progress-reporting quick-bench]]
            [clojure.set :as s]))

(def data
  (with-open [rdr (io/reader (io/resource "day13.in"))]
    (into [] (map #(Long/parseLong %) (.split (.readLine rdr) ",")))))


;; The arcade cabinet runs Intcode software like the game the Elves sent (your
;; puzzle input). It has a primitive screen capable of drawing square tiles on a
;; grid. The software draws tiles to the screen with output instructions: every
;; three output instructions specify the x position (distance from the left), y
;; position (distance from the top), and tile id. The tile id is interpreted as
;; follows:

;; 0 is an empty tile. No game object appears in this tile.
;; 1 is a wall tile. Walls are indestructible barriers.
;; 2 is a block tile. Blocks can be broken by the ball.
;; 3 is a horizontal paddle tile. The paddle is indestructible.
;; 4 is a ball tile. The ball moves diagonally and bounces off objects.

(defn collect-output [code state]
  (loop [output (transient [])
         code code
         state (assoc state :state 'running)]
    
    (let [out-val (:output state)
          new-out (if out-val (conj! output out-val) output)]

      (if (get #{'halt 'waiting-input} (:state state))
        [code state (persistent! new-out)]
        (let [[new-code new-state] (intcode-run code (assoc state :output nil))]
          (recur new-out new-code new-state))))))

(def solution
  (->>
   (last (collect-output (code-to-map data) {}))
   (partition 3)
   (filter #(= 2 (last %)))
   (count)))

;; The game didn't run because you didn't put in any quarters. Unfortunately,
;; you did not bring any quarters. Memory address 0 represents the number of
;; quarters that have been inserted; set it to 2 to play for free.
;;

(defn update-screen [screen out]
  (->>
   out
   (partition 3)
   (map (fn [[x y t]] [[x y] t]))
   (into screen)))

(defn update-state [{:keys [code state screen input] :as params}]
  (let [[[bx0 by0] _] (first (filter #(= 4 (last %)) screen))

        [new-code new-state out] (collect-output code (assoc state :input [input]))
        new-screen (update-screen screen out)

        [[bx by] _] (first (filter #(= 4 (last %)) new-screen))
        [[px py] _] (first (filter #(= 3 (last %)) new-screen))]

    (assoc params
           :code new-code
           :state new-state
           :screen new-screen
           :input (if (< by by0) (compare bx bx0)
                      (compare (+ bx -1 (* (compare bx bx0) (- py by)))
                               px)))))

(defn get-score [state]
    (get-in state [:screen [-1 0]]))

(loop [state init-state max-score 0 i 0]
  ;; (when (> 0 max-score) (println max-score))
  (if ;; (= 'halt (get-in state [:state :state]))
      (> 1000 max-score)
    [max-score i]
    (recur (update-state state) (max max-score (get-score state)) (inc i))))

(defn draw [{:keys [screen]}]
  (q/fill 0)
  (q/rect 0 0 (q/width) (q/height))

  (q/fill 0 255 0)
  (run! (fn [[[x y] t]]
          (if (= x -1)
            (q/text (str t) 0 130)
            (when (> t 0)
              (case t
                1 (q/fill 255 255 255)
                2 (q/fill 100 100 100)
                3 (q/fill 0 255 255)
                4 (q/fill 255 0 0))
              (q/ellipse (* 5 x) (* 5 y) 5 5))))
        screen))

(def init-state
  (let [with-coin (assoc (code-to-map data) 0 2)
        [code state screen-out] (collect-output with-coin {})
        screen (update-screen {} screen-out)]
    {:code code
     :input 0
     :state state
     :screen screen}))

(defn setup []
  (q/frame-rate 20)
  init-state)

(q/defsketch arcade
  :title "Arcade"
  ;; :rendered :p2d
  :size [210 150]
  :setup setup
  :update update-state
  :middleware [m/fun-mode]
  :draw draw)
;; The arcade cabinet has a joystick that can move left and right. The software
;; reads the position of the joystick with input instructions:

;; If the joystick is in the neutral position, provide 0.
;; If the joystick is tilted to the left, provide -1.
;; If the joystick is tilted to the right, provide 1.


;; When three output instructions specify X=-1, Y=0, the third output
;; instruction is not a tile; the value instead specifies the new score to show
;; in the segment display.

;; Beat the game by breaking all the blocks. What is your score after the last block is broken?
