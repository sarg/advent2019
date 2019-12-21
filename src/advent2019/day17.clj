(ns advent2019.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [quil.core :as q]
            [quil.middleware :as m]
            [advent2019.intcode :refer [intcode-run halt? code-to-map]]))


(def data
  (with-open [rdr (io/reader (io/resource "day17.in"))]
    (into [] (map #(Long/parseLong %) (.split (.readLine rdr) ",")))))

(defn collect-output [code state & {:keys [until]}]
  (loop [output (transient [])
         code code
         state (assoc state :state 'running)]
    
    (let [out-val (:output state)
          new-out (if out-val (conj! output out-val) output)]

      (if (or (get #{'halt 'waiting-input} (:state state))
              (when until (until new-out)))
        [code state (persistent! new-out)]
        (let [[new-code new-state] (intcode-run code (assoc state :output nil))]
          (recur new-out new-code new-state))))))

(defn index-of
  [pred coll]
  (ffirst (filter (comp (partial = pred) second) (map-indexed list coll))))

(def level-map
  (last (collect-output (code-to-map data) {})))

(def intersections
  (let [w (inc (index-of (int \newline) level-map))
        h (/ (dec (count level-map)) w)]

    (filter identity
            (for [y (range 1 (dec h))
                  x (range 1 (- w 2))
                  :let [p (+ x (* y w))
                        pv (level-map p)]
                  :when (= (int \#) pv)]
    
              (let [u (- p w)
                    b (+ p w)
                    l (- p 1)
                    r (+ p 1)]

                ;; #break ^{:break/when (= [12 6] [x y])}
                (when (apply = pv (map level-map [u b l r]))
                  [x y]))))))

(defn output-to-str [level-map] (apply str (map char level-map)))
(def level-str (output-to-str level-map))
(def part1 (reduce + (map (fn [[x y]] (* x y)) intersections)))

;; The main routine may only call the movement functions: A, B, or C.
;; Supply the movement functions to use as ASCII text, separating them with
;; commas (,, ASCII code 44), and ending the list with a newline (ASCII code
;; 10). For example, to call A twice, then alternate between B and C three
;; times, provide the string A,A,B,C,B,C,B,C and then a newline.

;; Movement functions may use L to turn left, R to turn right, or a number to
;; move forward that many units. Movement functions may not call other movement
;; functions. Again, separate the actions with commas and end the list with a
;; newline. For example, to move forward 10 units, turn left, move forward 8
;; units, turn right, and finally move forward 6 units, provide the string
;; 10,L,8,R,6 and then a newline.
;; 
;; Functions may each contain at most 20 characters, not counting the newline.

;; Finally, you will be asked whether you want to see a continuous video feed; provide either y or n and a newline.

;; Once it finishes the programmed set of movements, assuming it hasn't drifted
;; off into space, the cleaning robot will return to its docking station and
;; report the amount of space dust it collected as a large, non-ASCII value in a
;; single output instruction.

(defn prg-to-input [& s-seq]
  (into [] (map int (s/join "\n" (concat s-seq [""])))))

(defn find-way [level-map]
  (let [max-pos (dec (count level-map))
        w (inc (index-of (int \newline) level-map))
        h (/ max-pos w)
        me (index-of (int \^) level-map)

        ;; u -w, d +w, l -1, r +1
        turns [(- w) 1 w -1]
        do-turn (fn [d to]
                  (-> d
                      (index-of turns)
                      (+ to)
                      (mod (count turns))
                      (turns)))
        safe-look #(get level-map % (int \newline))]

    (loop [path []
           me me
           dir (- w)
           len 0]
      
      (let [new-pos (+ me dir)
            look-at (safe-look new-pos)
            on-wall (= (int \newline) look-at)]

        (if (or on-wall (= (int \.) look-at))
          (let [left-turn (do-turn dir -1)
                right-turn (do-turn dir 1)

                at-left (safe-look (+ me left-turn))
                at-right (safe-look (+ me right-turn))

                left? (= (int \#) at-left)
                right? (= (int \#) at-right)]
            
            (if (or left? right?)
              (recur (conj path len (if left? "L" "R"))
                     me (if left? left-turn right-turn) 0)
              (conj path len)))
          ;; continue path
          (recur path new-pos dir (inc len)))))))

(def path-out (s/join "," (drop 1 (find-way level-map))))

(defn last! [n a]
  (let [l (count a)]
    (for [i (range (- l n) l)] 
      (get a i))))

(defn next-frame [{:keys [code state level-map] :as quil-state}]
  (let [[new-code new-state output]
        (collect-output code state
                        :until #(apply = (int \newline) (last! 2 %)))]

    (when (and level-map
               (or (< (count level-map) 100)
                   (< (count output) 100))) (q/delay-frame 2000))

    (assoc quil-state
           :code new-code
           :state (assoc new-state :output nil)
           :level-map output)))

(defn prg [& [video-feed]]
  (prg-to-input
   "A,B,A,C,B,C,B,C,A,C"
   "R,12,L,6,R,12"
   "L,8,L,6,L,10"
   "R,12,L,10,L,6,R,10"
   (if video-feed "y" "n")))

(def video nil)
(def init-state
  ;; Force the vacuum robot to wake up by changing the value in your ASCII program at address 0 from 1 to 2.
  {:code (code-to-map (assoc data 0 2))
   :state {:input (prg 'with-video)}})

;; A = R,12,L,6,R,12,
;; B = L,8,L,6,L,10,
;; C = R,12,L,10,L,6,R,10,
;;   | -------------------- 
;; A | R,12,L,6,R,12,
;; B | L,8,L,6,L,10,
;; A | R,12,L,6,R,12,
;; C | R,12,L,10,L,6,R,10,
;; B | L,8,L,6,L,10,
;; C | R,12,L,10,L,6,R,10,
;; B | L,8,L,6,L,10,
;; C | R,12,L,10,L,6,R,10,
;; A | R,12,L,6,R,12,
;; C | R,12,L,10,L,6,R,10

(def part2
  (last (last (collect-output
               (code-to-map (assoc data 0 2))
               {:input (prg)}))))


(defn setup []
  (q/frame-rate 30)
  (if video
    (let [videoExport (com.hamoid.VideoExport. (quil.applet/current-applet) video)]
      (.startMovie videoExport)
      (assoc init-state :videoExport videoExport))
    init-state))


(defn draw [{:keys [level-map]}]
  (q/background 255)
  (q/rect-mode :center)
  (q/stroke 0)

  (if (< (count level-map) 1000) 
    (q/with-fill [0]
      (q/text (output-to-str level-map) 10 10))
    
    (let [w (inc (index-of (int \newline) level-map))
          h (/ (dec (count level-map)) w)]
      
      (doseq [y (range 0 h)
              x (range 0 (- w 1))
              :let [p (+ x (* y w))
                    pv (level-map p)]]
        
        (case (char pv)
          \#
          (do (q/ellipse (* x 5) (* y 5) 4 4))

          \.
          nil
          ;; (do (q/ellipse (* x 5) (* y 5) 1 1))

          (q/with-fill [255 0 0]
            (q/rect (* x 5) (* y 5) 4 4)))))))

(defn start []
  (q/defsketch maze
    :title "maze"
    :size [300 400]
    :setup setup

    :on-close (fn [{:keys [videoExport]}] (when videoExport (.endMovie videoExport)))

    :update next-frame
    :middleware [m/fun-mode]
    :draw (fn [& args]
            (q/translate 20 20)
            (apply draw args))))
