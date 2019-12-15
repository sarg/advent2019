(ns advent2019.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [quil.core :as q]
            [quil.middleware :as m]
            [advent2019.intcode :refer [intcode-run halt? code-to-map]]))

(def video "resources/day15.mp4")

(def data
  (with-open [rdr (io/reader (io/resource "day15.in"))]
    (into [] (map #(Long/parseLong %) (.split (.readLine rdr) ",")))))

(def init-state
  {:grid {[0 0] 1}
   :me [0 0]
   :code data
   :state {}})

;; Intcode program:
;; Accept a movement command via an input instruction.
;; north (1), south (2), west (3), and east (4).
;; 
;; Send the movement command to the repair droid.
;; Wait for the repair droid to finish the movement operation.
;; 
;; Report on the status of the repair droid via an output instruction.
;; 0: The repair droid hit a wall. Its position has not changed.
;; 1: The repair droid has moved one step in the requested direction.
;; 2: The repair droid has moved one step in the requested direction; its new position is the location of the oxygen system.
;;
;; What is the fewest number of movement commands required to move the repair
;; droid from its starting position to the location of the oxygen system?

(defn move [[x y] dir]
  ;; 1 n, 2 s, 3 w, 4 e
  (case dir
    1 [x (dec y)]
    2 [x (inc y)]

    3 [(dec x) y]
    4 [(inc x) y]))

(defn around [me]
  (map (partial move me) (range 1 5)))

(defn around-with-dir [me]
  (map (fn [dir] [(move me dir) dir]) (range 1 5)))

(defn distance [[cx cy] [x y]]
  (+ (Math/abs (- cx x)) (Math/abs (- cy y))))

(def reverse-dir {1 2, 2 1, 3 4, 4 3})

(defn reconstruct-path [grid dist me]
  (loop [dist (dec dist)
         me me
         acc (transient [])]
    (if (neg? dist)
      (into [] (reverse (persistent! acc)))

      (let [[m dir] (first (filter
                            (fn [[pos _]] (= dist (grid pos)))
                            (around-with-dir me)))]
        (recur (dec dist) m
               (conj! acc (reverse-dir dir)))))))

;; (defn draw-test []
;;   (q/background 255)

;;   (q/fill 0)
;;   (q/text (str (:i @debug-atom)) 100 100 )
;;   (draw @debug-atom)

;;   (q/fill 255 0 0)
;;   (doseq [i (:edge @debug-atom)
;;           :let [[x y] i]]
;;     (q/ellipse (* x 5) (* y 5) 4 4))

;;   (let [[x y] (:me @debug-atom)]
;;     (q/fill 0 255 0)
;;     (q/ellipse (* x 5) (* y 5) 4 4)))

;; (defn debug-step [grid me edge i]
;;   (reset! debug-atom
;;           {:grid grid
;;            :me me
;;            :edge edge
;;            :i i}))

(defn find-nearest [search-for me grid]
  (loop [edge [me]
         paths {me 0}
         i 1]

    ;; (when (and debug-atom edge)
    ;;   (debug-step grid me edge i)
    ;;   (.redraw test-sketch))

    (if (not edge)
      {:dist (- i 2)}

      (let [around-edge
            (->> edge
                 (map around)
                 (apply concat)
                 (filter (complement paths))
                 (group-by grid))

            unexplored (get around-edge search-for)
            next-edge (get around-edge 1)]

        (if unexplored
          (let [target
                (->> unexplored
                     (sort-by (partial distance me))
                     (first))]
            {:target target
             :path (reconstruct-path paths i target)
             :dist i})

          (recur next-edge
                 (merge-with min paths
                             (into {} (map #(vector % i) next-edge)))
                 (inc i)))))))

(defn select-new-target [{:keys [grid me state found]}]
  (let [{:keys [target path]} (find-nearest nil me grid)]
    (if target
      {:nearest target
       :state (assoc state :input path)}
      {:oxygen #{(:target found)}
       :time 0})))

(defn circulate-oxygen [{:keys [grid oxygen time]}]
  (let [next-oxygen
        (->> oxygen
             (map around)
             (apply concat)
             (filter #(= 1 (grid %))))]
    
    (if (empty? next-oxygen)
      {:finished true}

      {:oxygen (into oxygen next-oxygen)
       :time (inc time)
       :grid (into grid (map #(vector % 3) next-oxygen))})))

(defn update-state [{:keys [grid me code state found oxygen finished] :as quil-state}]
  (cond
    finished
    quil-state

    oxygen
    (conj quil-state (circulate-oxygen quil-state))

    :default
    (if-let [dir (first (:input state))]
      (let [[new-code new-state]
            (intcode-run code state)

            try-pos (move me dir)
            status (:output new-state)

            new-pos (if (zero? status) me try-pos)
            new-grid (assoc grid try-pos status)]

        (assoc quil-state
               :found (or found
                          (when (= 2 status)
                            (select-keys (find-nearest 2 [0 0] new-grid) [:dist :target])))
               :code new-code
               :state new-state
               :grid new-grid
               :me new-pos))

      (conj quil-state (select-new-target quil-state)))))

(defn draw [{:keys [grid me nearest found oxygen time videoExport]}]
  (q/background 255)
  (q/rect-mode :center)

  (doseq [tile grid
          :let [[[x y] c] tile]]
    (case c
      0 (q/ellipse (* x 5) (* y 5) 4 4)
      1 (q/ellipse (* x 5) (* y 5) 1 1)
      2 (q/with-fill [255 100 100]
          (q/ellipse (* x 5) (* y 5) 5 5))
      3 (q/with-fill [0 0 255]
          (q/rect (* x 5) (* y 5) 5 5))))

  (when found
    (q/with-fill [0]
      (q/text (str "Distance: " (:dist found)) -100 120)))

  (when time
    (q/with-fill [0]
      (q/text (str "Time: " time) -100 140)))

  (when-not oxygen
    (q/with-fill [255 100 100]
      (q/rect 0 0 6 6))

    (when nearest
      (let [[x y] nearest]
        (q/with-fill [255 0 0]
          (q/ellipse (* x 5) (* y 5) 4 4))))

    (let [[x y] me]
      (q/with-fill [0 255 0]
        (q/ellipse (* x 5) (* y 5) 4 4))))

  (when videoExport
    (.saveFrame videoExport)))

(defn setup []
  (q/frame-rate 30)
  (q/fill 0)
  (if video
    (let [videoExport (com.hamoid.VideoExport. (quil.applet/current-applet) video)]
      (.startMovie videoExport)
      (assoc init-state :videoExport videoExport))
    init-state))

(defn handle-key [state event]
  (update-in state [:state :input]
          (fn [input]
            ;; 1 n, 2 s, 3 w, 4 e
            (if-let [dir (case (:raw-key event) \k 1 \j 2 \h 3 \l 4 nil)]
              (if input (conj input dir) [dir])))))

(defn update-state-key [state]
  (if (empty? (:input state)) state
    (update-state state)))

(defn start []
  (q/defsketch maze
    :title "maze"
    :size [300 300]
    :setup setup

    :update update-state

    ;; :update update-state-key
    ;; :key-typed handle-key 
    :on-close (fn [{:keys [videoExport]}] (when videoExport (.endMovie videoExport)))

    :middleware [m/fun-mode]
    :draw (fn [& args]
            (q/translate (/ (q/width) 2) (/ (q/height) 2))
            (apply draw args))))

(defn solution []
  (let [full-map
        (->> init-state
             (update-state)
             (iterate update-state)
             (drop-while (complement :oxygen))
             (first)
             (:grid))

        {oxygen :target oxygen-dist :dist} (find-nearest 2 [0 0] full-map)
        farthest-dist (:dist (find-nearest 3 oxygen full-map))]

    (assert (= 272 oxygen-dist))
    (assert (= 398 farthest-dist))))
