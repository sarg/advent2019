(ns advent2019.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [flatland.ordered.set :refer [ordered-set]]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [quil.core :as q]
            [quil.middleware :as m]))

(def data
  (with-open [rdr (io/reader (io/resource "day20.in"))]
    (into [] (line-seq rdr))))

(defn row-to-entries [y row]
  (->> row
       (keep-indexed (fn [x el] (when-not (#{\# \space} el) [[x y] el])))
       (apply concat)))

(def level
  (apply hash-map (apply concat (map-indexed row-to-entries data))))

(defn move [[x y] dir]
  ;; 1 n, 2 s, 3 w, 4 e
  (case dir
    1 [x (dec y)]
    2 [x (inc y)]

    3 [(dec x) y]
    4 [(inc x) y]))


(defn find-around [me pred]
  (->> (range 1 5)
       (map (partial move me))
       (map #(vector % (level %)))
       (filter (every-pred (comp identity last) (comp pred last)))
       (first)))

(defn get-label [l1 l2]
  (->> [l1 l2]
       (sort-by first)
       (map second)
       (apply str)))

(defn detect-portal [[pos _ :as l1]]
  (let [[dot _] (find-around pos #(= \. %))
        l2 (find-around pos #(not= \. %))]

    (when (and dot l2) [dot (get-label l1 l2)])))

(def portals
  (->>
   level
   (filter #(not= \. (second %)))
   (keep detect-portal)
   (group-by second)
   (map (fn [[k vs]] [k (into [] (map first vs))]))
   (into {})))

(def level-without-portals
  (filter (comp (partial = \.) second) level))

(def level-with-portals 
  (->>
   portals
   (map (fn [[k vl]] (map #(vector % k) vl)))
   (apply concat)
   (concat level-without-portals)
   (into {})))

(defn jump [tname me]
  (into (filter (partial not= me) (portals tname))))

(defn around-or-jump [[me v]]
  (into []
        (concat
         (map (partial move me) (range 1 5))
         (if (not= \. v) (jump v me) []))))

(defn found? [what where]
  (->> where
       (filter (comp (partial = what) second))
       (first)))

(defn reconstruct-path [grid dist me]
  (loop [dist (dec dist)
         me me
         acc (transient [])]
    (if (neg? dist)
      (into [] (reverse (persistent! acc)))

      (let [m (first (filter
                      (fn [pos] (= dist (grid pos)))
                      (around-or-jump [me (level-with-portals me)])))]
        (recur (dec dist) m (conj! acc m))))))

(def start (first (portals "AA")))
(def end (first (portals "ZZ")))

(def init-state
  {:edge {start "AA"}
   :paths {start 0}
   :i 1})

(defn next-frame [{:keys [edge paths i fin] :as quil-state}]
  (if (or fin (empty? edge) (edge end))
    (if fin quil-state
        (assoc quil-state :fin (reconstruct-path paths (dec i) end)))

    (let [around-edge
          (->> edge
               (map around-or-jump)
               (apply concat)
               (filter (complement paths))
               (map #(vector % (level-with-portals %)))
               (filter (comp identity last))
               (into {}))]
      
      (assoc quil-state
             :edge around-edge
             :paths (merge-with min paths
                                (into {} (map #(vector (first %) i) around-edge)))
             :i (inc i)))))

(defn draw [{:keys [paths edge i fin] :as quil-state}]
  (q/background 255)
  (q/rect-mode :center)

  ;(println quil-state)

  (run! (fn [[[x y] v]]
          (if (= \. v)
            (q/ellipse (* x 5) (* y 5) 2 2)
            (q/with-stroke [255 0 0]
              (q/ellipse (* x 5) (* y 5) 4 4))))
        level-with-portals)

  (if fin
    (do
      (run!
       (fn [[x y]]
         (q/with-stroke [100 100 50]
           (q/ellipse (* x 5) (* y 5) 4 4)))
       fin)

      (q/with-stroke [0]
        (q/text (str (count fin)) 10 (- (q/height) 20))))

    (run!
     (fn [[[x y] v]]
       (q/with-stroke [0 255 0]
         (q/ellipse (* x 5) (* y 5) 4 4)))
     edge))

  (run! (fn [[x y]]
          (q/with-stroke [0 0 255]
            (q/rect (* x 5) (* y 5) 5 5)))
        [start end]))

(defn start-sketch [from]
  (q/defsketch maze
    :title "maze"
    :size [(* 5 (apply max-key identity (map count data)))
           (+ 20 (* 5 (count data)))]
    :setup (fn []
             (q/frame-rate 100)
             (q/fill 0)
             from)
    
    :update next-frame
    :middleware [m/fun-mode]
    :draw draw))

(def end-state 
  (->> init-state
       (iterate next-frame)
       (drop-while (complement :fin))
       (first)))

;(start-sketch end-state)
