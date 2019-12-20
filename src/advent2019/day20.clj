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

(def width (apply max-key identity (map count data)))
(def height (count data))

(defn row-to-entries [y row]
  (->> row
       (keep-indexed (fn [x el] (when-not (#{\# \space} el) [[x y] el])))
       (apply concat)))

(def level
  (apply hash-map (apply concat (map-indexed row-to-entries data))))

(defn move [[x y :as in] dir]
  ;; 1 n, 2 s, 3 w, 4 e
  (case dir
    1 (assoc in 0 x 1 (dec y))
    2 (assoc in 0 x 1 (inc y))

    3 (assoc in 0 (dec x) 1 y)
    4 (assoc in 0 (inc x) 1 y)))

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

(def level-without-labels
  (filter (comp (partial = \.) second) level))

(def level-with-portals 
  (->>
   portals
   (map (fn [[k vl]] (map #(vector % k) vl)))
   (apply concat)
   (concat level-without-labels)
   (into {})))

(defn jump [tname me]
  (first (filter (partial not= me) (portals tname))))

(defn around-or-jump [jump-fn [me v]]
  (into []
        (concat
         (map (partial move me) (range 1 5))
         (if (not= \. v)
           (jump-fn me (jump v (take 2 me)))
           []))))

(defn reconstruct-path [grid dist me jump-fn]
  (loop [dist (dec dist)
         me me
         acc (transient [])]
    (if (neg? dist)
      (into [] (reverse (persistent! acc)))

      (let [m (first (filter
                      (fn [pos] (= dist (grid pos)))
                      (around-or-jump jump-fn [me (level-with-portals (take 2 me))])))]
        (recur (dec dist) m (conj! acc m))))))

(def start (conj (first (portals "AA")) 0))
(def end (conj (first (portals "ZZ")) 0))

(def cx (quot width 2))
(def cy (quot height 2))

(defn dist-to-center [[x y]]
  (+ (Math/pow (- cx x) 2) (Math/pow (- cy y) 2)))

(def init-state
  {:edge {start "AA"}
   :paths {start 0}
   :i 1})

(defn next-frame [{:keys [edge paths i fin jump-fn] :as quil-state}]
  (if (or fin (empty? edge) (edge end))
    (if fin quil-state
        (assoc quil-state :fin (reconstruct-path paths (dec i) end jump-fn)))

    (let [around-edge
          (->> edge
               (map (partial around-or-jump jump-fn))
               (apply concat)
               (filter (complement paths))
               (map #(vector % (level-with-portals (take 2 %))))
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
    :size [(* 5 width) (+ 20 (* 5 height))]
    :setup (fn []
             (q/frame-rate 100)
             (q/fill 0)
             from)
    
    :update next-frame
    :middleware [m/fun-mode]
    :draw draw))

(defn solution [jump-fn]
  (->> (assoc init-state :jump-fn jump-fn)
       (iterate next-frame)
       (drop-while (complement :fin))
       (first)
       (:i)
       (dec)))

(defn part1-jump [[_ _ l] to]
  (if to [(conj to l)] []))

(defn part2-jump [[_ _ l :as from] to]
  (if-not to
    []
    (let [new-l (if (> (dist-to-center from) (dist-to-center to))
                  (dec l) (inc l))]
      
      (if (neg? new-l)
        []
        [(conj to new-l)]))))

(solution part1-jump)
(solution part2-jump)
