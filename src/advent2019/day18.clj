(ns advent2019.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [flatland.ordered.set :refer [ordered-set]]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [quil.core :as q]
            [quil.middleware :as m]))

(def data
  (with-open [rdr (io/reader (io/resource "day18.in"))]
    (into [] (line-seq rdr))))

;; How many steps is the shortest path that collects all of the keys?
;; Key is lowercase letters, doors uppercase, me is @.

(defn isKey [c]
  (and (Character/isLetter c) (Character/isLowerCase c)))

(defn isDoor [c]
  (and (Character/isLetter c) (Character/isLowerCase c)))

(defn row-to-entries [y row]
  (->> row
       (keep-indexed (fn [x el] (when-not (= \# el) [[x y] el])))
       (apply concat)))

(def level
  (apply hash-map (apply concat (map-indexed row-to-entries data))))
  
(def me (first (keep (fn [[k v]] (when (= \@ v) k)) level)))

(defn move [[x y] dir]
  ;; 1 n, 2 s, 3 w, 4 e
  (case dir
    1 [x (dec y)]
    2 [x (inc y)]

    3 [(dec x) y]
    4 [(inc x) y]))

(defn around [[me keys]]
  (->> (range 1 5)
       (map (partial move me))
       (map #(vector % keys))))

(defn around-me [me]
  (->> (range 1 5)
       (map (partial move me))))

(defn find-nearest []
  (let [level (assoc level me \.)]
    (loop [edge [[me #{}]]
           paths #{me}
           all-keys []]
      
      (if (empty? edge) (map (fn [[pos deps]] [(level pos) deps]) all-keys)

          (let [{:keys [t-doors t-empty t-keys]}
                (->> edge
                     (map around)
                     (apply concat)
                     (filter (complement (comp paths first)))
                     (group-by
                      (fn [[pos deps]]
                        (let [tile (level pos)]
                          (cond
                            (nil? tile) :walls
                            (= \. tile) :t-empty
                            (Character/isUpperCase tile) :t-doors
                            (Character/isLowerCase tile) :t-keys)))))

                next-edge (into [] (concat t-empty
                                           (map (fn [[pos deps]] [pos (conj deps (level pos))]) t-keys)
                                           (map (fn [[pos deps]]
                                                  [pos (conj deps (Character/toLowerCase (level pos)))])
                                                t-doors)))]
            
            (recur next-edge
                   (into paths (map first next-edge))
                   (into all-keys t-keys)))))))

(def deps (find-nearest))

(defn choices [open]
  (->>
   deps
   (filter (fn [[k d]] (and (not (open k)) (every? open d))))
   (map first)
   (into [])))

;; doesn't make sense to go for key \a if B is closed
;; ..@...a...B..A

(defn distance [a b]
  (loop [edge [a]
         paths #{a}
         dist 0]
    
    ;; (if (= i 13)
    ;;   (println [edge paths all-doors all-keys]))
    (if (paths b) dist

        (let [around-edge
              (->> edge
                   (map around-me)
                   (apply concat)
                   (filter (complement paths))
                   (filter level))]
        
          (recur around-edge
                 (into paths around-edge)
                 (inc dist))))))

(defn get-cost [a b]
  (let [ac (first (filter #(= a (second %)) level))
        bc (first (filter #(= b (second %)) level))]

    (distance (first ac) (first bc))))

(def get-cost-m (memoize get-cost))

(defn all-traversals [open left cost min-cost]
  (cond
    (> cost min-cost) nil
    (empty? left) [cost (apply str open)]
    :else
    (let [at (last open)]
      (loop [c (sort-by (partial get-cost-m at) (choices open))
             min-cost-in-loop min-cost
             min-path []]
        (if (empty? c)
          (do
            (when (< min-cost-in-loop min-cost) [min-cost-in-loop min-path]))
          
          (let [choice (first c)
                [nmc nmp] (all-traversals (conj open choice) (disj left choice)
                                          (+ cost (get-cost-m choice at)) min-cost-in-loop)
                better? (< (or nmc Integer/MAX_VALUE) min-cost-in-loop)]
            (when better? (println [nmc nmp]))
            (recur (rest c) (if better? nmc min-cost-in-loop) (if better? nmp min-path))))))))

(do (println "---")
    (all-traversals (ordered-set \@)
                    (into #{} (map first deps))
                    0 4172))

(defn greedy-traverse []
  (loop [open (ordered-set \@)
         total 0]
    (if (> (count open) (count deps))
      [total open]

      (let [at (last open)
            c (first (sort-by (partial get-cost-m at) (choices open)))]
        (recur (conj open c) (+ total (get-cost-m at c)))))))


;; 4672 too high
;; 4172 too high
(println "---" )
(run! println
      (flatten (map (fn [[f t]] (map #(str f " -> " %)
                                     (if (empty? t) ["me"] t))) deps)))
