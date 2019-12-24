(ns advent2019.day18
  (:require [clojure.java.io :as io]
            [flatland.ordered.set :refer [ordered-set]]))

(def data
  (with-open [rdr (io/reader (io/resource "day18.in"))]
    (into [] (line-seq rdr))))

;; How many steps is the shortest path that collects all of the keys?
;; Key is lowercase letters, doors uppercase, me is @.

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
  (loop [edge [[me (ordered-set)]]
         paths #{me}
         all-keys []]
    
    (if (empty? edge)
      (into [] (map (fn [[pos deps]] [(level pos) deps]) all-keys))

      (let [{:keys [t-doors t-empty t-keys]}
            (->> edge
                 (map around)
                 (apply concat)
                 (into #{})
                 (filter (comp nil? paths first))
                 (group-by
                  (fn [[pos deps]]
                    (let [tile (level pos)]
                      (cond
                        (nil? tile) :walls
                        (= \@ tile) :t-empty
                        (= \. tile) :t-empty
                        (Character/isUpperCase tile) :t-doors
                        (Character/isLowerCase tile) :t-keys)))))
            
            next-edge (into [] (concat t-empty
                                       (map (fn [[pos deps]] [pos (conj deps (level pos))]) t-keys)
                                       (map (fn [[pos deps]] [pos (conj deps (level pos))]) t-doors)))]
        
        (recur next-edge
               (into paths (map first next-edge))
               (into all-keys t-keys))))))

(def deps (find-nearest))

(defn filter-deps [d]
  (let [last-key (first (filter #(Character/isLowerCase %) (reverse d)))]
    (loop [t (ordered-set)
           d d]

      (if (empty? d) t
          (let [c (first d)]
            (recur (if (or (= c last-key)
                           (Character/isUpperCase c))
                     (conj t (Character/toLowerCase c))
                     t)
                   (rest d)))))))

(def filtered-deps
  (into {} (map (fn [[k v]] [k (filter-deps v)]) deps)))

(defn choices [left deps]
  (->>
   left
   (filter (fn [k] (not-any? left (deps k))))
   (into [])))

(defn distance [level a b]
  (loop [edge [a]
         paths #{a}
         dist 0]
    
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

(defn get-cost [level a b]
  (let [ac (first (filter #(= a (second %)) level))
        bc (first (filter #(= b (second %)) level))]

    (distance level (first ac) (first bc))))

(def get-cost-m (memoize (partial get-cost level)))

(def all-traversals
  (memoize
   (fn [at left]
     (if (empty? left) 0
         (->> (choices left filtered-deps)
              (map (fn [choice]
                     (+ (get-cost-m choice at)
                        (all-traversals choice (disj left choice)))))
              (apply min))))))

(defn part1 []
  (all-traversals \@ (into #{} (map first filtered-deps))))
