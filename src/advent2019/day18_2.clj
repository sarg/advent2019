(ns advent2019.day18-2
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as cmb]
            [clojure.pprint :as pp]
            [advent2019.day18 :refer [row-to-entries around get-cost]]
            [flatland.ordered.set :refer [ordered-set]]))


(def data
  (with-open [rdr (io/reader (io/resource "day18_part2.in"))]
    (into [] (line-seq rdr))))

(def level
  (apply hash-map (apply concat (map-indexed row-to-entries data))))

(def robots
  (keep (fn [[k v]] (when (#{\1 \2 \3 \4} v) [k v])) level))

(defn find-nearest [me]
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

(defn filtered-deps [[deps me]]
  (->> deps
       (map (fn [[k v]]
              [k (filter-deps v)]))
       (into {})))

(def robot-deps
  (->>
   robots
   (map (fn [[k v]] [(find-nearest k) v]))
   (map filtered-deps)
   (into [])))

(defn choices [open deps]
  (keep (fn [[k v]]
          (when (and (not (open k)) (every? open v)) k))
        deps))

(def get-cost-m (memoize (partial get-cost level)))

(def part2
  (memoize
   (fn [robots open]
     (let [next-moves 
           (->> robot-deps
                (map (fn [k v] [k v]) robots)
                (map (fn [[at deps]] (conj (choices open deps) at)))
                (apply cmb/cartesian-product)
                (map vec)
                (filter (complement (partial = robots)))
                (into []))]

       (if (empty? next-moves) 0
           (->> next-moves
                (map (fn [choice]
                       (+ (reduce + (map #(get-cost-m %1 %2) choice robots))
                          (part2 choice (into open choice)))))
                (apply min)))))))

(let [robots-order (into [] (map second robots))]
  (part2 robots-order
         (set robots-order)))

