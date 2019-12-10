(ns advent2019.day6
  (:require [clojure.java.io :as io]
            [clojure.core.reducers :as r]
            [clojure.set :as s]))

(def data
  (with-open [rdr (io/reader (io/resource "day6.in"))]
    (into [] (map #(into [] (.split % "\\)")) (line-seq rdr)))))

(def test-data
  [["COM" "B"]
   ["B" "C"]
   ["C" "D"]
   ["D" "E"]
   ["E" "F"]
   ["B" "G"]
   ["G" "H"]
   ["D" "I"]
   ["E" "J"]
   ["J" "K"]
   ["K" "L"]])

(defn adj-graph [vs]
  (loop [vs vs
         acc (transient {})
         roots (transient (hash-set))
         not-roots (transient (hash-set))]

    (if (empty? vs)
      [(persistent! acc) (s/difference (persistent! roots) (persistent! not-roots))]

      (let [[p1 p2] (first vs)
            p1-list (get acc p1 [])]
        
        (recur (drop 1 vs)
               (conj! acc {p1 (conj p1-list p2)})
               (conj! roots p1)
               (conj! not-roots p2))))))

(defn solution [vs]
  (let [[graph roots] (adj-graph vs)]
    
    (loop [layer roots
           level 0
           total 0]

      (if (empty? layer) total
          (recur
           (set (flatten (map #(get graph % []) layer)))
           (inc level)
           (+ total (* (count layer) level)))))))

(assert (= 42 (solution test-data)))

(assert (= 241064 (solution data)))

(defn adj-bonus-graph [vs]
  (loop [vs vs
         acc (transient {})]
    
    (if (empty? vs)
      (persistent! acc)

      (let [[p1 p2] (first vs)
            p1-list (get acc p1 [])
            p2-list (get acc p2 [])]

        (recur (drop 1 vs)
               (assoc! acc p2 (conj p2-list p1)))))))

(defn reachable [graph from]
  (loop [acc (transient {})
         step 0
         from from]

    (if (empty? from) (persistent! acc)
        (let [new-from (flatten (map #(get graph % []) from))]
          (recur (conj! acc (zipmap from (repeat step)))
                 (inc step)
                 new-from)))))

(defn bonus [vs]
  (let [graph (adj-bonus-graph vs)
        you (reachable graph ["YOU"])
        san (reachable graph ["SAN"])

        common (s/intersection (into #{} (keys you))
                               (into #{} (keys san)))]

    (first (sort (map #(+ -2 (get you %) (get san %)) common)))))

(assert (= 4 (bonus (concat test-data [["K" "YOU"] ["I" "SAN"]]))))
(assert (= 418 (bonus data)))
