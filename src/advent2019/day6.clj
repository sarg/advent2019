(ns advent2019.day6
  (:require [clojure.java.io :as io]
            [clojure.set :as s]))

(def data
  (with-open [rdr (io/reader (io/resource "day6.in"))]
    (into [] (map #(into [] (.split % "\\)")) (line-seq rdr)))))

(defn adj-graph [vs]
  (loop [vs vs
         acc (transient {})
         roots (transient (hash-set))
         not-roots (transient (hash-set))]

    (if (empty? vs)
      [(persistent! acc) (s/difference (persistent! roots) (persistent! not-roots))]

      ;; p0 ) p1
      ;; p1 ) p2
      ;;
      ;; { p1 [ ... p0 ], p0 [ ... ]}
      ;; { p2 [ ... p1 ], p1 [ ... ]}
      (let [[p1 p2] (first vs)
            p1-list (get acc p1 [])
            p2-list (get acc p2 [])]

        (recur (drop 1 vs)
               (conj! acc {p2 p2-list, p1 (conj p1-list p2)})
               (conj! roots p1)
               (conj! not-roots p2))))))

(defn solution [vs]
  (let [[graph roots] (adj-graph vs)]
    
    (loop [layer roots
           level 0
           total 0]

      (if (empty? layer) total
          (recur
           (set (flatten (map #(get graph %) layer)))
           (inc level)
           (+ total (* (count layer) level)))))))

(assert (= 42 (solution
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
                ["K" "L"]])))

(assert (= 241064 (solution data)))
