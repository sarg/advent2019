(ns advent2019.day17
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [quil.core :as q]
            [quil.middleware :as m]
            [advent2019.intcode :refer [intcode-run halt? code-to-map]]))


(def data
  (with-open [rdr (io/reader (io/resource "day17.in"))]
    (into [] (map #(Long/parseLong %) (.split (.readLine rdr) ",")))))

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

(def level-map
  (last (collect-output (code-to-map data) {})))

(defn index-of-pred
  [pred coll]
  (ffirst (filter (comp pred second) (map-indexed list coll))))

(def intersections
  (let [w (inc (index-of-pred (partial = (int \newline)) level-map))
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

(def level-str (apply str (map char level-map)))
(def part1 (reduce + (map (fn [[x y]] (* x y)) intersections)))
