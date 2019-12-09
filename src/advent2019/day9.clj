(ns advent2019.day9
  (:require [clojure.java.io :as io]
            [advent2019.intcode :refer [intcode-run code-to-map halt?]]))

(def data
  (with-open [rdr (io/reader (io/resource "day9.in"))]
    (into [] (map #(Integer/parseInt %) (.split (.readLine rdr) ",")))))

(defn collect-output [code input]
  (loop [output (transient [])
         code-with-state [(code-to-map data) {:input input}]]

    (let [[code state] code-with-state
          out-val (:output state)
          new-out (if out-val (conj! output out-val) output)]

      (if (halt? code-with-state)
        (persistent! new-out)
        (recur new-out
               (intcode-run code (assoc state :output nil)))))))

(assert (= [3518157894] (collect-output data [1])))
(assert (= [80379] (collect-output data [2])))
