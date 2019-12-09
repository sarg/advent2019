(ns advent2019.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent2019.day7 :refer [intcode-run-map code-to-map halt?]]))

(def data
  (with-open [rdr (io/reader (io/resource "day9.in"))]
    (into [] (map #(Integer/parseInt %) (.split (.readLine rdr) ",")))))

(get-in (intcode-run-map data {:input [1]}) [1 :output])

(loop [output (transient [])
       code [(code-to-map data) {:input [2]}]]

  (let [out (get-in code [1 :output])
        new-out (if out (conj! output out) output)]

    (if (halt? code)
      (persistent! new-out)
      (recur new-out (apply intcode-run code)))))
