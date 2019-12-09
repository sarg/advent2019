(ns advent2019.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent2019.day7 :refer [intcode-run-map]]))

(def data
  (with-open [rdr (io/reader (io/resource "day9.in"))]
    (into [] (map #(Integer/parseInt %) (.split (.readLine rdr) ",")))))

(get-in (intcode-run-map data {:input [1]}) [1 :output])
