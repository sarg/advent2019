(ns advent2019.day8
  (:require [clojure.java.io :as io]
            [advent2019.day5 :refer [digits]]
            [clojure.math.combinatorics :as cmb]))


(def data
  (with-open [rdr (io/reader (io/resource "day8.in"))]
    (into [] (.readLine rdr))))


;; The image you received is 25 pixels wide and 6 pixels tall.

;; To make sure the image wasn't corrupted during transmission, the Elves would
;; like you to find the layer that contains the fewest 0 digits. On that layer,
;; what is the number of 1 digits multiplied by the number of 2 digits?

(defn layers [data x y]
  (partition (* x y) data))

(def layer 
  (first (sort (fn [c1 c2] (compare (count (filter #(= \0 %) c1))
                                   (count (filter #(= \0 %) c2))))
              (layers data 25 6))))

(* (count (filter #(= \1 %) layer))
   (count (filter #(= \2 %) layer)))
