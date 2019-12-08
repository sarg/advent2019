(ns advent2019.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))


(def data
  (with-open [rdr (io/reader (io/resource "day8.in"))]
    (into [] (.readLine rdr))))

;; The image you received is 25 pixels wide and 6 pixels tall.

;; To make sure the image wasn't corrupted during transmission, the Elves would
;; like you to find the layer that contains the fewest 0 digits. On that layer,
;; what is the number of 1 digits multiplied by the number of 2 digits?

(defn layers [data x y]
  (partition (* x y) data))

(def width 25)
(def height 6)
(def image-layers (layers data width height))
(def layer-with-fewest-0
  (first (sort-by (fn [c] (count (filter #(= \0 %) c)))
                  image-layers)))

(assert (= 2210
           (* (count (filter #(= \1 %) layer-with-fewest-0))
              (count (filter #(= \2 %) layer-with-fewest-0)))))

;; The image is rendered by stacking the layers and aligning the pixels with the
;; same positions in each layer. The digits indicate the color of the
;; corresponding pixel: 0 is black, 1 is white, and 2 is transparent.

;; So, if a given position has a transparent pixel in the first and second
;; layers, a black pixel in the third layer, and a white pixel in the fourth
;; layer, the final image would have a black pixel at that position.

(defn reduce-layers [l1 l2]
  (map #(if (= %1 \2) %2 %1) l1 l2))

(println 
 (->>
  image-layers
  (reduce reduce-layers)
  (partition width)
  (map #(replace {\0 " "} %))
  (map #(apply str %))
  (s/join "\n")))

;;  11   11  1111  11  1111 
;; 1  1 1  1 1    1  1 1    
;; 1    1    111  1    111  
;; 1    1 11 1    1 11 1    
;; 1  1 1  1 1    1  1 1    
;;  11   111 1111  111 1111 
