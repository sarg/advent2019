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

(defn solution []
  (let [layer-with-fewest-0-freq
        (->> image-layers
             (map frequencies)
             (sort-by #(get % \0))
             (first))]

    (* (get layer-with-fewest-0-freq \2)
       (get layer-with-fewest-0-freq \1))))

(assert (= 2210 (solution)))

;; The image is rendered by stacking the layers and aligning the pixels with the
;; same positions in each layer. The digits indicate the color of the
;; corresponding pixel: 0 is black, 1 is white, and 2 is transparent.

;; So, if a given position has a transparent pixel in the first and second
;; layers, a black pixel in the third layer, and a white pixel in the fourth
;; layer, the final image would have a black pixel at that position.

(defn reduce-layers [l1 l2]
  (map #(if (= %1 \2) %2 %1) l1 l2))

(defn bonus []
  (println 
   (->>
    image-layers
    (reduce reduce-layers)
    (partition width)
    (map #(replace {\0 \space} %))
    (map #(apply str %))
    (s/join "\n"))))

;;  11   11  1111  11  1111 
;; 1  1 1  1 1    1  1 1    
;; 1    1    111  1    111  
;; 1    1 11 1    1 11 1    
;; 1  1 1  1 1    1  1 1    
;;  11   111 1111  111 1111 
