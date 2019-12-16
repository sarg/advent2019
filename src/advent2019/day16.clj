(ns advent2019.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))


(defn digit-str-to-vec [s]
  (into [] (map (comp (partial + (- (int \0))) int) s)))

(def data
  (with-open [rdr (io/reader (io/resource "day16.in"))]
    (.readLine rdr)))

(def base-pattern [0 1 0 -1])
(def pattern-len (count base-pattern))

(defn pattern-n [n]
  (drop 1 (flatten (repeat (flatten (map (partial repeat n) base-pattern))))))

;; 0 1 2 3 | 0 1 2 3
;; 00 11 22 33
(defn pattern-n* [n i]
  (as-> i v
    (mod v (* n pattern-len))
    (quot v n)
    (get base-pattern v)))

(definline last-digit [n] `(mod (Math/abs ~n) 10))

(defn fft-step [in idx & _]
  (->> in
       (map * (map (partial pattern-n* (inc idx)) (range 1 (inc (count in)))))
       (reduce +)
       (last-digit)))

(defn fft [in] (map-indexed (partial fft-step in) in))

(defn solution [x & [n]]
  (->> x
       (digit-str-to-vec)
       (iterate fft)
       (drop (or n 100))
       (first)
       (take 8)
       (s/join)))

(assert (= "24176176" (solution "80871224585914546619083218645595")))
(assert (= "73745418" (solution "19617804207202209144916044189917")))
(assert (= "52432133" (solution "69317163492948606335995924319873")))
;; (assert (= "84970726" (solution data)))

