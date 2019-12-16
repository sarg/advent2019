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
  (loop [in in
         i 1
         acc 0]

    (if (empty? in) (last-digit acc)
        (let [el (first in)
              p (pattern-n* (inc idx) i)]
          (recur (rest in) (inc i)
                 (+ acc (* el p)))))))

(defn fft [in] (map-indexed (partial fft-step in) in))

(defn solution [x]
  (->> x
       (map #(- (int %) (int \0)))
       (iterate fft)
       (drop 100)
       (first)
       (take 8)
       (s/join)))

(assert (= "24176176" (solution "80871224585914546619083218645595")))
(assert (= "73745418" (solution "19617804207202209144916044189917")))
(assert (= "52432133" (solution "69317163492948606335995924319873")))
;; (assert (= "84970726" (solution data)))

;; The real signal is your puzzle input repeated 10000 times. Treat this new
;; signal as a single input list. Patterns are still calculated as before, and
;; 100 phases of FFT are still applied.

;; The first seven digits of your initial input signal also represent the
;; message offset.
(def offset
  (Integer/parseInt (.substring data 0 7)))

;; The message offset is the location of the eight-digit message in the final
;; output list. Specifically, the message offset indicates the number of digits
;; to skip before reading the eight-digit message. For example, if the first
;; seven digits of your initial input signal were 1234567, the eight-digit
;; message would be the eight digits after skipping 1,234,567 digits of the
;; final output list. Or, if the message offset were 7 and your final output
;; list were 98765432109876543210, the eight-digit message would be
;; 21098765. (Of course, your real message offset will be a seven-digit number,
;; not a one-digit number like 7.)


;; After repeating your input signal 10000 times and running 100 phases of FFT,
;; what is the eight-digit message embedded in the final output list?

;; d zeros
;; d+1 ones
;; d+1 zeros
;; d+1 minus ones

(defn partial-sum [cumsums len from to]
  (- (cumsums (min len to)) (cumsums from)))

(defn fft1 [s]
  (let [sums
        (into [] (reductions + 0 (map #(- (int %) 48) s)))

        s-len (count s)

        dest (byte-array s-len)]

    (doseq [d (range s-len)
            :let [p (* 4 (inc d))

                  ones
                  (map #(partial-sum sums s-len % (+ % d 1))
                       (range d s-len p))

                  minus-ones
                  (map #(partial-sum sums s-len % (+ % d 1))
                       (range (+ d d d 1 1) s-len p))]]

      (aset-byte dest d
                 (+ 48
                    (last-digit
                     (- (reduce + ones)
                        (reduce + minus-ones))))))
    (String. dest)))

(defn solution-fast [x]
  (subs (first (drop 100 (iterate fft1 x)))
        0 8))

(assert (= "84970726" (solution-fast data)))

;; Given OFFSET and SIGNAL * 10000
;; Return part of signal from OFFSET till end
(defn from-offset [signal offset times]
  ;; first skip X signals from beginning
  ;; and get reminder
  ;; xxx xxx xxx xx|x ... xxx xxx
  ;;            ^  ^
  ;;         skip  offset
  ;; 
  (let [s-len (count signal)
        s-rem (rem offset s-len)
        s-quot (quot offset s-len)]

    (s/join
     [(subs signal s-rem)
      (s/join (repeat (- times s-quot 1) signal))])))

(defn cumulative-sum [s]
  (loop [l (dec (count s)) acc s]
    (if (neg? l) acc
        (recur (dec l)
               (assoc! acc l
                       (mod (+ (get acc (inc l) 0)
                               (get acc l))
                            10))))))

(defn bonus [s]
  (let [offset (Integer/parseInt (subs s 0 7))

        signal
        (->>
         (from-offset s offset 10000)
         (map #(- (int %) (int \0)))
         (into []))]
    
    (loop [i 0 signal (transient signal)]
      (if (= i 100)
        (s/join (take 8 (persistent! signal)))

        (recur (inc i) (cumulative-sum signal))))))

(assert (= "84462026" (bonus "03036732577212944063491565474664")))
(assert (= "78725270" (bonus "02935109699940807407585447034323"))) 
(assert (= "53553731" (bonus "03081770884921959731165446850517"))) 
;; (assert (= "47664469" (bonus data)))
