(ns advent2019.day22
  (:require [clojure.java.io :as io]))

(def data
  (with-open [rdr (io/reader (io/resource "day22.in"))]
    (into [] (line-seq rdr))))

(defn cmd-coeff [alen cmd]
  (let [parts (into [] (.split cmd " "))]
    (cond
      (= "cut" (parts 0))
      [1 (- (Integer/parseInt (last parts)))]
      
      (= "stack" (last parts))
      [-1 -1]

      (= "deal" (parts 0))
      [(Integer/parseInt (last parts)) 0])))


(defn mulmod [a b m]
  (loop [res 0
         a (mod a m)
         b (mod b m)]
    (if (zero? a) res
        (recur
         (if (even? a) res (mod (+ res b) m))
         (quot a 2)
         (mod (* b 2) m)))))

(defn comp-mod
  ([m] [1 0])
  ([m [k1 b1] [k2 b2]]
   [(mulmod k1 k2 m) (mod (+ (mulmod k2 b1 m) b2) m)]))

(defn comp-coeff [data alen]
  (->>
   data
   (map (partial cmd-coeff alen))
   (reduce (partial comp-mod alen))))

(defn ap [idx alen [k b]]
  (mod (+ (mulmod k idx alen) b) alen))

(defn modinv [a n]
  (loop [t 0
         newt 1
         r n
         newr a]
    (if (zero? newr)
      (if (> r 1)
        "not invertible"
        (if (neg? t) (+ t n) t))

      (let [q (quot r newr)]
        (recur newt (- t (* q newt))
               newr (- r (* q newr)))))))

(defn modpow [b e m]
  (long (.modPow (biginteger b) (biginteger e) (biginteger m))))


(defn part1* [data alen idx]
  (->> (comp-coeff data alen)
       (ap idx alen)))

(defn part2* [data m n idx]
  (let [[k b] (comp-coeff data m)

        kn (modpow k n m)
        fk (mulmod (dec k) kn m)

        fb (mulmod b (dec kn) m)

        c (modinv fk m)
        t (mod (- (mulmod (dec k) idx m) fb) m)]
    (mulmod t c m)))

(part1* data 10007 2019)
(part2* data 119315717514047 101741582076661 2020)
