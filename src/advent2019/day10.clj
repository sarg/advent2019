(ns advent2019.day10
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as cmb]
            [clojure.math.numeric-tower :refer [gcd]]
            [criterium.core :refer [bench with-progress-reporting quick-bench]]
            [clojure.set :as s]))

(def data
  (with-open [rdr (io/reader (io/resource "day10.in"))]
    (into [] (line-seq rdr))))

(defn cap [i m]
  (cond
    (< i 0) 0
    (>= i m) (dec m)
    :else i))

;; divide by GCD
;; e.g.
;; [5 5] -> [1 1]
;; [4 6] -> [2 2]
(defn to-angle [[cx cy] [x y]]
  (let [dx (- x cx)
        dy (- y cy)
        g (gcd dx dy)]

    [(quot dx g) (quot dy g)]))

;; (to-angle [0 0] [5 5])
;; (to-angle [0 0] [4 6])
;; (to-angle [0 0] [-5 5])

(defn l-hor [x1 x2 y] (map #(vector %1 y) (range x1 x2)))
(defn l-ver [x y1 y2] (map #(vector x %1) (range y1 y2)))

(defn rect [tlx tly brx bry]
  (concat 
   (l-hor tlx brx tly)
   (l-hor tlx brx bry)
   (l-ver tlx (inc tly) bry)
   (l-ver brx tly (inc bry))))

;; Expand rectangle with center in [x y]
;; Keep set of obstructed angles d = [dx dy]. Any i * d will be obstructed also.
(defn visible-count [grid x y w h]
  (let [max-d (max x y (- w x) (- h y))]
    (loop [d 1 oa #{}]
      (if (= d max-d) (count oa)
          (let [tlx (cap w (- x d))
                brx (cap w (+ x d))
                tly (cap h (- y d))
                bry (cap h (+ y d))

                obstructed-angles
                (->>
                 (rect tlx tly brx bry)
                 (filter (fn [[x y]] (= \# (get-in grid [y x]))))
                 (filter #(not= % [x y]))
                 (map (partial to-angle [x y]))
                 (set))]
            
            (recur (inc d)
                   (s/union oa obstructed-angles)))))))

(defn points [data]
  (let [h (count data)
        w (count (first data))
        mi (* h w)]
    (loop [i mi m (transient [])]
      (if (zero? i) (persistent! m)
          (let [x (mod i w)
                y (quot i w)
                ast? (= \# (get-in data [y x]))]
            (if ast? 
              (recur (dec i) (conj! m [x y (visible-count data x y w h)]))
              (recur (dec i) m)))))))

;; actually no need to go in expanding rectangle
;; just make a set of GCD-vectors for all asteroids but current
;; size of this set is the number of visible asteroids
(defn solution-1 [data]
  (let [all-asteroids
        (into [] (apply concat (map-indexed
                                (fn [y row]
                                  (keep-indexed
                                   (fn [x v] (when (= v \#) [x y]))
                                   row))
                                data)))]

    (->> all-asteroids
         (map (fn [me]
                (conj me
                      (->> all-asteroids
                           (filter (partial not= me))
                           (map (partial to-angle me))
                           (set)
                           (count)))))

         (sort-by last)
         last)))


(defn solution [data]
  (->> data
       (points)
       (sort-by last)
       last))

(def tst [".#..#"
          "....."
          "#####"
          "....#"
          "...##"])

(assert (= [3 4 8] (solution tst)))
(assert (= [37 25 309] (solution data)))

(assert (= [3 4 8] (solution-1 tst)))
(assert (= [37 25 309] (solution-1 data)))

(def tst1
  [".#..##.###...#######"
   "##.############..##."
   ".#.######.########.#"
   ".###.#######.####.#."
   "#####.##.#.##.###.##"
   "..#####..#.#########"
   "####################"
   "#.####....###.#.#.##"
   "##.#################"
   "#####.##.###..####.."
   "..######..##.#######"
   "####.##.####...##..#"
   ".#####..#.######.###"
   "##...#.##########..."
   "#.##########.#######"
   ".####.#.###.###.#.##"
   "....##.##.###..#####"
   ".#.#.###########.###"
   "#.#.#.#####.####.###"
   "###.##.####.##.#..##"])

(assert (= [11 13 210] (solution tst1)))

;; The Elves are placing bets on which will be the 200th asteroid to be
;; vaporized. Win the bet by determining which asteroid that will be; what do
;; you get if you multiply its X coordinate by 100 and then add its Y
;; coordinate? (For example, 8,2 becomes 802.)

(defn radian [[x y]]
  (let [y (- y)]
    (->
     (if (and (>= y 0) (< x 0))
       (- (* Math/PI 2.5) (Math/atan2 y x))
       (- (/ Math/PI 2) (Math/atan2 y x)))

     (* 10000)
     (Math/round))))

(defn with-angle-distance [cx cy [x y]]
  [(radian (to-angle [cx cy] [x y]))
   (+ (Math/abs (- x cx)) (Math/abs (- y cy)))
   x y])

(defn with-angles [data x y]
  (let [h (count data)
        w (count (first data))]
    
    (->>
     (cmb/cartesian-product (range w) (range h))
     (filter (partial not= [x y]))
     (filter (fn [[x y]] (= \# (get-in data [y x]))))

     (map (partial with-angle-distance x y))
     (sort))))

(defn laser-ordered [data x y]
  (let [an (with-angles data x y)
        total (count an)
        grouped-angles (group-by first an)
        all-angles (sort (keys grouped-angles))]

    (loop [i 0 acc []]
      (if (= (count acc) total) acc
        (recur (inc i)
               (->>
                all-angles
                (map #(get-in grouped-angles [% i]))
                (filter identity)
                (concat acc)))))))

(defn bonus [data x y n]
  (let [o (laser-ordered data x y)
        [_ _ xx yy] (nth o (dec n))]

    (+ yy (* 100 xx))))

;; taken from https://github.com/weavejester/medley/blob/1.1.0/src/medley/core.cljc#L198
(defn interleave-all
  "Returns a lazy seq of the first item in each coll, then the second, etc.
  Unlike `clojure.core/interleave`, the returned seq contains all items in the
  supplied collections, even if the collections are different sizes."
  {:arglists '([& colls])}
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
    (let [s1 (seq c1), s2 (seq c2)]
      (if (and s1 s2)
        (cons (first s1) (cons (first s2) (interleave-all (rest s1) (rest s2))))
        (or s1 s2)))))
  ([c1 c2 & colls]
   (lazy-seq
    (let [ss (remove nil? (map seq (conj colls c2 c1)))]
      (if (seq ss)
        (concat (map first ss) (apply interleave-all (map rest ss))))))))

(defn bonus-i [data x y n]
  (->> (with-angles data x y)
       (group-by first)
       (into (sorted-map))
       (vals)
       (apply interleave-all)
       (drop (dec n))
       (first)
       ((fn [[_ _ x y]] (+ y (* 100 x))))))

(assert (= 802 (bonus tst1 11 13 200)))
(assert (= 416 (bonus data 37 25 200)))

(assert (= 416 (bonus-i data 37 25 200)))
