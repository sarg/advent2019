(ns advent2019.day24
  (:require [clojure.string :as string]))

(def data
  ["#.#.."
   ".#.#."
   "#...#"
   ".#..#"
   "##.#."])

(def test-data
  ["....#"
   "#..#."
   "#..##"
   "..#.."
   "#...."])

(defn s-to-num [s]
  (->> s
       (map-indexed
        (fn [i v]
          (if (= \# v)
            (bit-shift-left 1 i)
            0)))
       (reduce +)))

(defn grid-to-num [g] (s-to-num (string/join g)))

(defn masks [w h]
  (into []
        (for [y (range h)
              x (range w)]

          (let [bit (+ x (* y w))
                v (bit-shift-left 1 bit)]
            (+
             (if (zero? x) 0 (bit-shift-right v 1))
             (if (= x (dec w)) 0 (bit-shift-left v 1))
             (if (zero? y) 0 (bit-shift-right v w))
             (if (= y (dec h)) 0 (bit-shift-left v w)))))))

(defn bit-count [v]
  (loop [v v c 0]
    (if (= v 0) c
        (recur (bit-shift-right v 1)
               (+ c (bit-and v 1))))))

;; A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
;; An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
(defn tick [masks grid]
  (->> masks
       (map-indexed
        (fn [i v]
          (let [around (bit-count (bit-and v grid))
                my-bit (bit-shift-left 1 i)
                me (bit-and my-bit grid)]
            (cond
              (and (= me 0) (or (= 1 around) (= 2 around))) my-bit
              (and (> me 0) (not= 1 around)) 0
              :else me))))
       (reduce +)))

(defn num-to-s [l v]
  (loop [v v c (transient []) l l]
    (if (= l 0)
      (apply str (persistent! c))
        (recur (bit-shift-right v 1)
               (conj! c (if (zero? (bit-and v 1)) \. \#))
               (dec l)))))

(defn print-grid [g]
  (println "---")
  (->> g
       (num-to-s 25)
       (partition 5)
       (map (partial apply str))
       (run! println))
  g)

(defn part1 [g]
  (let [g (grid-to-num g)
        m (masks 5 5) ]
    (loop [g g acc (transient #{g})]
      (let [v (tick m g)]
        (if (acc v) v
            (recur v (conj! acc v)))))))

(part1 data)

;;      |     |         |     |     
;;   1  |  2  |    3    |  4  |  5  
;;      |     |         |     |     
;; -----+-----+---------+-----+-----
;;      |     |         |     |     
;;   6  |  7  |    8    |  9  |  10 
;;      |     |         |     |     
;; -----+-----+---------+-----+-----
;;      |     |A|B|C|D|E|     |     
;;      |     |-+-+-+-+-|     |     
;;      |     |F|G|H|I|J|     |     
;;      |     |-+-+-+-+-|     |     
;;  11  | 12  |K|L|?|N|O|  14 |  15 
;;      |     |-+-+-+-+-|     |     
;;      |     |P|Q|R|S|T|     |     
;;      |     |-+-+-+-+-|     |     
;;      |     |U|V|W|X|Y|     |     
;; -----+-----+---------+-----+-----
;;      |     |         |     |     
;;  16  | 17  |    18   |  19 |  20 
;;      |     |         |     |     
;; -----+-----+---------+-----+-----
;;      |     |         |     |     
;;  21  | 22  |    23   |  24 |  25 
;;      |     |         |     |     
(defn inner-masks [w h]
  (let [c (+ (* w (quot h 2)) (quot w 2))]
    
    (assoc 
     (into [] (repeat (* w h) 0))
     (- c w) (reduce + (map (partial bit-shift-left 1) (range 0 w 1)))
     (+ c w) (reduce + (map (partial bit-shift-left 1) (range (* (- h 1) w) (* h w) 1)))

     (+ c 1) (reduce + (map (partial bit-shift-left 1) (range (- w 1) (* h w) w)))
     (- c 1) (reduce + (map (partial bit-shift-left 1) (range 0 (inc (* (- h 1) w)) w))))))

(defn outer-masks [w h]
  (let [c (+ (* w (quot h 2)) (quot w 2))
        up (bit-shift-left 1 (- c w))
        left (bit-shift-left 1 (- c 1))
        right (bit-shift-left 1 (+ c 1))
        down (bit-shift-left 1 (+ c w))]

    (->>
     (for [y (range h)
           x (range w)]
       (+ 
        (cond
          (= y 0) up
          (= y (dec h)) down
          :else 0)
        (cond
          (= x 0) left
          (= x (dec w)) right
          :else 0)))
     (into []))))

(defn outer-mask [w h]
  (->>
   (for [y (range h)
         x (range w)
         :when 
         (or (= y 0) (= y (dec h)) (= x 0) (= x (dec w)))]

     (+ x (* y w)))
   (map (partial bit-shift-left 1))
   (reduce +)))

(defn inner-mask [w h]
  (->>
   (for [y (range h)
         x (range w)
         :when 
         (not (or (= y 0) (= y (dec h)) (= x 0) (= x (dec w))))]

     (+ x (* y w)))
   (map (partial bit-shift-left 1))
   (reduce +)))

(defn add-layers [out-mask in-mask g]
  (->> g
       (map (fn [[k v]]
              [(when (pos? (bit-and v out-mask)) (dec k))
               (when (pos? (bit-and v in-mask)) (inc k))]))

       (flatten)
       (keep identity)
       (filter (complement g))
       (map #(vector % 0))
       (into g)))

(defn apply-mask [out-m m in-m layers [level v]]
  (->>
   (map (fn [o m i bit]
          (let [around (bit-count (bit-and m v))
                outer-around (bit-count (bit-and o (layers (dec level) 0)))
                inner-around (bit-count (bit-and i (layers (inc level) 0)))

                total (+ around outer-around inner-around)

                my-bit (bit-shift-left 1 bit)
                me (bit-and my-bit v)]

            (cond
              (and (= me 0) (or (= 1 total) (= 2 total))) my-bit
              (and (> me 0) (not= 1 total)) 0
              :else me)))
        out-m m in-m
        (range (count m))
       ; (iterate inc 0)
        )
   (reduce +)
   (vector level)))

(defn apply-masks [out-m m in-m layers]
  (->> layers
       (map (partial apply-mask out-m m in-m layers))
       (into {})))

(defn masks-with-center-removed [w h]
  (let [c (+ (* w (quot h 2)) (quot w 2))]
    (assoc (masks w h) c 0)))

(defn part2-step [g]
  (let [out-masks (outer-masks 5 5)
        in-masks (inner-masks 5 5 )
        masks (masks-with-center-removed 5 5)

        out-mask (outer-mask 5 5)
        in-mask (inner-mask 5 5)]
    
   (->> g
        (add-layers out-mask in-mask)
        (apply-masks out-masks masks in-masks))))

(defn part2 [g cnt]
  (->>
   (iterate part2-step {0 (grid-to-num g)})
   (drop cnt)
   (first)
   (vals)
   (map bit-count)
   (reduce +)))

(part2 data 200)
