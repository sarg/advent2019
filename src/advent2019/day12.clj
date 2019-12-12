(ns advent2019.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent2019.intcode :refer [intcode-run halt? code-to-map]]))


;; Each moon has a 3-dimensional position (x, y, and z) and a 3-dimensional
;; velocity. The position of each moon is given in your scan; the x, y, and z
;; velocity of each moon starts at 0.

(def data
  [[  -8,  -9,  -7 ]
   [  -5,  2,  -1 ]
   [  11,  8,  -14 ]
   [  1,  -4,  -11 ]])

(def tst
  [[  -1,  0,  2 ]
   [  2,  -10,  -7 ]
   [  4,  -8,  8 ]
   [  3,  5,  -1 ]])


;; Simulate the motion of the moons in time steps. Within each time step, first
;; update the velocity of every moon by applying gravity. Then, once all moons'
;; velocities have been updated, update the position of every moon by applying
;; velocity. Time progresses by one step once all of the positions are updated.

;; To apply gravity, consider every pair of moons. On each axis (x, y, and z),
;; the velocity of each moon changes by exactly +1 or -1 to pull the moons
;; together. For example, if Ganymede has an x position of 3, and Callisto has a
;; x position of 5, then Ganymede's x velocity changes by +1 (because 5 > 3) and
;; Callisto's x velocity changes by -1 (because 3 < 5). However, if the
;; positions on a given axis are the same, the velocity on that axis does not
;; change for that pair of moons.
(defn apply-gravity-to-axis [coords]
  (map (fn [x]
         (-> (- (count (filter (partial > x) coords)))
             (+ (count (filter (partial < x) coords)))))
       coords))

;; Once all gravity has been applied, apply velocity: simply add the velocity of
;; each moon to its own position. For example, if Europa has a position of x=1,
;; y=2, z=3 and a velocity of x=-2, y=0,z=3, then its new position would be
;; x=-1, y=2, z=6. This process does not modify the velocity of any moon.

(defn abs+ [[a b c]]
  (+ (Math/abs a) (Math/abs b) (Math/abs c)))

(defn solution [moons n]
  (let [[mx my mz] (map (fn [p] (into [] (map #(nth % p) moons))) (range 3))
        [vx vy vz] (repeat 3 (into [] (repeat (count moons) 0)))]
    
    (loop [i 0
           mx mx my my mz mz
           vx vx vy vy vz vz]
      
      (if (= i n)
        (let [pot-energy
              (->>
               (interleave mx my mz)
               (partition 3)
               (map abs+))

              kin-energy
              (->>
               (interleave vx vy vz)
               (partition 3)
               (map abs+))

              total-energy
              (reduce + (map * pot-energy kin-energy))]

          total-energy)

        (let [nvx (map + vx (apply-gravity-to-axis mx))
              nvy (map + vy (apply-gravity-to-axis my))
              nvz (map + vz (apply-gravity-to-axis mz))

              nmx (map + mx nvx)
              nmy (map + my nvy)
              nmz (map + mz nvz)]
          
          (do
            (when nil
              (println "Step " i)
              (run!
               #(println "pos=" (first %) ", vel=" (last %))
               (map vector
                    (partition 3 (interleave mx my mz))
                    (partition 3 (interleave vx vy vz))))
              (println))

            (recur (inc i) nmx nmy nmz nvx nvy nvz)))))))

(assert (= 179 (solution tst 10)))

(def tst1
  [[  -8,  -10,  0 ]
   [  5,  5,  10 ]
   [  2,  -7,  3 ]
   [  9,  -8,  -3 ]])

(assert (= 9127 (solution data 1000)))
