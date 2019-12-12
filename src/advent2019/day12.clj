(ns advent2019.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math.numeric-tower :refer [lcm]]
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

(definline cmpn [a b c d]
  `(- 0
      (compare ~a ~b)
      (compare ~a ~c)
      (compare ~a ~d)))

(defn apply-gravity [[a b c d] [va vb vc vd]]
  [(+ va (cmpn a b c d))
   (+ vb (cmpn b a c d))
   (+ vc (cmpn c b a d))
   (+ vd (cmpn d b a c))])

;; Once all gravity has been applied, apply velocity: simply add the velocity of
;; each moon to its own position. For example, if Europa has a position of x=1,
;; y=2, z=3 and a velocity of x=-2, y=0,z=3, then its new position would be
;; x=-1, y=2, z=6. This process does not modify the velocity of any moon.

(defn vsum [[a b c d] [va vb vc vd]]
  [(+ a va)
   (+ b vb)
   (+ c vc)
   (+ d vd)])

(defn abs+ [[a b c]]
  (+ (Math/abs a) (Math/abs b) (Math/abs c)))

(defn next-state [[mx my mz vx vy vz]]
  (let [nvx (apply-gravity mx vx)
        nvy (apply-gravity my vy)
        nvz (apply-gravity mz vz)

        nmx (vsum mx nvx)
        nmy (vsum my nvy)
        nmz (vsum mz nvz)]

    [nmx nmy nmz nvx nvy nvz]))

(defn init-state [moons]
  (into [] (concat
            (map (fn [p] (into [] (map #(nth % p) moons))) (range 3))
            (repeat 3 (into [] (repeat (count moons) 0))))))

(defn solution [moons n]
  (let [[mx my mz vx vy vz]
        (nth (iterate next-state (init-state moons)) n)

        pot-energy
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

    total-energy))

(assert (= 179 (solution tst 10)))
(assert (= 9127 (solution data 1000)))

(defn find-cycle [x]
  (let [v (repeat (count x) 0)

        v1 (apply-gravity x v)
        x1 (vsum x v1)

        v2 (apply-gravity x1 v1)
        x2 (vsum x1 v2)]
    
    (loop [x1 x1 v1 v1
           x2 x2 v2 v2
           i 1]

      (if (and (= x1 x2) (= v1 v2)) i
          (let [nv1 (apply-gravity x1 v1)
                nx1 (vsum x1 nv1)

                nv2 (apply-gravity x2 v2)
                nx2 (vsum x2 nv2)

                nv3 (apply-gravity nx2 nv2)
                nx3 (vsum nx2 nv3)]
            (recur nx1 nv1 nx3 nv3 (inc i)))))))

(defn bonus [moons]
  (reduce lcm
          [(find-cycle (map #(nth % 0) moons))
           (find-cycle (map #(nth % 1) moons))
           (find-cycle (map #(nth % 2) moons))]))

(def tst1
  [[ -8,  -10,  0]
   [ 5,  5,  10]
   [ 2,  -7,  3]
   [ 9,  -8,  -3]])

(assert (= 2772 (bonus tst)))
(assert (= 4686774924 (bonus tst1)))
(assert (= 353620566035124 (bonus data)))
