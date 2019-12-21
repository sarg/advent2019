(ns advent2019.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.combinatorics :as cmb]
            [advent2019.day17 :refer [collect-output prg-to-input last!]]
            [quil.core :as q]
            [quil.middleware :as m]))

(def data
  (with-open [rdr (io/reader (io/resource "day21.in"))]
    (into [] (map #(Long/parseLong %) (.split (.readLine rdr) ",")))))

;; Registers:
;; 
;; ground
;; 1234
;; ABCD
;; ground true
;; hole false
;;
;; write: T - temp, J - jump (start with false)
;;
;; instructions AND|OR|NOT X Y, where Y is writable
;;
;; Jump is pos + 4, walk pos + 1


;; jump if D ground
;; don't jump if all ABCD are ground
;; to avoid this situation
;; .................
;; .................
;; ....@............
;; #####..#.########

(def part1-prg
  ;; D && !(A && C) 
  ;; D && (!A || !C)
  ["NOT A T"
   "NOT C J"
   "OR T J"
   "AND D J"
   "WALK"])

(defn prg-x [prg A B C D & [E F G H I]]
  (loop [prg prg
         T false
         J false]

    (let [[cmd r1 r2] (string/split (first prg) #" ")

          reg (fn [n] (case n "A" A "B" B "C" C "D" D "J" J "T" T "E" E "F" F "G" G "H" H "I" I))

          result
          (case cmd
            "OR" (or (reg r1) (reg r2))
            "AND" (and (reg r1) (reg r2))
            "NOT" (not (reg r1))
            nil)]
      (if (nil? result) J
              (recur (rest prg)
                     (if (= r2 "T") result T)
                     (if (= r2 "J") result J))))))

(defn to-ground [s]
  (apply str (map #(if % "#" ".") s)))

;;  ABCD JUMP
;; @###.   n
;; @##.#   ?
;; @#.##   y
;;
;; @###.   n
;; @##..   n
;; @#..#   
;; @..#.
;;
;; 
;; ##..#
;; #.#..
;; #...#

;; If the springdroid falls into space, an ASCII rendering of the last moments of
;; its life will be produced. In these, @ is the springdroid, # is hull, and . is
;; empty space. For example, suppose you program the springdroid like this:

(def part1
  (->>
   {:input (apply prg-to-input part1-prg)}
   (collect-output (code-to-map data))
   (last)
   (last)))

(def part2-prg
  [;; E is nono
   "AND E J"

   ;; but !(B && C) allows
   "OR B T"
   "AND C T"
   "NOT T T"
   "OR T J"

   ;; !H is boo
   "AND H J"

   ;; A is definitely jump
   "NOT A T"
   "OR T J"

   ;; D should be ground of course
   "AND D J"

   "RUN"])

;; tricky:
;;      ABCDEFGHI
;; ####j...######|##
;; ####j.#.######|### ?
;; ####j.#.#..###|###
;;   ##j##.#..###|#####
;;  ###j#..###.#.|###
;;  ###j#.##.#.##|####
;;  ###j#..###.#.|.###
;;  ###j#..#.####|####

(defn inp-to-regs [s]
  (into [] (map #(case % \# true \. false) s)))

(defn test-2 [s]
  (apply prg-x part2-prg
         (inp-to-regs s)))

;; !C && D && !(!E && F)
;; should jump
(assert
 (empty?
  (keep-indexed
   (fn [i s] (when (false? (test-2 s)) [i s]))
   ["##.#..###"
    "#.##.#.##"
    ".########"
    "#..#.####"
    "#..###.#."
    ".#.#..###"])))
;;   ABCDEFGHI

;; no jump
(assert
 (empty?
  (keep-indexed
   (fn [i s] (when (true? (test-2 s)) [i s]))
   ["###..#.##"
    "##...####"
    "##.#.#..#"
    "####..###"])))
;;   ABCDEFGHI
(def part2
  (->>
   (collect-output (code-to-map data) {:input (apply prg-to-input part2-prg)})
   (last)
   (last)))
