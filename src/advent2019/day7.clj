(ns advent2019.day7
  (:require [clojure.java.io :as io]
            [advent2019.day5 :refer [intcode-run]]
            [clojure.math.combinatorics :as cmb]))

(def data
  (with-open [rdr (io/reader (io/resource "day7.in"))]
    (into [] (map #(Integer/parseInt %) (.split (.readLine rdr) ",")))))

;; When a copy of the program starts running on an amplifier, it will first use
;; an input instruction to ask the amplifier for its current phase setting (an
;; integer from 0 to 4). Each phase setting is used exactly once, but the Elves
;; can't remember which amplifier needs which phase setting.
;;
;; The program will then call another input instruction to get the amplifier's
;; input signal, compute the correct output signal, and supply it back to the
;; amplifier with an output instruction. (If the amplifier has not yet received
;; an input signal, it waits until one arrives.)

;; The first amplifier's input value is 0, and the last amplifier's output leads
;; to your ship's thrusters.
 
(defn run-chain [code [t0 t1 t2 t3 t4]]
  (let [l0 (first (last (intcode-run code 0 [t0 0] [])))
        l1 (first (last (intcode-run code 0 [t1 l0] [])))
        l2 (first (last (intcode-run code 0 [t2 l1] [])))
        l3 (first (last (intcode-run code 0 [t3 l2] [])))
        l4 (first (last (intcode-run code 0 [t4 l3] [])))]

    l4))

(defn all-combs [code]
  (map #(run-chain code %)
       (cmb/permutations [0 1 2 3 4])))

(defn solution [code]
  (last (sort (all-combs code))))

(assert (= 43210 (solution [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])))
(assert (= 255840 (solution data)))
