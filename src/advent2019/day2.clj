(ns advent2019.day2
  (:require [clojure.java.io :as io]))

;; An Intcode program is a list of integers separated by commas (like 1,0,0,3,99).
 
(def data
  (with-open [rdr (io/reader (io/resource "day2.in"))]
    (into [] (map #(Integer/parseInt %) (.split (.readLine rdr) ",")))))

;; To run one, start by looking at the first integer (called position 0). Here,
;; you will find an opcode - either 1, 2, or 99. The opcode indicates what to
;; do; for example, 99 means that the program is finished and should immediately
;; halt. Encountering an unknown opcode means something went wrong.

(defn intcode-run [code ip]
  (let [[op arg1 arg2 dest] (drop ip code)]
    (case op
      ;; halt - return program
      99 code

      ;; + 
      1 (intcode-run (assoc code dest (+ (nth code arg1) (nth code arg2))) (+ ip 4))

      ;; *
      2 (intcode-run (assoc code dest (* (nth code arg1) (nth code arg2))) (+ ip 4)))))

;; Here are the initial and final states of a few more small programs:

;; 1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
;; 2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
;; 2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
;; 1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.

(assert (= (intcode-run [1,0,0,0,99] 0) [2,0,0,0,99]))
(assert (= (intcode-run [2,3,0,3,99] 0) [2,3,0,6,99]))
(assert (= (intcode-run [2,4,4,5,99,0] 0) [2,4,4,5,99,9801]))
(assert (= (intcode-run [1,1,1,4,99,5,6,0,99] 0) [30,1,1,4,2,5,6,0,99]))

;; Once you have a working computer, the first step is to restore the gravity
;; assist program (your puzzle input) to the "1202 program alarm" state it had
;; just before the last computer caught fire. To do this, before running the
;; program, replace position 1 with the value 12 and replace position 2 with the
;; value 2. What value is left at position 0 after the program halts?

(first (intcode-run
        (assoc data 1 12 2 2)
        0))

;; The inputs should still be provided to the program by replacing the values at
;; addresses 1 and 2, just like before. In this program, the value placed in
;; address 1 is called the noun, and the value placed in address 2 is called the
;; verb. Each of the two input values will be between 0 and 99, inclusive.

;; Once the program has halted, its output is available at address 0, also just
;; like before. Each time you try a pair of inputs, make sure you first reset
;; the computer's memory to the values in the program (your puzzle input) - in
;; other words, don't reuse memory from a previous attempt.

;; Find the input noun and verb that cause the program to produce the output
;; 19690720. What is 100 * noun + verb? (For example, if noun=12 and verb=2, the
;; answer would be 1202.)


(loop [verb 0
       noun 0]

  (if (= 19690720
         (first (intcode-run
                 (assoc data 1 noun 2 verb)
                 0)))
    (+ (* 100 noun) verb)
    (recur (mod (+ 1 verb) 100) (if (= 99 verb) (+ 1 noun) noun))))
