(ns advent2019.day7
  (:require [clojure.java.io :as io]
            [advent2019.intcode :refer [intcode-run halt?]]
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
 
(defn do-io [[code {:keys [input] :as state}] new-input]
  [code (assoc state :input (if new-input (conj input new-input) input) :output nil)])

(defn pipe-output [code-with-state-seq]
  (loop [acc (transient [])
         input nil
         cs code-with-state-seq]

    (if (empty? cs)
      (persistent! (assoc! acc 0 (do-io (get acc 0) input)))
      
      (let [cur (first cs)
            output (get-in cur [1 :output])]

        (recur
         (conj! acc (do-io cur input))
         output
         (drop 1 cs))))))

(defn boot-with-0 [code-state-vec]
  (update-in code-state-vec [0 1 :input] conj 0))

(def all-halt? (partial every? halt?))

(defn set-phases [code phases]
  (into [] (map #(vector code {:input [%]}) phases)))

(defn run-loop-until-halt [code phases]
  (loop [chain (-> code
                   (set-phases phases)
                   boot-with-0)]
    
    (if (all-halt? chain)
      (first (get-in (first chain) [1 :input]))
      (recur (->> chain
                 (map (partial apply intcode-run))
                 pipe-output)))))

(defn solution [code]
  (last (sort (map #(run-loop-until-halt code %)
                   (cmb/permutations (range 5))))))

(def test-data [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
(assert (= 43210 (solution test-data)))
(assert (= 255840 (solution data)))

(defn bonus [code]
  (last (sort (map #(run-loop-until-halt code %)
                   (cmb/permutations (range 5 10))))))

(def bonus-test-data [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
(assert (= 139629729 (bonus bonus-test-data)))
(assert (= 84088865) (bonus data))
