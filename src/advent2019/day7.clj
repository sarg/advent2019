(ns advent2019.day7
  (:require [clojure.java.io :as io]
            [advent2019.day5 :refer [digits]]
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
 
(defn intcode-run
  [code {:keys [ip input output state]
         :or {ip 0, state 'running, input [], output []}
         :as all-state}]

  (loop [code (transient code)
         loop-state (transient {:ip ip :input input})]
    
    (let [ip (:ip loop-state)
          mode-op (get code ip)
          op (rem mode-op 100)
          mode-part (quot mode-op 100)
          modes (digits mode-part)

          at #(get code %)
          arg #(get code (+ ip 1 %))
          mode #(or (get modes %) 0)
          marg #(case (mode %)
                  0 (arg %)
                  1 (+ ip 1 %))]

      (case op
        ;; halt - return program
        99 [(persistent! code)
            (conj all-state (persistent! loop-state) {:state 'halt})]

        ;; + 
        1 (recur
           (assoc! code (arg 2)
                   (+ (at (marg 0)) (at (marg 1))))
           (assoc! loop-state
                   :ip (+ ip 4)))
        
        ;; *
        2 (recur
           (assoc! code (arg 2)
                   (* (at (marg 0)) (at (marg 1))))
           (assoc! loop-state
                   :ip (+ ip 4)))
        
        ;; Opcode 3 takes a single integer as input and saves it to the address given by
        ;; its only parameter. For example, the instruction 3,50 would take an input
        ;; value and store it at address 50.
        3 (let [input (:input loop-state)]
            (if (empty? input)
              [(persistent! code)
               (conj all-state {:ip ip :input [] :state 'waiting-input})]

              (recur
               (assoc! code (arg 0) (first input))
               (assoc! loop-state
                       :ip (+ ip 2)
                       :state 'running
                       :input (drop 1 input)))))
        
        ;; Opcode 4 outputs the value of its only parameter. For example, the
        ;; instruction 4,50 would output the value at address 50.
        4 (let [out-val (at (arg 0))]
            [(persistent! code)
             (conj all-state (persistent! loop-state) {:ip (+ ip 2) :output out-val})])
        
        ;; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets
        ;; the instruction pointer to the value from the second parameter.
        ;; Otherwise, it does nothing.
        5 (recur
           code
           (assoc! loop-state
                   :ip (if (zero? (at (marg 0)))
                         (+ ip 3)
                         (at (marg 1)))))
        
        ;; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the
        ;; instruction pointer to the value from the second parameter. Otherwise,
        ;; it does nothing.
        6 (recur
           code
           (assoc! loop-state
                   :ip (if (zero? (at (marg 0)))
                         (at (marg 1))
                         (+ ip 3))))
        
        ;; Opcode 7 icode-with-state-seqs less than: if the first parameter is less than the second
        ;; parameter, it stores 1 in the position given by the third parameter.
        ;; Otherwise, it stores 0.
        7 (recur
           (assoc! code (marg 2)
                   (if (< (at (marg 0)) (at (marg 1))) 1 0))

           (assoc! loop-state
                   :ip (+ ip 4)))
        
        ;; Opcode 8 is equals: if the first parameter is equal to the second
        ;; parameter, it stores 1 in the position given by the third parameter.
        ;; Otherwise, it stores 0.
        8 (recur
           (assoc! code (marg 2)
                   (if (= (at (marg 0)) (at (marg 1))) 1 0))

           (assoc! loop-state
                   :ip (+ ip 4)))))))

(defn intcode-run-chain [code-with-state-seq]
  (map #(apply intcode-run %) code-with-state-seq))

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

(defn all-halt? [code-with-state-seq]
  (every? #(= 'halt (get-in % [1 :state])) code-with-state-seq))

(defn set-phases [code phases]
  (into [] (map #(vector code {:input [%]}) phases)))

(defn run-loop-until-halt [code phases]
  (loop [chain (-> code
                   (set-phases phases)
                   boot-with-0)]
    
    (if (all-halt? chain)
      (first (get-in (first chain) [1 :input]))
      (recur (-> chain
                 intcode-run-chain
                 pipe-output)))))

(defn solution [code]
  (last (sort (map #(run-loop-until-halt code %)
                   (cmb/permutations [0 1 2 3 4])))))

(def test-data [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
(assert (= 43210 (solution test-data)))
(assert (= 255840 (solution data)))

(defn bonus [code]
  (last (sort (map #(run-loop-until-halt code %)
                   (cmb/permutations [5 6 7 8 9])))))

(def bonus-data [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
(assert (= 139629729 (bonus bonus-data)))
(assert (= 84088865) (bonus data))
