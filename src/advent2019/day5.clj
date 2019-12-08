(ns advent2019.day5
  (:require [clojure.java.io :as io]))

(def data
  (with-open [rdr (io/reader (io/resource "day5.in"))]
    (into [] (map #(Integer/parseInt %) (.split (.readLine rdr) ",")))))

(defn digits [num]
  (loop [acc (transient [])
         num num]
    (if (< num 10)
      (persistent! (conj! acc num))
      (recur (conj! acc (rem num 10)) (quot num 10)))))

(defn intcode-run
  ([code ip] (intcode-run code ip [] []))
  ([code ip input output]

   ;; (println [(take ip code) (drop ip code)])
   (let [[mode-op & args] (drop ip code)
         op (rem mode-op 100)
         mode-part (quot mode-op 100)
         modes (digits mode-part)

         at #(nth code %)
         arg #(nth args %)
         mode #(or (get modes %) 0)
         marg #(case (mode %)
                 0 (arg %)
                 1 (+ ip 1 %))]

     (case op
       ;; halt - return program
       99 [code (into [] input) output]

       ;; + 
       1 (intcode-run
          (assoc code (arg 2)
                 (+ (at (marg 0)) (at (marg 1))))
          (+ ip 4)
          input output)

       ;; *
       2 (intcode-run
          (assoc code (arg 2)
                 (* (at (marg 0)) (at (marg 1))))
          (+ ip 4)
          input output)

       ;; Opcode 3 takes a single integer as input and saves it to the address given by
       ;; its only parameter. For example, the instruction 3,50 would take an input
       ;; value and store it at address 50.
       3 (intcode-run
          (assoc code (arg 0)
                 (first input))
          (+ ip 2)
          (drop 1 input) output)

       ;; Opcode 4 outputs the value of its only parameter. For example, the
       ;; instruction 4,50 would output the value at address 50.
       4 (intcode-run
          code
          (+ ip 2)
          input (conj output (at (arg 0))))

       ;; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets
       ;; the instruction pointer to the value from the second parameter.
       ;; Otherwise, it does nothing.
       5 (intcode-run
          code
          (if (zero? (at (marg 0)))
            (+ ip 3)
            (at (marg 1)))
          input output)
       
       ;; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the
       ;; instruction pointer to the value from the second parameter. Otherwise,
       ;; it does nothing.
       6 (intcode-run
          code
          (if (zero? (at (marg 0)))
            (at (marg 1))
            (+ ip 3))
          input output)
       
       ;; Opcode 7 is less than: if the first parameter is less than the second
       ;; parameter, it stores 1 in the position given by the third parameter.
       ;; Otherwise, it stores 0.
       7 (intcode-run
          (assoc code (marg 2)
                 (if (< (at (marg 0)) (at (marg 1))) 1 0))
          (+ ip 4)
          input output)
       
       ;; Opcode 8 is equals: if the first parameter is equal to the second
       ;; parameter, it stores 1 in the position given by the third parameter.
       ;; Otherwise, it stores 0.
       8 (intcode-run
          (assoc code (marg 2)
                 (if (= (at (marg 0)) (at (marg 1))) 1 0))
          (+ ip 4)
          input output)))))

(assert (= (intcode-run [1,0,0,0,99] 0) [[2,0,0,0,99] [] []]))
(assert (= (intcode-run [2,3,0,3,99] 0) [[2,3,0,6,99] [] []]))
(assert (= (intcode-run [2,4,4,5,99,0] 0) [[2,4,4,5,99,9801] [] []]))
(assert (= (intcode-run [1,1,1,4,99,5,6,0,99] 0) [[30,1,1,4,2,5,6,0,99] [] []]))

(assert (= (intcode-run [3,0,4,0,99] 0 [5] []) [[5,0,4,0,99] [] [5]]))

(assert (= (intcode-run [1101,100,-1,4,0] 0) [[1101,100,-1,4,99] [] []]))

;; solution
(assert (= (last (intcode-run data 0 [1] [])) [3 0 0 0 0 0 0 0 0 16489636]))

;; bonus
(assert (= (last (intcode-run data 0 [5] [])) [9386583]))
