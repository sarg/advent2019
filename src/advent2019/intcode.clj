(ns advent2019.intcode
  (:require [advent2019.day5 :refer [digits]]))

(defn intcode-step [code loop-state]
  (let [ip (:ip loop-state)
        mode-op (get code ip)
        op (rem mode-op 100)
        mode-part (quot mode-op 100)
        modes (digits mode-part)

        at #(get code % 0)
        arg #(get code (+ ip 1 %))
        mode #(get modes % 0)
        marg #(case (mode %)
                0 (arg %)
                1 (+ ip 1 %)
                2 (+ (:relbase loop-state) (arg %)))]

    (case op
      ;; halt - return program
      99 [code (assoc! loop-state :state 'halt)]

      ;; + 
      1 [(assoc! code (marg 2)
                 (+ (at (marg 0)) (at (marg 1))))

         (assoc! loop-state
                 :ip (+ ip 4))]
      
      ;; *
      2 [(assoc! code (marg 2)
                 (* (at (marg 0)) (at (marg 1))))
         (assoc! loop-state
                 :ip (+ ip 4))]
      
      ;; Opcode 3 takes a single integer as input and saves it to the address given by
      ;; its only parameter. For example, the instruction 3,50 would take an input
      ;; value and store it at address 50.
      3 (let [input (:input loop-state)]
          (if (empty? input)
            [code
             (assoc! loop-state :state 'waiting-input)]

            [(assoc! code (marg 0) (first input))
             (assoc! loop-state
                     :ip (+ ip 2)
                     :state 'running
                     :input (drop 1 input))]))
      
      ;; Opcode 4 outputs the value of its only parameter. For example, the
      ;; instruction 4,50 would output the value at address 50.
      4 [code
         (assoc! loop-state
                 :ip (+ ip 2)
                 :state 'pause
                 :output (at (marg 0)))]
      
      ;; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets
      ;; the instruction pointer to the value from the second parameter.
      ;; Otherwise, it does nothing.
      5 [code
         (assoc! loop-state
                 :ip (if (zero? (at (marg 0)))
                       (+ ip 3)
                       (at (marg 1))))]
      
      ;; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the
      ;; instruction pointer to the value from the second parameter. Otherwise,
      ;; it does nothing.
      6 [code
         (assoc! loop-state
                 :ip (if (zero? (at (marg 0)))
                       (at (marg 1))
                       (+ ip 3)))]
      
      ;; Opcode 7 is less than: if the first parameter is less than the second
      ;; parameter, it stores 1 in the position given by the third parameter.
      ;; Otherwise, it stores 0.
      7 [(assoc! code (marg 2)
                 (if (< (at (marg 0)) (at (marg 1))) 1 0))

         (assoc! loop-state
                 :ip (+ ip 4))]
      
      ;; Opcode 8 is equals: if the first parameter is equal to the second
      ;; parameter, it stores 1 in the position given by the third parameter.
      ;; Otherwise, it stores 0.
      8 [(assoc! code (marg 2)
                 (if (= (at (marg 0)) (at (marg 1))) 1 0))

         (assoc! loop-state
                 :ip (+ ip 4))]

      
      ;; Opcode 9 adjusts the relative base by the value of its only
      ;; parameter. The relative base increases (or decreases, if the value is
      ;; negative) by the value of the parameter.
      9 [code

         (assoc! loop-state
                 :ip (+ ip 2)
                 :relbase (+ (:relbase loop-state) (at (marg 0))))])))

(defn intcode-run
  [code {:keys [ip input output state relbase step]
         :or {ip 0, state 'running, input [], output [], relbase 0, step 0}
         :as all-state} & {:keys [steps doprint]}]

  (loop [code (transient code)
         loop-state (transient {:ip ip :input input
                                :relbase relbase
                                :state 'running
                                :step step})]

    (when doprint
      (println (into {} (map #(hash-map % (% loop-state)) [:ip :relbase :state :step]))))

    (if (not= 'running (:state loop-state))
      [(persistent! code) (conj all-state (persistent! loop-state))]

      (let [[new-code new-state]
            (intcode-step code loop-state)]
        (recur new-code (assoc! new-state :step (inc (:step loop-state))))))))

(defn code-to-map [vec]
  (into {} (map-indexed vector vec)))

(defn halt? [code-with-state]
  (-> code-with-state
      (get-in [1 :state])
      (= 'halt)))
