(ns advent2019.util)

(defn digits [num]
  (loop [acc (transient [])
         num num]
    (if (< num 10)
      (persistent! (conj! acc num))
      (recur (conj! acc (rem num 10)) (quot num 10)))))
