(ns advent2019.day23
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.combinatorics :as cmb]
            [advent2019.day17 :refer [collect-output prg-to-input last!]]
            [advent2019.intcode :refer [intcode-run code-to-map to-ascii]]
            [quil.core :as q]
            [quil.middleware :as m]))


(def data
  (with-open [rdr (io/reader (io/resource "day23.in"))]
    (into [] (map #(Long/parseLong %) (.split (.readLine rdr) ",")))))


;; To send a packet to another computer, the NIC will use three output
;; instructions that provide the destination address of the packet followed by
;; its X and Y values. For example, three output instructions that provide the
;; values 10, 20, 30 would send a packet with X=20 and Y=30 to the computer with
;; address 10.


;; To receive a packet from another computer, the NIC will use an input
;; instruction. If the incoming packet queue is empty, provide -1. Otherwise,
;; provide the X value of the next packet; the computer will then use a second
;; input instruction to receive the Y value for the same packet. Once both
;; values of the packet are read in this way, the packet is removed from the
;; queue.

;; Boot up all 50 computers and attach them to your network. What is the Y value
;; of the first packet sent to address 255?

(defn boot-with-address [i]
  (intcode-run (code-to-map data) {:input [i] :netbuf [] :netidx i}))

(def net (into {} (map #(vector % (boot-with-address %)) (range 50))))

(defn run-net [net]
  (loop [net net
         new-net (transient [])]
    (if (empty? net) (into {} (persistent! new-net))
        (let [[i [code state]] (first net)

              [new-code new-state out]
              (collect-output code state
                              :until #(= 3 (count %1)))]

          ;(when (not (empty? out)) (println "OUT" i ": " out))
          (recur (rest net)
                 (conj! new-net
                        [i [new-code (update new-state :netbuf
                                             #(into %1 %2) out)]]))))))

(defn get-packets [buf]
  (let [len (count buf)
        pcnt (quot len 3)

        [packets leftover]
        (split-at (* pcnt 3) buf)]

    [(map #(into [] %) (partition 3 packets))
     (into [] leftover)]))

(defn netbuf-collect-to-bus [net]
  (loop [net net
         new-net (transient net)
         bus []]
    (if (empty? net) [(persistent! new-net) bus]
        (let [[netidx [code {:keys [netbuf] :as state}]] (first net)]
          (let [[packets netbuf] (get-packets netbuf)]
            (recur (rest net)
                   (assoc! new-net netidx [code (assoc state :netbuf netbuf)])
                   (if (empty? packets) bus (into bus packets))))))))

;(netbuf-collect-to-bus {0 ['code {:netbuf [0 1 2 3 ]}]})

(defn apply-input [[code {:keys [input] :or {input []} :as state}] data]
  [code (assoc state :input (into [] (concat input data)))])

;(apply-input [nil {:input '()}] [3 4])

(defn netbus-to-input [net bus]
  (loop [net (transient net)
         bus bus]
    (if (empty? bus) (persistent! net)
        (let [[netidx & data] (first bus)]
          ;(println "IN" netidx ": " data)
          (recur (assoc! net netidx
                         (apply-input (get net netidx [[99] {}])
                                      data))
                 (rest bus))))))

;(netbus-to-input {0 ['code {}]} [[0 1 2] [0 4 3]])
(defn net-no-data [net]
  (loop [new-net (transient net)
         net net]
    (if (empty? net) (persistent! new-net)
        (let [[netidx [code {:keys [input] :as state}]] (first net)]
          (recur (if (empty? input)
                   (assoc! new-net netidx [code (assoc state :input [-1])])
                   new-net)
                 (rest net))))))

;(net-no-data {0 ['code {:input [3]}]})
;(net-no-data {0 ['code {}]})

(defn tick [net]
  (->> net
       (run-net)
       (netbuf-collect-to-bus)
       (apply netbus-to-input)
       (net-no-data)))

(defn all-idle [net]
  (every? (fn [[k v]] (or (= 255 k) (= [-1] (get-in v [1 :input])))) net))

;(all-idle {0 ['code {:input [-1]}]})
(defn part1 []
  (loop [net net]
    (if (get net 255)
      (get-in net [255 1 :input 1])
      (recur (tick net)))))

(defn part2-end? [n]
  (let [[[_ y1] [_ y2]] (take-last 2 n)]
    (and y1 y2 (= y1 y2))))

(defn part2 []
  (loop [net net
         nat []
         prev-nat nil
         i 0]

    ;; (->> net
    ;;      (map #(get-in % [1 1 :input]))
    ;;      (map count)
    ;;      (println))
    (let [nat-packet
          (get-in net [255 1 :input])

          new-nat (if (empty? nat-packet) nat (conj nat nat-packet))

          idle? (all-idle net)

          new-net
          (if-not (and idle? (not-empty new-nat)) net
                  (do
                    ;(println "idle" i (last new-nat))
                    (assoc net 0 (apply-input (net 0) (last new-nat)))))]
      ;(if nat-packet (println nat-packet))
      
      (if (and idle? prev-nat (= prev-nat (last new-nat)))
        (last prev-nat)
        (recur (tick (dissoc new-net 255))
               (if idle? [] new-nat)
               (if idle? (last new-nat) prev-nat)
               (inc i))))))
