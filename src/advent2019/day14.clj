(ns advent2019.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-ingridient [in]
  (let [[cnt item]
        (.split (s/trim in) " ")]
    [item (bigint cnt)]))

(defn parse-reaction [in]
  (let [[src-str dst] (.split in "=>")
        src-seq (.split src-str ",")

        [item cnt] (parse-ingridient dst)]

    { item [cnt (into {} (map parse-ingridient src-seq))] }))

(defn parse-reactions [in-seq]
  (into {"ORE" [1 {"ORE" 1}]} (map parse-reaction in-seq)))

(def data
  (with-open [rdr (io/reader (io/resource "day14.in"))]
    (parse-reactions (line-seq rdr))))

(defn recipe* [recipe n]
  (if (zero? n) {}
    (into {} (for [[k v] recipe] [k (* n v)]))))

(defn ceil+ [a b]
  (+ (quot a b)
     (if (pos? (rem a b)) 1 0)))

(defn step [rs [bom have]]
  (loop [bom bom
         need {}
         have have]
    (if (empty? bom)
      [need have]

      (let [[el need-cnt] (first bom)
            has-cnt (have el (bigint 0))
            [recipe-out recipe] (rs el)

            real-need (- need-cnt has-cnt)
            recipe-cnt (ceil+ real-need recipe-out)

            produced (* recipe-cnt recipe-out)
            leftover (- produced real-need)]

        (recur (rest bom)
               (merge-with + need (recipe* recipe recipe-cnt))
               (if (pos? leftover)
                 (assoc have el leftover)
                 (dissoc have el)))))))

(defn not-only-ore? [[rs ls]]
  (or (not= 1 (count rs))
      (not= ["ORE"] (keys rs))))

(defn end-result [data amount]
  (as-> data v
    (partial step v)
    (iterate v [{"FUEL" amount} {}])
    (drop-while not-only-ore? v)
    (first v)))

(defn solution [data & amount]
  (get-in (end-result data (first (or amount [1]))) [0 "ORE"]))

(def tst
  (parse-reactions
   ["10 ORE => 10 A"
    "1 ORE => 1 B"
    "7 A, 1 B => 1 C"
    "7 A, 1 C => 1 D"
    "7 A, 1 D => 1 E"
    "7 A, 1 E => 1 FUEL"]))
(assert (= 31 (solution tst)))

(assert (= 165
           (solution
            (parse-reactions
             ["9 ORE => 2 A"
              "8 ORE => 3 B"
              "7 ORE => 5 C"
              "3 A, 4 B => 1 AB"
              "5 B, 7 C => 1 BC"
              "4 C, 1 A => 1 CA"
              "2 AB, 3 BC, 4 CA => 1 FUEL"]))))

(def tst2210736
  (parse-reactions
   ["171 ORE => 8 CNZTR"
    "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
    "114 ORE => 4 BHXH"
    "14 VRPVC => 6 BMBT"
    "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
    "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
    "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
    "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
    "5 BMBT => 4 WPTQ"
    "189 ORE => 9 KTJDG"
    "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
    "12 VRPVC, 27 CNZTR => 2 XDBXC"
    "15 KTJDG, 12 BHXH => 5 XCVML"
    "3 BHXH, 2 VRPVC => 7 MZWV"
    "121 ORE => 7 VRPVC"
    "7 XCVML => 6 RJRHP"
    "5 BHXH, 4 VRPVC => 5 LTCX"]))
(assert (= 2210736 (solution tst2210736)))

(def tst180697
  (parse-reactions
   ["2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
    "17 NVRVD, 3 JNWZP => 8 VPVL"
    "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
    "22 VJHF, 37 MNCFX => 5 FWMGM"
    "139 ORE => 4 NVRVD"
    "144 ORE => 7 JNWZP"
    "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
    "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
    "145 ORE => 6 MNCFX"
    "1 NVRVD => 8 CXFTF"
    "1 VJHF, 6 MNCFX => 4 RFSQX"
    "176 ORE => 6 VJHF"]))
(assert (= 180697 (solution tst180697)))

(def tst13312
  (parse-reactions
   ["157 ORE => 5 NZVS"
    "165 ORE => 6 DCFZ"
    "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
    "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
    "179 ORE => 7 PSHF"
    "177 ORE => 5 HKGWZ"
    "7 DCFZ, 7 PSHF => 2 XJWVT"
    "165 ORE => 2 GPVTF"
    "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]))

(assert (= 13312 (solution tst13312)))
(assert (= 502491 (solution data)))

(defn bonus [d upto]
  (let [min (quot upto (solution d))
        max (* 2 min)]

    (loop [min min max max]
      (let [guess (quot (+ min max) 2)
            ore (solution d guess)
            cmp (compare upto ore)]
        (if (or (zero? cmp) (= min guess) (= max guess))
          min
          (recur (if (pos? cmp) guess min)
                 (if (pos? cmp) max guess)))))))

(assert (= 82892753 (bonus tst13312 1000000000000)))
(assert (= 5586022 (bonus tst180697 1000000000000)))
(assert (= 460664 (bonus tst2210736 1000000000000)))
(assert (= 2944565 (bonus data 1000000000000)))
