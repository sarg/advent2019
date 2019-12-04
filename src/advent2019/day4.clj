(ns advent2019.day4)

;; You arrive at the Venus fuel depot only to discover it's protected by a
;; password. The Elves had written the password on a sticky note, but someone
;; threw it out.

;; However, they do remember a few key facts about the password:

;; It is a six-digit number. The value is within the range given in your puzzle
;; input. Two adjacent digits are the same (like 22 in 122345). Going from left
;; to right, the digits never decrease; they only ever increase or stay the
;; same (like 111123 or 135679). Other than the range rule, the following are
;; true:

;; 111111 meets these criteria (double 11, never decreases). 223450 does not
;; meet these criteria (decreasing pair of digits 50). 123789 does not meet
;; these criteria (no double). How many different passwords within the range
;; given in your puzzle input meet these criteria?

;; Your puzzle input is 138241-674034.

(defn pass-matches? [x]
  (loop [prev-d (rem x 10)
         x (quot x 10)
         has-double? false]
    (let [d (rem x 10)]
      (cond
        (zero? x) has-double?
        (< prev-d d) false
        :default (recur d (quot x 10) (or has-double? (= prev-d d)))))))

(assert (pass-matches? 111111))
(assert (not (pass-matches? 223450)))
(assert (not (pass-matches? 123789)))

(filter pass-matches? (range 138241 674035))

;; An Elf just remembered one more important detail: the two adjacent matching
;; digits are not part of a larger group of matching digits.

(defn get-doubles [x]
  (loop [acc (transient {})
         prev-d (rem x 10)
         x (quot x 10)]
    
    (let [d (rem x 10)
          is-double? (= prev-d d)
          new-acc (if is-double? (assoc! acc prev-d (inc (get acc prev-d 1))) acc)]
      (cond
        (zero? x) (persistent! acc)
        (< prev-d d) nil
        :default (recur new-acc d (quot x 10))))))

(defn bonus-pass-matches? [x]
  (when-let [d (vals (get-doubles x))]
    (= (apply min d) 2)))

;; Given this additional criterion, but still ignoring the range rule, the
;; following are now true:

;; 112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
(assert (bonus-pass-matches? 112233))

;; 123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
(assert (not (bonus-pass-matches? 123444)))

;; 111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).
(assert (bonus-pass-matches? 111122))

(count (filter bonus-pass-matches? (range 138241 674035)))
