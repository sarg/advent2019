(ns advent2019.day25
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.combinatorics :as cmb]
            [advent2019.day17 :refer [collect-output prg-to-input last!]]
            [advent2019.intcode :refer [intcode-run code-to-map]]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.set :as set]))


(def data
  (with-open [rdr (io/reader (io/resource "day25.in"))]
    (into [] (map #(Long/parseLong %) (.split (.readLine rdr) ",")))))

;; As the droid moves through its environment, it will describe what it
;; encounters. When it says Command?, you can give it a single instruction
;; terminated with a newline (ASCII code 10). Possible instructions are:

;; Movement via north, south, east, or west. To take an item the droid sees in
;; the environment, use the command take <name of item>. For example, if the
;; droid reports seeing a red ball, you can pick it up with take red ball. To
;; drop an item the droid is carrying, use the command drop <name of item>. For
;; example, if the droid is carrying a green ball, you can drop it with drop
;; green ball. To get a list of all of the items the droid is currently
;; carrying, use the command inv (for "inventory").

(defn cmd-to-input [c] (into [] (map int c)))
(defn print-out [r]
  (println (apply str (map char (last r)))) r)

(defn step [[code state] & cmd]
  (collect-output code (assoc state :input (cmd-to-input (string/join "\n" (concat cmd [""]))))))

(defn try-step [r & cmd] (print-out (apply step r cmd)))

(def hull-breach (collect-output (code-to-map data) {}))
;; == Hull Breach ==
;; You got in through a hole in the floor here. To keep your ship from also freezing, the hole has been sealed.
;; Doors here lead:
;; - north (holodeck)
;; - south (observatory)

(def holodeck (try-step hull-breach "north"))
;; == Holodeck ==
;; Someone seems to have left it on the Giant Grid setting.
;; Doors here lead:
;; - east (Warp Drive Maintenance)
;; - south (hull breach)
;; - west (science lab)
;; Items here:
;; - giant electromagnet
  
(def observatory (step hull-breach "south"))
;; == Observatory ==
;; There are a few telescopes; they're all bolted down, though.
;; Doors here lead:
;; - north (hull breach)
;; - east (navigation)
;; Items here:
;; - infinite loop

(def warp-drive-maintenance (try-step holodeck "east"))
;; == Warp Drive Maintenance ==
;; It appears to be working normally.
;; Doors here lead:
;; - north (kitchen)
;; - west (holodeck)
;; Items here:
;; - ornament

(def kitchen (try-step warp-drive-maintenance "north"))
;; == Kitchen ==
;; Everything's freeze-dried.
;; Doors here lead:
;; - north (sick bay)
;; - east (hallway)
;; - south (warp-drive-maintenance)
;; Items here:
;; - escape pod

(def hallway (try-step kitchen "east"))
;; == Hallway ==
;; This area has been optimized for something; you're just not quite sure what.
;; Doors here lead:
;; - west (kitchen)

(def sick-bay (try-step kitchen "north"))
;; == Sick Bay ==
;; Supports both Red-Nosed Reindeer medicine and regular reindeer medicine.
;; Doors here lead:
;; - south (kitchen)
;; Items here:
;; - dark matter

(def navigation (step observatory "east"))
;; == Navigation ==
;; Status: Stranded. Please supply measurements from fifty stars to recalibrate.
;; Doors here lead:
;; - west (observatory)
;; Items here:
;; - whirled peas

(def science-lab (step holodeck "west"))
;; == Science Lab ==
;; You see evidence here of prototype polymer design work.
;; Doors here lead:
;; - north (crew quarters)
;; - east (holodeck)
;; - west (gift wrapping center)

(def gift-wrapping-center (step science-lab "west"))
;; == Gift Wrapping Center ==
;; How else do you wrap presents on the go?
;; Doors here lead:
;; - east (science lab)
;; - west (passages)
;; Items here:
;; - candy cane

(def crew-quarters (step science-lab "north"))
;; == Crew Quarters ==
;; The beds are all too small for you.
;; Doors here lead:
;; - east (engineering)
;; - south (science-lab)
;; - west (stables)
;; Items here:
;; - astrolabe

(def engineering (try-step crew-quarters "east"))
;; == Engineering ==
;; You see a whiteboard with plans for Springdroid v2.
;; Doors here lead:
;; - east
;; - south (arcade)
;; - west (crew-quarters)
;; Items here:
;; - hologram


(def corridor (try-step engineering "east"))
;; == Corridor ==
;; The metal walls and the metal floor are slightly different colors. Or are they?
;; Doors here lead:
;; - west
;; Items here:
;; - klein bottle

(def arcade (try-step engineering "south"))
;; == Arcade ==
;; None of the cabinets seem to have power.
;; Doors here lead:
;; - north
;; - west
;; Items here:
;; - molten lava

(def security-checkpoint (try-step arcade "west"))
;; == Security Checkpoint ==
;; In the next room, a pressure-sensitive floor will verify your identity.
;; Doors here lead:
;; - north
;; - east

(def stables (try-step crew-quarters "west"))
;; == Stables ==
;; Reindeer-sized. They're all empty.
;; Doors here lead:
;; - east (crew-quarters)

(def passages (try-step gift-wrapping-center "west"))
;; == Passages ==
;; They're a little twisty and starting to look all alike.
;; Doors here lead:
;; - east (gift-wrapping-center)
;; - south (hot chocolate fountain)
;; - west ( storage)
;; Items here:
;; - photons

(def storage (try-step passages "west"))
;; == Storage ==
;; The boxes just contain more boxes.  Recursively.
;; Doors here lead:
;; - east (passages)
;; Items here:
;; - tambourine

(def hot-chocolate-fountain (try-step passages "south"))
;; == Hot Chocolate Fountain ==
;; Somehow, it's still working.
;; Doors here lead:
;; - north (passages)

(def security-with-all-items
  (try-step hull-breach
            "south"
            "east" "take whirled peas" "west"
            "north" ;; hull breach

            "north" ;; holodeck
            "east" "take ornament"

            "north" ;; kitchen
            "north" "take dark matter" "south"
            "south"

            "west" ;; holodeck

            "west" "west" "take candy cane" "west"
            "west" "take tambourine"
            "east" "east" "east" ;; science lab

            "north" "take astrolabe"
            "east" "take hologram"
            "east" "take klein bottle"
            "west" "south" "west"))

(try-step security-with-all-items "inv")

(def inv-items
  ["ornament"
   "klein bottle"
   "dark matter"
   "candy cane"
   "hologram"
   "astrolabe"
   "whirled peas"
   "tambourine"])

(defn drop-others [l]
  (->>
   (set l)
   (set/difference (set inv-items))
   (map (fn [item] (str "drop " item)))
   (into [])))

(defn solution []
  (loop [c (cmb/subsets inv-items)]
    (when-not (empty? c)
      (let [dropped
            (apply step security-with-all-items
                   (conj (drop-others (first c))))

            [_ _ result]
            (step dropped "north")

            txt (apply str (map char result))]

        (if-not (string/includes? txt "Alert!")
          (println txt))
        (recur (rest c))))))
