(ns rdp.213-intermediate (:gen-class))


(def ^:private keyboard-vec
  [["Q"  "W" "E" "R"  "T"  "Y"  "U"  "I"  "O" "P"]
   ["A"  "S" "D" "F"  "G"  "H"  "J"  "K"  "L" " "]
   ["LS" "Z" "X" "C"  "V"  "B"  "N"  "M"  " " "RS"]
   [" "  " " " " "S1" "S2" "S3" "S4" "S5" " " " "]])

(def ^:private keyboard
  (into {} 
        (mapcat 
         (fn [row-idx keys-row]
           (keep-indexed 
            (fn [col-idx key]
              (when-not (= " " key) 
                [key {:key key :row row-idx :col col-idx}]))
            keys-row))
         (range (count keyboard-vec))
         keyboard-vec)))

(def ^:private space-keys 
  (vec (map keyboard ["S1" "S2" "S3" "S4" "S5"])))

(def ^:private shift-keys
  (vec (map keyboard ["LS" "RS"])))

(defn- char->keyboard-key
  "Returns a [] of keyboards keys to consider. 
  Can contain either one or two items. If two items are provided
  it means, the first item is a modifier and the second is the actual 
  key. The first item is always a sequence of keys. 
  \" \" -> [[S1 S2 S3 S4 S5]]
  capital T -> [[LS RS] T]
  small case t -> [[T]]"
  [c]
  (let [char-str (str c)
        upper-cased (clojure.string/upper-case c)]
    (cond
      (= char-str " ") [space-keys]
      (false? (contains? keyboard 
                         upper-cased)) (throw (Exception. 
                                               (str "Unsupported character: " c)))
      (= char-str upper-cased) [shift-keys (keyboard upper-cased)]
      :else  [[(keyboard upper-cased)]])))

(defn- calc-effort 
  [{row1 :row col1 :col}
   {row2 :row col2 :col}]
  (+ (Math/abs (- row1 row2))
     (Math/abs (- col1 col2))))

(defn- calc-effort-for-hand-side
  [hand key side]
  (calc-effort key (keyboard (hand side) key)))

(defn- get-hand-moves
  [hand key]
  (map #(hash-map 
         :effort (calc-effort-for-hand-side hand key %)
         :key-id (:key key)
         :which %)
       [:left :right]))

(defn- merge-modifier-next-key-moves
  "Merges :left modifier-move with :right next-key-move & vice versa"
  [modifier-moves next-key-moves]
  (mapcat (fn [modifier-move] 
            (map 
             (fn [next-key-move] 
               ;; (println "mod-- " modifier-move)
               ;; (println "nkm-- " next-key-move)
               (hash-map 
                :modifier-move modifier-move
                :next-key-move next-key-move)) 
             (remove #(= (:which modifier-move) (:which %)) 
                     next-key-moves))) 
          modifier-moves))

(defn- get-next-state
  "Returns hashmap containing the new hand positions and 
  the movement information for printing purposes"
  [prev-hand move-details]
  {:hand (assoc prev-hand 
                (:which move-details)
                (:key-id move-details))
   :movement {:which (:which move-details)
              :from (prev-hand (:which move-details))
              :effort (:effort move-details)}})

(defn- get-next-states
  "The 3 arity function handles the typing of an upper case char.
  The 2 arity function handles a lower case or a space char.

  For upper case chars, we select the lowest effort combination of
  moving to a Shift key and the actual char key. A combination will have
  a :left move for Shift key and a :right move for the char key. Or vice versa."
  ([hand next-keys]
   (let [moves (mapcat (partial get-hand-moves hand) next-keys)
         best-move (->> moves
                        (sort-by :effort)
                        (first))] 
     [(get-next-state hand best-move)]))
  ([hand modifier-keys next-key]
   (let [modifier-moves (mapcat (partial get-hand-moves hand) 
                                modifier-keys)
         next-key-moves (get-hand-moves hand next-key) 
         combined-moves (merge-modifier-next-key-moves modifier-moves
                                                       next-key-moves)
         best-move (->> combined-moves
                        (sort-by #(+ (get-in % [:modifier-move :effort])
                                     (get-in % [:next-key-move :effort])))
                        (first))
         state1 (get-next-state 
                 hand (:modifier-move best-move))
         state2 (get-next-state 
                 (:hand state1) (:next-key-move best-move))]
     [state1 state2])))

(defn- key->text [key]
  (cond
    (nil? key) ""
    (some #{key} (map :key shift-keys)) "Shift"
    (some #{key} (map :key space-keys)) "Space"
    :else (clojure.string/upper-case key)))

(defn- movement-message 
  [hand {:keys [effort from] side :which}]
  (let [key-text (key->text (get hand side))
        from-key-text (key->text from)
        hand-side-name (name side)] 
    (cond
      ;; initial state
      (nil? from) (format "%s: Use %s hand" key-text hand-side-name)
      ;; no movement needed
      (= key-text from-key-text) (format "%s: Use %s hand again" key-text hand-side-name)
      :else (format "%s: Move %s hand from %s (effort: %d)" 
                    key-text hand-side-name 
                    from-key-text effort))))

(defn- process-sentence [sentence]
  (loop [sentence sentence
         hand {:left nil :right nil}
         results []]
    (if (empty? sentence)
      results
      (let [char (first sentence)
            keyboard-keys (char->keyboard-key char)
            temp-results (apply get-next-states hand keyboard-keys)
            results (concat results temp-results)] 
        ;; (println "TMP --" temp-results)
        ;; (println "RSLT --" results)
        ;; (println "LST --" (last results))
        (recur (rest sentence) (:hand (last results)) results)))))

(defn- print-summary
  [sentence results]
  ;; (println results)
  (let [total-effort (reduce #(+ %1 (get-in %2 [:movement :effort])) 
                             0
                             results)] 
    ;;(println total-effort)
    (println sentence) 
    (doseq [result results]
      (println (movement-message (:hand result) (:movement result))))
    (println "Total effort: " total-effort)
    (println "")
    (println "")))

;;  (let [sentence "The quick brown Fox"]
;;    (->> sentence
;;         (process-sentence)
;;         (print-summary sentence)))
;;

(defn -main
  []
  (doseq [sentence ["The quick brown Fox"
                    "hello world"
                    "qpalzm woskxn"
                    "Hello there DailyProgrammers"
                    "QPgizm QFpRKbi Qycn"]]
    (->> sentence
         (process-sentence)
         (print-summary sentence))))
