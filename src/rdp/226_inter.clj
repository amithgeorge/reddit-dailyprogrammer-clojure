(ns rdp.226-inter)

;; https://www.reddit.com/r/dailyprogrammer/comments/3fva66/20150805_challenge_226_intermediate_connect_four/

(def ^:private sticks [:a :b :c :d :e :f :g])
(def ^:private sticks-set (set sticks))
(def ^:private max-slots-per-stick 6)

;;  There are seven sticks. Each stick has 6 slots. 
;;  Players place discs on a stick.
;;  The disc occupies the next available slot. The winner is the player who 
;;  places 4 discs that can be connected in a straight line 
;;  - vertically (on 4 consecutive slots on same stick), 
;;  - horizontally (4 on consecutive sticks, but at same slot) or
;;  - diagonally (any combination of 4 consecutive slots and 4 consecutive sticks)


(defn- consecutive-4-slots 
  [slot]
  (->> (partition 4 1 (range 0 max-slots-per-stick))
       (filter #(some #{slot} %1))))

(defn- consecutive-4-sticks
  [stick]
  (->> (partition 4 1 sticks)
       (filter #(some #{stick} %1))))


(defn- connected-horizontally 
  "4 consecutive sticks, same slot"
  [[_ slot] seq-sticks]
  (->> seq-sticks
       (map #(map (fn [stick] [stick slot]) %1))
       ;; (map #(do (println "h - " %1) %1))
;;
       ))

(defn- connected-vertically
  "Same stick, 4 consecutive slots"
  [[stick _] seq-slots]
  (->> seq-slots
       (map #(map (fn [slot] [stick slot]) %1))
       ;; (map #(do (println "v - " %1) %1))
;;        
       ))

(defn- connected-diagonally
  "Interleave the consecutive slots in `seq-slots` with 
  its reverse. This ensures that we get both ascending
  and descending diagonal lines. 
  Join all consecutive sticks with consecutive slots
  that gives all diagonal lines with atleast one disc
  either in the right slot or right stick.
  Filter that to get lines with the disc in both the
  right stick and right slot."  
  [disc seq-sticks seq-slots]
  (let [seq-slots (interleave seq-slots (map reverse seq-slots))]
    (->> (mapcat (fn [sticks-seq] 
                   (map #(map vector sticks-seq %1) seq-slots))
                 seq-sticks)
         (filter #(some #{disc} %1))
         (map #(do (println "d - " %1) %1))
;;           
         )))

(defn- all-connected-discs 
  [[stick slot :as disc]]
  (let [seq-slots (consecutive-4-slots slot)
        seq-sticks (consecutive-4-sticks stick)]
    (concat (connected-horizontally disc seq-sticks) 
            (connected-vertically disc seq-slots)
            (connected-diagonally disc seq-sticks seq-slots))))

(defn- is-disc?
  [[stick slot :as disc]]
  (and (sticks-set stick)
       (< -1 slot max-slots-per-stick)))

(defn- connected-four? 
  "Returns first 4 connected discs. `discs` is a set of all 
  discs that have been placed by a given player. `disc` is 
  the latest disc placed. All connected discs sequences 
  containing `disc` is generated. It selects the first sequence
  for which all discs have already been placed, ie exists 
  in `discs`."
  ([discs disc]
   {:pre [(set? discs) (is-disc? disc)]}
   (when (<= 4 (count discs))
     (->> disc
          (all-connected-discs)
          (filter #(every? discs %1))
          (first)))))


(defn- winner? 
  "Checks if player has satisfied winning condition. If yes,
  updates game state with winning stats and marks game as
  over. Returns game state."
  [state player]
  (when-let [discs (connected-four? (get-in state [player :discs]) 
                                    (get-in state [player :latest-disc]))]
    (assoc state 
           :winner {:moves-count (count (:moves state))
                    :line (sort discs)
                    :player (get-in state [player :name])}
           :game-over true)))

(defn- process-player-move 
  "Updates game state with player placed disk."
  [player stick state]
  (let [disc [stick (get-in state [:next-stick-index stick] 0)]]
    (-> state
        (update-in [player :discs] conj disc)
        (assoc-in [player :latest-disc] disc)
        (update-in [:next-stick-index stick] (fnil inc 0)))))

(defn- process-players-move 
  "Processes move for both players and determines winner. 
  Returns game state."
  [state move]
  (let [old-state state
        state (->> old-state
                   (process-player-move :p1 (:p1 move))
                   (process-player-move :p2 (:p2 move))
                   (#(update-in %1 [:moves] conj move)))]
    (or (winner? state :p1) (winner? state :p2) state)))

(defn- disc->player-disc-str
  "args [\"X\", [:d 3] => D3. 
  args [\"O\", [:d 3] => d3"
  [player [stick slot]]
  (condp = player
    "X" (str (clojure.string/upper-case (name stick)) (inc slot))
    "O" (str (clojure.string/lower-case (name stick)) (inc slot))))

(defn- stick-char->stick 
  " D => :d
    d => :d"
  [stick]
  (let [stick (keyword (clojure.string/lower-case stick))]
    (if (sticks-set stick)
      stick
      (throw (Exception. (str "Invalid stick - " stick))))))

(defn- read-players-move 
  []
  (when-let [line (read-line)]
    (let [line (clojure.string/trim line)
          parts (clojure.string/split line #"  ")]
      (prn line)
      {:p1 (stick-char->stick (parts 0))
       :p2 (stick-char->stick (parts 1))})))

(def ^:private initial-game-state 
  {:p1 {:name "X" :discs #{}}
   :p2 {:name "O" :discs #{}}
   :moves []
   :game-over false
   :winner nil
   :next-stick-index {:a 0 :b 0 :c 0 :d 0 :e 0 :f 0 :g 0}})

(defn- game-loop [input-str] 
  (with-in-str input-str
    (loop [state initial-game-state]
      (let [move (read-players-move)]
        (if (nil? move)
          (throw (Exception. "No more moves left. Game undecided."))
          (let [next-state (process-players-move state move)]
            (if (:game-over next-state)
              next-state
              (recur next-state))))))))

(defn- print-result 
  [{:keys [moves-count line player]} moves] 
  ;; (println "Moves played till game over - ")
  ;; (prn moves)
  (let [discs (map (partial disc->player-disc-str player) line)]
    (println (format "%s won at move %d (with %s)"
                     player 
                     moves-count 
                     (clojure.string/join " " discs)))))

(def ^:private input-1 "C  d
D  d
D  b
C  f
C  c
B  a
A  d
G  e
E  g")

(def ^:private input-2 "D  d
D  c    
C  c    
C  c
G  f
F  d
F  f
D  f
A  a
E  b
E  e
B  g
G  g
B  a")

(defn play []
  (let [result (game-loop input-1)]
    (print-result (:winner result) (:moves result)))
  (let [result (game-loop input-2)]
    (print-result (:winner result) (:moves result))))






;; (def pattern-length 4)
;; (def disc-indices (take max-slots-per-stick (range)))
;; (def sticks (into {} 
;;                   (map #(vector %1 (vec (repeat max-slots-per-stick nil))) 
;;                        stick-tags)))

;; (def player-one-token :x)
;; (def player-two-token :o)
;;
;; (defn- check-sticks [sticks pattern indices tags]
;;   (->> indices
;;        (map #(map vector tags %1))
;;        (map (fn [keys] (map #(vector %1 (get-in sticks %1)) keys)))
;;        (map #(into {} %1))
;;        ;; ((fn [arg] (do (prn arg) arg)))
;;        (filter #(= pattern (vals %1)))
;;        (first)
;;        (keys)))

;; (defn- c4-vertical 
;;   "Checks whether any of the sticks contains 4 consecutive token values. Returns sequence of pairs of stick-tag and disc index"
;;   [token sticks]
;;   (let [indices (partition pattern-length 1 disc-indices)
;;         pattern (repeat pattern-length token)
;;         result (->> stick-tags
;;                     (map #(repeat pattern-length %1))
;;                     (keep #(check-sticks sticks pattern indices %1))
;;                     (first))]
;;     result))

;; (defn- c4-diagonal  
;;   "Checks for the presence of token in sequentially decreasing (or increasing) indices in consecutive sticks. Returns sequence of pairs of stick-tag and disc index"
;;   [token sticks]  
;;   (let [indices (partition pattern-length 1 disc-indices)
;;         indices (into indices (partition pattern-length 1 (reverse disc-indices)))
;;         pattern (repeat pattern-length token)
;;         result (->> (partition pattern-length 1 stick-tags)
;;                     (keep #(check-sticks sticks pattern indices %1))
;;                     (first))]
;;     result))

;; (defn- c4-horizontal 
;;   "Checks whether a token exists in 4 consecutive sticks at the same disc index"
;;   [token sticks]
;;   (let [indices (map #(repeat pattern-length %1) disc-indices)
;;         pattern (repeat pattern-length token)
;;         result (->> (partition pattern-length 1 stick-tags)
;;                     (keep #(check-sticks sticks pattern indices %1))
;;                     (first))]
;;     result))

;; (defn- winner? [token sticks]
;;   (->> [c4-diagonal c4-vertical c4-horizontal]
;;        (keep #(%1 token sticks))
;;        (first)))
