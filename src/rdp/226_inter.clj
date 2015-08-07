(ns rdp.226-inter)

;; https://www.reddit.com/r/dailyprogrammer/comments/3fva66/20150805_challenge_226_intermediate_connect_four/

(def sticks [:a :b :c :d :e :f :g])
(def sticks-set (set sticks))
(def max-slots-per-stick 6)

;; There are seven sticks. Each stick has 6 slots. Players place discs on a stick. The disc occupies the next available slot. The winner is the player who places 4 discs that can be connected in a straight line 
;;  - vertically (on 4 consecutive slots on same stick), 
;;  - horizontally (4 on consecutive sticks, but at same slot) or
;;  - diagonally (any combination of 4 consecutive slots and 4 consecutive sticks).


(defn- previous-slots 
  "Returns the 3 slots prior to `slot`. May include out of bound slots."
  [slot]
  (take 3 (drop 1 (iterate dec slot))))

(defn- next-slots 
  "Returns the 3 slots after `slot`. May include out of bound slots."
  [slot]
  (take 3 (drop 1 (iterate inc slot))))

(defn- previous-sticks 
  "Returns the 3 sequential sticks before to `stick`. The order is the reverse of the order in `sticks`. If `stick` is among the first 3 sticks, then returns empty vector.
  (previous-sticks :d) => [:c :b :a]
  (previous-sticks :c) => []"
  [stick]
  (if (#{:d :e :f :g} stick) 
    (->> sticks
         (take-while #(not (= %1 stick)))
         (take-last 3)
         (reverse)) 
    []))

(defn- next-sticks 
  "Returns the 3 sticks appearing after `stick`. Returns in the same order as in `sticks`.
  If `stick` is among the last 3 sticks, then returns empty vector.
  (next-sticks :d) => [:e :f :g]"
  [stick]
  (if (#{:a :b :c :d} stick) 
    (->> sticks 
         (drop-while #(not (= %1 stick)))
         (drop 1) 
         (take 3))
    []))

(defn- vertically-connected-discs 
  "A disc is represented by a pair of stick and slot. 
  Returns all possible sequences of 4 discs having same stick and sequential slots.
  May contain invalid discs."
  [[stick slot]]
  (->> [next-slots previous-slots] 
       (map #(%1 slot))
       (map #(map vector (repeat stick) %1))
       (remove empty?)
       (map #(conj %1 [stick slot]))))

(defn- horizontally-connected-discs 
  "A disc is represented by a pair of stick and slot. 
  Returns all possible sequences of 4 discs having same slot and resting on consecutive sticks.
  May contain invalid discs."
  [[stick slot]]
  (let [next-sticks (next-sticks stick) 
        prev-sticks (previous-sticks stick)] 
    (->> [next-sticks prev-sticks]
         (map #(map vector %1 (repeat slot)))
         (remove empty?)
         (map #(conj %1 [stick slot])))))

(defn- diagonally-connected-discs 
  "A disc is represented by a pair of stick and slot. 
  Returns all possible sequences of 4 discs that can be considered as diagonally connected.
  May contain invalid discs."
  [[stick slot]]
  (let [prev-sticks (previous-sticks stick)
        next-sticks (next-sticks stick)
        previous-indices (previous-slots slot)
        next-indices (next-slots slot)]
    (->>
     [(map vector prev-sticks previous-indices)
      (map vector prev-sticks next-indices)
      (map vector next-sticks previous-indices)
      (map vector next-sticks next-indices)]
     (remove empty?)
     (map #(conj %1 [stick slot])))))

(defn- possibly-connected-discs 
  "Returns all vertically, horizontally and diagonally connected discs for `disc`. Might include discs that haven't been placed yet."
  [[stick slot :as disc]]
  {:pre [(sticks-set stick) 
         (< -1 slot max-slots-per-stick)]}
  (concat (vertically-connected-discs disc) 
          (horizontally-connected-discs disc) 
          (diagonally-connected-discs disc)))

(defn connected-four? 
  "Returns first 4 connected discs. `discs` is a set of all discs that have been placed by a given player. For every `disc` in `discs`, it generates sequences of 4 connected discs. It selects the first sequence for which all its discs have already been placed, ie they exist in the set `discs`."
  [discs]
  {:pre [(set? discs)]}
  (->> discs
       (mapcat possibly-connected-discs) 
       (filter #(every? discs %1))
       (first)))


(defn- winner? 
  "Checks if player has satisfied winning condition. If yes, updates game state with winning stats and marks game as over. Returns game state."
  [state player]
  (when-let [discs (connected-four? (get-in state [player :discs]))]
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
        (update-in [:next-stick-index stick] (fnil inc 0)))))

(defn- process-players-move 
  "Processes move for both players and determines winner. Returns game state."
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

(def initial-game-state 
  {:p1 {:name "X" :discs #{}}
   :p2 {:name "O" :discs #{}}
   :moves []
   :game-over false
   :winner nil
   :next-stick-index {:a 0 :b 0 :c 0 :d 0 :e 0 :f 0 :g 0}})

(defn game-loop [input-str] 
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

(def input-1 "C  d
D  d
D  b
C  f
C  c
B  a
A  d
G  e
E  g")

(def input-2 "D  d
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
