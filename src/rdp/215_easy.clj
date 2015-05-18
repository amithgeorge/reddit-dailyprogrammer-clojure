(ns rdp.215-easy)

(defn parse-input
  []
  (let [base (Long/parseLong (clojure.string/trim (read-line)))
        n (Long/parseLong (clojure.string/trim (read-line)))]
    [n base]))

(defn digits
  [x]
  (if (zero? x)
    [0] 
    (loop [x x
           digits (list)]
      (if (zero? x)
        (into [] digits)
        (recur (quot x 10) (conj digits (mod x 10)))))))

(defn sum-of-digits
  ([digits] (sum-of-digits identity digits))
  ([tx-fn digits]
   (->> digits
        (map tx-fn)
        (reduce + 0))))

(defn sad-cycle
  [n base]
  (let [sum-of-digits-fn (partial sum-of-digits #(long (Math/pow %1 base)))] 
    (loop [n n
           sums []]
      (let [sum (sum-of-digits-fn (digits n))] 
        ;; (prn n sum)
        (if (some #{sum} sums)
          (drop-while #(not= sum %1) sums)
          (recur sum (conj sums sum)))))))


(def inputs 
  ["5
   117649"
   "6
   2"
   "7
   7"
   "3
   14"
   "11
   2"])

(defn process-input
  [input-str]
  (with-in-str input-str
    (println (clojure.string/join 
              ", " 
              (apply sad-cycle (parse-input))))))

(defn -main 
  []
  (map process-input inputs))
