(ns rdp.happy-numbers (:gen-class))

(defn- digits
  [num]
  (loop [num num
         digits []]
    (let [quo (int (/ num 10))
          rem (mod num 10)
          digits (conj digits rem)]
      (if (zero? quo)
        (reverse digits)
        (recur quo digits)))))

(defn- square [x] (* x x))

(defn- sum-of-squares-of-digits [num] 
  (->> num
       digits
       (map square)
       (reduce + 0)))

(defn- happy-number?
  [num]
  (loop [num num
         generated-nums #{}]
    (let [result (sum-of-squares-of-digits num)]
      ;(println result) 
      (cond
        (= 1 result) true
        (contains? generated-nums result) false
        :else (recur result (conj generated-nums result))))))

(defn happy-numbers
  ([] (happy-numbers 1))
  ([i] 
   (let [n (first (filter happy-number? (iterate inc i)))]
     (cons n (lazy-seq (happy-numbers (inc n)))))))

