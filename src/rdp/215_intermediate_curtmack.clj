(ns rdp.215-intermediate-curtmack
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; Original version - https://www.reddit.com/r/dailyprogrammer/comments/36m83a/20150520_challenge_215_intermediate_validating/crfk7ho

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defn all-zeroes-and-ones [^long n]
  (if (zero? n)
    [[]]
    (concat (map #(conj % 0) (lazy-seq (all-zeroes-and-ones (dec n))))
            (map #(conj % 1) (lazy-seq (all-zeroes-and-ones (dec n)))))))

(defn swap-vector [v idx1 idx2]
  (assoc v idx1 (v idx2) idx2 (v idx1)))

(defn sorted-order? [v]
  (apply <= v))

(defn sort-vals
  [input ^long idx1 ^long idx2]
  (if (> ^long (nth input idx1) ^long (nth input idx2))
    (swap-vector input idx1 idx2)
    input))

(defn do-sort-network [comparators input]
  (loop [input input
         remn comparators]
    (if-let [[^long idx1 ^long idx2] (first remn)]
      ;; the let form is not needed as the input is guaranteed to be idx1 < idx2
      (let [[small large] (if (<= idx1 idx2) [idx1 idx2] [idx2 idx1])]
        (recur (sort-vals input idx1 idx2) (rest remn)))
      input)))

;; (defn do-sort-network [comparators input]
;;   (reduce 
;;    (fn [input [^long idx1 ^long idx2]]
;;      (if (> ^long (nth input idx1) ^long (nth input idx2))
;;        (swap-vector input idx1 idx2)
;;        input))
;;    input
;;    comparators))

(defn test-sort-network [num-wires comparators]
  (every? true?
          (for [input (all-zeroes-and-ones num-wires)]
            (sorted-order? 
             (do-sort-network comparators input)))))

;; (defn test-sort-network [num-wires comparators]
;;   (let [sorting-network (partial do-sort-network comparators)]
;;     (every? sorted-order?
;;             (map sorting-network (all-zeroes-and-ones num-wires)))))

(defn parse-long [s] (Long/parseLong s))

(def file-name "215_intermediate_challenge_2.txt")

(defn parse-input
  []
  (with-open [rdr (io/reader (io/resource file-name))]
    (let [lines (line-seq rdr)
          desc-line (first lines)
          [num-wires num-comparators] (map parse-long (string/split desc-line #"\s+"))
          comparators (doall (take num-comparators 
                                   (map #(vec (map parse-long 
                                                   (string/split % #"\s+"))) 
                                        (next lines))))]
      {:num-wires num-wires
       :comparators comparators})))

(defn -main []
  (time 
   (let [input (parse-input)]
     (println (if (test-sort-network (:num-wires input)
                                     (:comparators input))
                "Valid network"
                "Invalid network")))))
