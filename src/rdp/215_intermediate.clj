(ns rdp.215-intermediate
  (:require [clojure.java.io :as io])
  (:gen-class))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defn- read-line-longs 
  []
  (if-let [line (read-line)] 
    (-> line
        (clojure.string/split #" ")
        (#(map (fn [s] (Long/parseLong s)) %1)))))

(defn- parse-inputs
  []
  ;; (println "parsing inputs")
  (let [[^long wires-count ^long comparators-count] (read-line-longs)] 
    (loop [endpoints []
           i 0]
      (if (< i comparators-count)
        (recur (conj endpoints (vec (read-line-longs)))
               (inc i))
        {:wires-count wires-count
         :comparator-endpoints endpoints}))))

(defn- binary-sequence
  [^long n]
  (take (Math/pow 2 n) 
        (->> (iterate inc 0)
             (map #(Long/toBinaryString %1))
             (map #(if (< (count %1) n)
                     (apply str (concat (take (- n (count %1)) (repeat \0)) %1))
                     %1))
             (map #(map (fn [^Character c] (Character/digit c 10)) %1))
             (map vec))))


(defn- wire-values-swapper
  [inputs wire-a wire-b]
  (let [^long a (nth inputs wire-a)
        ^long b (nth inputs wire-b)]
    (if (< a b)
      inputs
      (assoc! inputs 
              wire-a b 
              wire-b a))))

(defn- wire-values-swapper-arr 
  [^"[J" inputs ^long wire-a ^long wire-b]
  (let [a (aget inputs wire-a)
        b (aget inputs wire-b)]
    (if-not (< a b)
      (do 
        (aset inputs wire-a b)
        (aset inputs wire-b a)))))

(defn- make-sorting-network
  [comparator-endpoints]
  (fn [nums]
    (persistent!
     (reduce (fn [^longs nums [^long wire-a ^long wire-b]] 
               (wire-values-swapper nums wire-a wire-b))
             (transient nums)
             comparator-endpoints))))

(defn- make-sorting-network-arr
  [comparator-endpoints]
  (let [endpoints-arr 
        (long-array (mapcat identity comparator-endpoints))
        endpoints-length (count endpoints-arr)] 
    (fn [nums]
      (let [nums-arr (long-array nums)] 
        (loop [i 0] 
          (if (< i endpoints-length)
            (let [wire-a (aget endpoints-arr i)
                  wire-b (aget endpoints-arr (inc i))]
              (wire-values-swapper-arr nums-arr wire-a wire-b)
              (recur (+ 2 i)))
            (into [] nums-arr)))))))

(defn- test-sorting-network
  [sorting-network input]
  (apply <= (sorting-network input)))

(def input-file-names 
  ["215_intermediate_sample_1.txt"
   "215_intermediate_sample_2.txt"
   "215_intermediate_challenge_1.txt"
   "215_intermediate_challenge_2.txt"])

(defn- read-input-file
  [file-name]
  (with-open [rdr (io/reader (io/resource file-name))]
    (binding [*in* rdr]
      (parse-inputs))))


(defn- process-sorting-network-defn
  [maker-fn input]
  (let [sorting-network (maker-fn (:comparator-endpoints input))
        test-sorting-network (partial test-sorting-network sorting-network)
        valid? (every? test-sorting-network
                       (binary-sequence (:wires-count input)))]
    (if valid?
      "Valid network"
      "Invalid network")))

(defn -main
  [index use-array]
  (time 
   (let [input (read-input-file (input-file-names (Long/parseLong index)))
         maker-fn (if (Boolean/parseBoolean use-array)
                    make-sorting-network-arr 
                    make-sorting-network)
         result (process-sorting-network-defn maker-fn input)]
     (println result))))


;; (-main "3" "true")
;; Elapsed time: 483ms
