(ns rdp.214-intermediate-arr
  (:require [clojure.java.io :as io]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defn- read-line-nums
  ([] (read-line-nums (read-line) #(Long/parseLong %1)))
  ([input-line parse-fn]
   (if-let [line input-line]
     (->> line
          (#(clojure.string/split %1 #" "))
          (map parse-fn)))))

(defrecord Paper [^long color 
                  ^long x1  ^long y1
                  ^long x2 ^long y2])

(defrecord Canvas [^long width ^long height])

(defn- make-paper
  ([^long w ^long h] (make-paper [0 0 0 w h]))
  ([^longs [color ^long x ^long y ^long w ^long h]]
   (Paper. color x y (+ x w -1) (+ y h -1))))

(defn- parse-inputs
  []
  (let [canvas (apply ->Canvas (read-line-nums))]
    (loop [papers (list (make-paper (:width canvas) (:height canvas)))
           colors #{0}]
      (if-let [values (read-line-nums)]
        (recur (conj papers (make-paper values))
               (conj colors (first values)))
        {:papers (into [] papers)
         :colors colors
         :canvas canvas}))))

(defn- read-input-file 
  [file-name]
  (do
    (println "parsing file into records")
    (time (with-open 
           [rdr (io/reader (io/resource file-name))]
            (binding [*in* rdr]
              (parse-inputs))))))

(def ^:private input-files
  ["100rects100x100.in"
   "100rects10Kx10K.in"
   "100rects3Kx3K.in"])

(defn- covered?
  [^long canvas-x ^long canvas-y ^Paper paper]
  (let [x1 (.x1 paper)
        x2 (.x2 paper)
        y1 (.y1 paper)
        y2 (.y2 paper)]
    (and (<= x1 canvas-x)
         (<= canvas-x x2)
         (<= y1 canvas-y)
         (<= canvas-y y2))))

(defn- visible-color
  [^long x ^long y papers]
  (some (fn [^Paper paper] 
          (when (covered? x y paper)
            (.color paper)))
        papers))

;; (defn- visible-color
;;   [^long x ^long y papers]
;;   (loop [papers papers] 
;;     (if (empty? papers)
;;       nil
;;       (let [^Paper paper (first papers)]
;;         (if (covered? x y paper)
;;           (.color paper)
;;           (recur (rest papers)))))))

(defn- visible-color-arr
  [^long x ^long y ^"[Lrdp.214_intermediate_arr.Paper;" papers]
  (let [len (alength papers)] 
    (loop [i 0]
      (if (< i len)
        (let [p ^Paper (aget papers i)] 
          (if (covered? x y p)
            (.color p)
            (recur (inc i))))  
        nil))))

(defn- visible-color-frequencies
  [{:keys [^Canvas canvas papers]}]
  (let [height (.height canvas)
        width (.width canvas)] 
    (loop [y 0
           acc (transient {})]
      (if (< y height)
        (let [acc 
              (loop [x 0 
                     acc acc]
                (if (< x width)
                  (recur (inc x)
                         (if-let [color (visible-color x y papers)]
                           (assoc! acc color (inc ^long (get acc color 0)))
                           acc))
                  acc))]
          (recur (inc y) acc))
        (persistent! acc)))))

(defn- visible-color-frequencies-arr
  [{:keys [colors ^Canvas canvas papers]}]
  (let [colorCounts (long-array (count colors))
        height (.height canvas)
        width (.width canvas)
        papers (into-array Paper papers)] 
    (loop [y 0]
      (if (< y height)
        (do 
          (loop [x 0]
            (if (< x width)
              (if-let [color (visible-color-arr x y papers)]
                (do
                  (aset colorCounts color (inc (aget colorCounts color)))
                  (recur (inc x))))))
          (recur (inc y)))))
    (zipmap (range) colorCounts)))

(defn- solve
  [input-file]
  (let [input (read-input-file input-file)
        color-map (visible-color-frequencies input)
        sorted (sort-by key color-map)]
    (doseq [line sorted]
      (println (key line) (val line)))))

(defn- solve-arr
  [input-file]
  (let [input (read-input-file input-file)
        color-map (visible-color-frequencies-arr input)
        sorted (sort-by key (remove #(zero? ^long (val %1)) color-map))]
    (doseq [line sorted]
      (println (key line) (val line)))))

(defn -main 
  ([] (-main "0" "false"))
  ([index use-arrays] 
   (time 
    (if-not (Boolean/parseBoolean use-arrays) 
      (solve (input-files (Integer/parseInt index)))
      (solve-arr (input-files (Integer/parseInt index)))))))
